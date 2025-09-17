const net = @This();
const std = @import("std");
const ecs = @import("ecs.zig");
const zeng = @import("zeng.zig");
const rpc = @import("rpc.zig");
const main = @import("main.zig");

const FIONBIO: u32 = 0x8004667e;

pub const remote_message = struct {
    seq: usize,
    time_to_send: f64,
    payload: []u8,
    sender_socket: net.socket_t,
    target_address: net.sockaddr_socklen_t,
    resend_timer: f64,
    channel: zeng.commands.reliability_channel = .unreliable,
};
pub const resend_interval_sec = 1.0;

/// this holds the information given by recvfrom representing the address of the sender
pub const sockaddr_socklen_t = struct {
    sockaddr: sockaddr_t,
    socklen: socklen_t,
};

pub const sockaddr_t = std.os.windows.ws2_32.sockaddr;
pub const socklen_t = i32;
pub const socket_t = std.os.windows.ws2_32.SOCKET;
pub const Address = std.net.Address;

fn WINDOWS_set_socket_non_blocking(sock: socket_t) !void {
    const one: u32 = 1;
    const one_ptr = @as([*]const u8, @ptrCast(&one))[0..4];
    const err = try std.os.windows.WSAIoctl(sock, FIONBIO, one_ptr, ""[0..0], null, null); // windows' way of setting a socket to non blocking (disgusting)
    if (err != 0) unreachable;
}

pub fn assign_addr_to_sock(socket: socket_t, my_address: Address) !void {
    const err = std.os.windows.ws2_32.bind(socket, &my_address.any, @intCast(my_address.getOsSockLen()));
    if (err != 0) unreachable;
}

pub fn remote_event(commands: *zeng.commands, socket: net.socket_t, address: net.sockaddr_socklen_t, event: anytype, channel: zeng.commands.reliability_channel) void {
    const payload_array = commands.allocator.alloc(u8, @sizeOf(usize) + @sizeOf(u32) + @sizeOf(@TypeOf(event))) catch unreachable;
    var curr_byte: u32 = 0;
    const seq: usize = if (channel == .unreliable) 0 else commands.curr_seq;
    zeng.loader.serialize_to_bytes(seq, payload_array, &curr_byte);
    zeng.loader.serialize_to_bytes(comptime zeng.GET_MSG_CODE(@TypeOf(event)), payload_array, &curr_byte);
    zeng.loader.serialize_to_bytes(event, payload_array, &curr_byte);

    const msg = remote_message{ .seq = seq, .resend_timer = net.resend_interval_sec, .payload = commands.allocator.realloc(payload_array, curr_byte) catch unreachable, .sender_socket = socket, .target_address = address, .time_to_send = commands.get_sim_send_time(), .channel = channel };
    if (channel == .reliable) {
        commands.reliable_message_seqs.put(seq, msg) catch unreachable;
        commands.curr_seq += 1;
    }

    commands.remote_messages_send_queue[commands.remote_messages_send_queue_len] = msg;
    commands.remote_messages_send_queue_len += 1;
}
pub fn send_net_messages(commands: *zeng.commands, delta_time: f64) void {
    _ = delta_time;

    var curr: usize = 0;
    while (curr < commands.remote_messages_send_queue_len) {
        const rem_message = commands.remote_messages_send_queue[curr];
        if (rem_message.time_to_send <= commands.time) {
            if (commands.random.float(f32) < 0.7 or rem_message.channel == .reliable) {
                const err = std.os.windows.ws2_32.sendto(rem_message.sender_socket, rem_message.payload.ptr, @intCast(rem_message.payload.len), 0, &rem_message.target_address.sockaddr, rem_message.target_address.socklen);
                if (err == -1) {
                    const last_error = zeng.c.WSAGetLastError();
                    if (last_error == 10054) {
                        std.debug.print("Win32Error: WSAECONNRESET - connection reset (?)\n", .{});
                    } else if (last_error == 10022) {
                        std.debug.print("Win32Error: WSAEINVAL - invalid argument\n", .{});
                    } else if (last_error == zeng.c.WSAEWOULDBLOCK) {} else {
                        std.debug.print("Win32Error: {}\n", .{last_error});
                        unreachable;
                    }
                }
            }
            commands.allocator.free(rem_message.payload);

            commands.remote_messages_send_queue[curr] = commands.remote_messages_send_queue[commands.remote_messages_send_queue_len - 1];
            commands.remote_messages_send_queue_len -= 1;
        } else curr += 1;
    }
}
pub fn recieve_net_messages(socket: socket_t, res: *zeng.resources_t, commands: *zeng.commands, allocator: std.mem.Allocator) void {
    var sender_addr: sockaddr_t = undefined;
    var sender_addr_len: socklen_t = @sizeOf(sockaddr_t);

    var recv_read_buf: [4096]u8 = undefined;
    get_messages_loop: while (true) {
        const recv_result = zeng.c.recvfrom(@intFromPtr(socket), &recv_read_buf, recv_read_buf.len, 0, @ptrCast(&sender_addr), &sender_addr_len);
        // const recv_result = std.os.windows.ws2_32.recvfrom(socket, &recv_read_buf, recv_read_buf.len, 0, &sender_addr, &sender_addr_len);
        if (recv_result == -1) {
            const last_error = zeng.c.WSAGetLastError();
            if (last_error == 10054) {
                std.debug.print("Win32Error: WSAECONNRESET - connection reset (?)\n", .{});
            } else if (last_error == 10022) {
                std.debug.print("Win32Error: WSAEINVAL - invalid argument\n", .{});
                break :get_messages_loop;
            } else if (last_error == zeng.c.WSAEWOULDBLOCK) {
                break :get_messages_loop;
            } else {
                std.debug.print("Win32Error: {}\n", .{last_error});
                unreachable;
            }
        }

        var sequence_number: usize = undefined;
        @memcpy(@as([*]u8, @ptrCast(&sequence_number)), recv_read_buf[0..@sizeOf(usize)]);
        if (sequence_number > commands.last_recieved_seq) {
            commands.ack_bits = commands.ack_bits << 1;
            commands.ack_bits = commands.ack_bits & 1;
            commands.ack_bits = commands.ack_bits << @intCast(@min(31, sequence_number - commands.last_recieved_seq - 1));
            commands.last_recieved_seq = sequence_number;
        } else {
            // handle the acknowledgement of packets that are out of order
        }
        if (commands.reliable_message_seqs.get(sequence_number)) |_| {
            _ = commands.reliable_message_seqs.remove(sequence_number);
        }

        var event_code: u32 = undefined;
        @memcpy(@as([*]u8, @ptrCast(&event_code)), recv_read_buf[@sizeOf(usize) .. @sizeOf(usize) + @sizeOf(u32)]);

        inline for (rpc.REMOTE_MESSAGE_TYPES) |msg_type| {
            if (event_code == comptime zeng.GET_MSG_CODE(msg_type)) {
                var payload: msg_type = undefined;

                var curr: u32 = @sizeOf(usize) + @sizeOf(u32);
                zeng.loader.deserialize_from_bytes(msg_type, @as([*]u8, @ptrCast(&payload)), recv_read_buf[0..], &curr, 0);

                if (res.get(zeng.events(msg_type)).addresses != null) {
                    const address = net.sockaddr_socklen_t{ .sockaddr = sender_addr, .socklen = sender_addr_len };
                    res.get(zeng.events(msg_type)).send_with_address(allocator, payload, address);
                } else unreachable;
            }
        }
    }
}

pub fn do_setup(address_string: []const u8, port: u16, is_server: bool) !struct { socket_t, Address } {
    var wsa_data: zeng.c.WSADATA = undefined;
    _ = zeng.c.WSAStartup(zeng.c.MAKEWORD(2, 2), &wsa_data);

    if (is_server) {
        const my_socket: socket_t = std.os.windows.ws2_32.socket(std.os.windows.ws2_32.AF.INET, std.os.windows.ws2_32.SOCK.DGRAM, std.os.windows.ws2_32.IPPROTO.UDP);
        try WINDOWS_set_socket_non_blocking(my_socket);
        const my_address: Address = try std.net.Address.parseIp(address_string, port);
        try assign_addr_to_sock(my_socket, my_address);
        return .{ my_socket, my_address };
    } else {
        const my_socket: socket_t = std.os.windows.ws2_32.socket(std.os.windows.ws2_32.AF.INET, std.os.windows.ws2_32.SOCK.DGRAM, std.os.windows.ws2_32.IPPROTO.UDP);
        try WINDOWS_set_socket_non_blocking(my_socket);
        const server_address: Address = try std.net.Address.parseIp(address_string, port);
        return .{ my_socket, server_address };
    }
}
pub fn undo_setup(socket: socket_t) void {
    _ = std.os.windows.ws2_32.closesocket(socket);
    _ = zeng.c.WSACleanup();
}

// for reference
pub fn Server() !void {
    const my_address = try std.net.Address.parseIp("0.0.0.0", 55555);
    const my_socket = try std.os.socket(std.os.AF.INET, std.os.SOCK.DGRAM, std.os.IPPROTO.UDP);
    defer std.os.close(my_socket);
    try WINDOWS_set_socket_non_blocking(my_socket);

    try std.os.bind(my_socket, &my_address.any, my_address.getOsSockLen());
    var client_addr: std.os.sockaddr = undefined;
    var client_addr_len: std.os.socklen_t = @sizeOf(std.os.sockaddr);

    const message = "Hello, Client!";

    var buf: [1024]u8 = undefined;
    main_loop: while (true) {
        var recv_result: std.os.RecvFromError!usize = 1;
        get_messages_loop: while (true) {
            recv_result = std.os.recvfrom(my_socket, &buf, 0, &client_addr, &client_addr_len);
            if (recv_result) |len| {
                std.debug.print("recieved: '{s}'\n", .{buf[0..len]});
                _ = try std.os.sendto(my_socket, message, 0, &client_addr, client_addr_len);
            } else |err| {
                switch (err) {
                    std.os.RecvFromError.WouldBlock => {
                        break :get_messages_loop;
                    },
                    std.os.RecvFromError.ConnectionResetByPeer => {
                        std.debug.print("connection was reset by peer\n", .{});
                        break :main_loop;
                    },
                    else => return err,
                }
            }
        }
        std.time.sleep(std.time.ns_per_s);
    }
}
pub fn Client() !void {
    const server_address = try std.net.Address.parseIp("127.0.0.1", 55555);
    const my_socket = try std.os.socket(std.os.AF.INET, std.os.SOCK.DGRAM, std.os.IPPROTO.UDP);
    defer std.os.close(my_socket);
    try WINDOWS_set_socket_non_blocking(my_socket);

    const message = "Hello, Server!";
    _ = try std.os.sendto(my_socket, message, 0, &server_address.any, server_address.getOsSockLen()); // binds an ephemeral port to this socket and sends the info to the server

    var buf: [1024]u8 = undefined;
    main_loop: while (true) {
        var recv_result: std.os.RecvFromError!usize = 1;
        get_messages_loop: while (true) {
            recv_result = std.os.recv(my_socket, &buf, 0);
            if (recv_result) |len| {
                std.debug.print("recieved: '{s}'\n", .{buf[0..len]});
                _ = try std.os.sendto(my_socket, message, 0, &server_address.any, server_address.getOsSockLen());
            } else |err| {
                switch (err) {
                    std.os.RecvFromError.WouldBlock => {
                        break :get_messages_loop;
                    },
                    std.os.RecvFromError.ConnectionResetByPeer => {
                        std.debug.print("connection was reset by peer\n", .{});
                        break :main_loop;
                    },
                    else => return err,
                }
            }
        }
        std.time.sleep(std.time.ns_per_s);
    }
}
