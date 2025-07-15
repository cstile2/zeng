const net = @This();
const std = @import("std");
const ecs = @import("ecs.zig");
const zeng = @import("zeng.zig");
const rpc = @import("rpc.zig");
const main = @import("main.zig");

const FIONBIO: u32 = 0x8004667e;

pub const connection_id = u32; // represents the connection
pub const network_id = u32; // gloablly unique identifier of an object that is synced between computers
pub const remote_message = struct {
    seq: usize,
    time_to_send: f64,
    payload: []u8,
    sender_socket: net.socket_t,
    target_address: net.Address_t,
    resend_timer: f64,
};
pub const resend_interval_sec = 1.0;

/// this holds the information given by recvfrom representing the address of the sender
pub const Address_t = struct {
    sockaddr: std.os.sockaddr,
    socklen: std.os.socklen_t,
};

pub const socket_t = std.os.socket_t;
pub const address_t = std.net.Address;

fn WINDOWS_set_socket_non_blocking(sock: std.os.socket_t) !void {
    const one: u32 = 1;
    const one_ptr = @as([*]const u8, @ptrCast(&one))[0..4];
    _ = try std.os.windows.WSAIoctl(sock, FIONBIO, one_ptr, ""[0..0], null, null); // windows' way of setting a socket to non blocking (disgusting)
}

pub fn make_socket_and_address(address: anytype, port: u16, nonblocking: bool) !struct { socket_t, address_t } {
    const addr = try std.net.Address.parseIp(address, port);
    const sock = try std.os.socket(std.os.AF.INET, std.os.SOCK.DGRAM, std.os.IPPROTO.UDP);
    if (nonblocking)
        try WINDOWS_set_socket_non_blocking(sock);
    return .{ sock, addr };
}
pub fn assign_addr_to_sock(socket: socket_t, my_address: address_t) !void {
    try std.os.bind(socket, &my_address.any, my_address.getOsSockLen());
}

pub fn send_net_messages(commands: *zeng.commands, delta_time: f64) void {
    // var list = std.ArrayList(usize).init(allocator);
    // defer list.deinit();
    var it = commands.reliable_message_seqs.iterator();
    while (it.next()) |curr| {
        const _msg = curr.key_ptr.*;
        const msg = &commands.remote_messages[_msg];

        if (msg.resend_timer <= 0.0) {
            commands.remote_messages[commands.remote_messages_len] = msg.*;
            commands.remote_messages_len += 1;
            // list.append(i) catch unreachable;
            msg.resend_timer = resend_interval_sec;
        }
        msg.resend_timer -= delta_time;
    }

    var curr: usize = 0;
    while (curr < commands.remote_messages_len) {
        const rem_message = commands.remote_messages[curr];
        if (rem_message.time_to_send <= commands.time) {
            if (commands.random.float(f32) < 0.7) {
                _ = std.os.sendto(rem_message.sender_socket, rem_message.payload, 0, &rem_message.target_address.sockaddr, rem_message.target_address.socklen) catch unreachable;
            }
            commands.allocator.free(rem_message.payload);

            commands.remote_messages[curr] = commands.remote_messages[commands.remote_messages_len - 1];
            commands.remote_messages_len -= 1;
        } else curr += 1;
    }
}
pub fn recieve_net_messages(socket: std.os.socket_t, res: *zeng.resources_t, commands: *zeng.commands) void {
    var sender_addr: std.os.sockaddr = undefined;
    var sender_addr_len: std.os.socklen_t = @sizeOf(std.os.sockaddr);

    var recv_read_buf: [4096]u8 = undefined;
    get_messages_loop: while (true) {
        const recv_result = std.os.recvfrom(socket, &recv_read_buf, 0, &sender_addr, &sender_addr_len);

        if (recv_result) |_| {
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
            if (commands.reliable_messages[sequence_number].seq == sequence_number) {
                _ = commands.reliable_message_seqs.remove(sequence_number);
            }

            var event_code: u32 = undefined;
            @memcpy(@as([*]u8, @ptrCast(&event_code)), recv_read_buf[@sizeOf(usize) .. @sizeOf(usize) + @sizeOf(u32)]);

            inline for (rpc.REMOTE_MESSAGE_TYPES) |msg_type| {
                if (event_code == comptime zeng.GET_MSG_CODE(msg_type)) {
                    var payload: msg_type = undefined;

                    var curr: u32 = @sizeOf(usize) + @sizeOf(u32);
                    zeng.deserialize_from_bytes(msg_type, @as([*]u8, @ptrCast(&payload)), recv_read_buf[0..], &curr, 0);

                    if (res.get(main.events(msg_type)).addresses != null) {
                        const address = net.Address_t{ .sockaddr = sender_addr, .socklen = sender_addr_len };
                        res.get(main.events(msg_type)).send_with_address(payload, address);
                    } else {
                        // res.get(main.events(msg_type)).send(payload);
                        unreachable;
                    }
                }
            }
        } else |err| {
            if (err != error.WouldBlock) // no more messages to read
                std.debug.print("recv error: '{}'\n", .{err});
            break :get_messages_loop;
        }
    }
}

pub fn do_setup(address_string: []const u8, port: u16, is_server: bool) !struct { socket_t, address_t } {
    var socket: socket_t = undefined;
    var address: address_t = undefined;
    if (is_server) {
        socket, address = try zeng.net.make_socket_and_address(address_string, port, true);
        try assign_addr_to_sock(socket, address);
    } else {
        socket, address = try zeng.net.make_socket_and_address(address_string, port, true);
    }
    return .{ socket, address };
}
pub fn undo_setup(socket: socket_t) void {
    std.os.close(socket);
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

// socket - kind of represents a file to read/write to? or a channel with which to communicate through

// for clients using udp - sendto() - OS automatically assigns a port number to listen from
// server_address is used to send data there

// for servers - bind() is needed to associate the servers own address to a specific socket,
// to listen on the socket.
// server_address is used to bind, then it isn't needed
