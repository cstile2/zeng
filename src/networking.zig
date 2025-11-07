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
    target_address: net.peer_info_t,
    resend_timer: f64,
    channel: zeng.commands.reliability_channel = .unreliable,
};
pub const resend_interval_sec = 1.0;

/// this holds the information given by recvfrom representing the address of the sender
pub const peer_info_t = struct {
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

pub const packet_ack_tracker_t = struct {
    pub const packet_data_t = struct {
        acked: bool,
        timestamp: i64,
        rem_message: ?remote_message = null,
    };
    pub const header_t = struct {
        sequence_number: usize,
        most_recent_sequence_number_recieved: usize,
        ack_bits: u32,
    };
    pub const table_t = struct {
        packet_data: [10240]packet_data_t = undefined,
        sequence_buffer: [10240]usize = undefined,
        pub fn get_packet_data(this: *@This(), sequence_number: usize) ?*packet_data_t {
            const index: usize = sequence_number % this.packet_data.len;
            if (this.sequence_buffer[index] == sequence_number) {
                return &this.packet_data[index];
            } else return null;
        }

        pub fn insert(this: *@This(), sequence_number: usize) *packet_data_t {
            const index: usize = sequence_number % this.packet_data.len;
            this.sequence_buffer[index] = sequence_number;
            return &this.packet_data[index];
        }
    };

    mine: table_t = table_t{},
    theirs: table_t = table_t{},
    mine_sequence: usize = 1,
    their_sequence: usize = 0,
    their_last_recieved_of_my_sequence: usize = 0,

    pub fn generate_header(this: *@This(), _seq: usize) header_t {
        var bits: u32 = 0;

        for (0..32) |i| {
            if (!(this.their_sequence >= i)) continue;
            const seq = this.their_sequence - i;
            if (this.theirs.get_packet_data(seq) != null and this.theirs.get_packet_data(seq).?.acked) {
                bits |= @as(u32, 1) << @intCast(i);
            }
        }

        return header_t{
            .ack_bits = bits,
            .sequence_number = _seq,
            .most_recent_sequence_number_recieved = this.their_sequence,
        };
    }
    pub fn new_seq_number(this: *@This()) usize {
        const temp = this.mine_sequence;
        this.mine_sequence += 1;
        return temp;
    }
};

pub fn track_packet_for_send(rem_message: remote_message, tracker: *packet_ack_tracker_t, allocator: std.mem.Allocator) []const u8 {
    const header = tracker.generate_header(rem_message.seq);
    const header_bytes = zeng.loader.serialize_to_byte_slice(header, allocator);
    defer allocator.free(header_bytes);
    const data_with_header = std.fmt.allocPrint(allocator, "{s}{s}", .{ header_bytes, rem_message.payload }) catch unreachable;

    return data_with_header;
}

pub fn track_packet_from_recieve(headerful_bytes: []const u8, tracker: *packet_ack_tracker_t, allocator: std.mem.Allocator) ?[]const u8 {
    var header: packet_ack_tracker_t.header_t = undefined;

    var curr: u32 = 0;
    zeng.loader.deserialize_from_bytes(packet_ack_tracker_t.header_t, @as([*]u8, @ptrCast(&header)), headerful_bytes, &curr, 0);

    if (header.sequence_number > tracker.their_sequence) tracker.their_sequence = header.sequence_number;

    // if (header.sequence_number > 0) std.debug.print("client sequence number: {}\n", .{header.sequence_number});

    if (header.most_recent_sequence_number_recieved > 0) {
        const packet_to_ack = tracker.mine.get_packet_data(header.most_recent_sequence_number_recieved).?;
        if (packet_to_ack.acked == false) allocator.free(packet_to_ack.rem_message.?.payload);
        packet_to_ack.acked = true;

        var curr_bit: u32 = 1;
        var curr_seq = header.most_recent_sequence_number_recieved;
        while (true) {
            const is_curr_seq_acked = (curr_bit & header.ack_bits) != 0;
            if (is_curr_seq_acked) {
                const _packet_to_ack = tracker.mine.get_packet_data(curr_seq).?;
                if (_packet_to_ack.acked == false) allocator.free(_packet_to_ack.rem_message.?.payload);
                _packet_to_ack.acked = true;
            }

            if (curr_bit == 0x80000000) break;
            if (curr_seq == 0) break;
            curr_bit = curr_bit << 1;
            curr_seq -= 1;
        }
    }

    if (header.sequence_number == 0) return headerful_bytes[curr..];
    if (header.sequence_number + 64 < tracker.their_sequence) return null;
    if (tracker.theirs.get_packet_data(header.sequence_number) != null and tracker.theirs.get_packet_data(header.sequence_number).?.acked) return null;

    tracker.theirs.insert(header.sequence_number).acked = true;

    return headerful_bytes[curr..];
}

pub fn remote_event(commands: *zeng.commands, tracker: *packet_ack_tracker_t, socket: net.socket_t, peer: net.peer_info_t, event: anytype, channel: zeng.commands.reliability_channel) void {
    const payload_array = commands.allocator.alloc(u8, @sizeOf(u32) + @sizeOf(@TypeOf(event))) catch unreachable;
    var curr_byte: u32 = 0;
    zeng.loader.serialize_to_bytes(comptime zeng.GET_MSG_CODE(@TypeOf(event)), payload_array, &curr_byte);
    zeng.loader.serialize_to_bytes(event, payload_array, &curr_byte);

    const msg = remote_message{ .seq = if (channel == .reliable) tracker.new_seq_number() else 0, .resend_timer = net.resend_interval_sec, .payload = commands.allocator.realloc(payload_array, curr_byte) catch unreachable, .sender_socket = socket, .target_address = peer, .time_to_send = commands.get_sim_send_time(), .channel = channel };
    if (channel == .reliable) {
        tracker.mine.insert(msg.seq).* = .{ .acked = false, .timestamp = zeng.timer_get(), .rem_message = msg };
    }

    commands.remote_messages_send_queue[commands.remote_messages_send_queue_len] = msg;
    commands.remote_messages_send_queue_len += 1;
}
pub fn send_net_messages(commands: *zeng.commands, delta_time: f64, tracker: *packet_ack_tracker_t) void {
    _ = delta_time;

    var curr_seq: usize = tracker.their_last_recieved_of_my_sequence + 1;
    while (curr_seq < tracker.mine_sequence) {
        defer curr_seq += 1;

        const pd = tracker.mine.get_packet_data(curr_seq) orelse continue;
        const rem_message = pd.rem_message;
        const new_time = zeng.timer_get();
        if (!pd.acked and zeng.timer_calc_delta(pd.timestamp, new_time) > 0.5) {
            commands.remote_messages_send_queue[commands.remote_messages_send_queue_len] = rem_message.?;
            commands.remote_messages_send_queue_len += 1;
            pd.timestamp = new_time;
        }
    }

    var curr: usize = 0;
    while (curr < commands.remote_messages_send_queue_len) {
        const rem_message = commands.remote_messages_send_queue[curr];
        if (rem_message.time_to_send <= commands.time) {
            const data_with_header = track_packet_for_send(rem_message, tracker, commands.allocator);
            defer commands.allocator.free(data_with_header);
            if (commands.random.float(f32) > 0.2) {
                const err = std.os.windows.ws2_32.sendto(rem_message.sender_socket, data_with_header.ptr, @intCast(data_with_header.len), 0, &rem_message.target_address.sockaddr, rem_message.target_address.socklen);
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
            if (rem_message.channel != .reliable) commands.allocator.free(rem_message.payload);

            commands.remote_messages_send_queue[curr] = commands.remote_messages_send_queue[commands.remote_messages_send_queue_len - 1];
            commands.remote_messages_send_queue_len -= 1;
        } else curr += 1;
    }
}
pub fn recieve_net_messages(socket: socket_t, res: *zeng.resources_t, allocator: std.mem.Allocator, tracker: *packet_ack_tracker_t) void {
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
                continue :get_messages_loop;
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

        var curr: u32 = 0;

        const headerless_bytes = track_packet_from_recieve(recv_read_buf[0..], tracker, allocator) orelse continue;

        var event_code: u32 = undefined;
        @memcpy(@as([*]u8, @ptrCast(&event_code)), headerless_bytes[curr .. curr + @sizeOf(u32)]);
        curr += @sizeOf(u32);

        inline for (rpc.REMOTE_MESSAGE_TYPES) |msg_type| {
            if (event_code == comptime zeng.GET_MSG_CODE(msg_type)) {
                var payload: msg_type = undefined;

                var _curr: u32 = curr;
                zeng.loader.deserialize_from_bytes(msg_type, @as([*]u8, @ptrCast(&payload)), headerless_bytes[0..], &_curr, 0);

                if (res.get(zeng.events(msg_type)).addresses != null) {
                    const address = net.peer_info_t{ .sockaddr = sender_addr, .socklen = sender_addr_len };
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
