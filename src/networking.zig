const std = @import("std");

const FIONBIO: u32 = 0x8004667e;

const ecs = @import("ecs.zig");
const zeng = @import("zeng.zig");

const Transform = zeng.Transform;
const EventWriter = zeng.EventWriter;
const PlayerEvent = @import("main.zig").PlayerEvent;
const ECS = @import("main.zig").ECS;

pub const NetAddressSized = struct {
    client_addr: std.os.sockaddr,
    client_addr_len: std.os.socklen_t,
};
pub const ConnectionID = u32; // represents the connection
pub const NetworkID = u32; // gloablly unique identifier of an object that is synced between computers
pub const SocketAndAddress = struct {
    socket: std.os.socket_t,
    address: std.net.Address,
};
pub const RemoteMessage = struct {
    payload: []u8,
    target: SocketAndAddress,
};

fn WINDOWS_set_socket_non_blocking(sock: std.os.socket_t) !void {
    const one: u32 = 1;
    const one_ptr = @as([*]const u8, @ptrCast(&one))[0..4];
    _ = try std.os.windows.WSAIoctl(sock, FIONBIO, one_ptr, ""[0..0], null, null); // windows' way of setting a socket to non blocking (disgusting)
}

// prepare a socket and address
pub fn make_udp_sock_and_address(address: anytype, port: anytype, comptime nonblocking: bool) !SocketAndAddress {
    const addr = try std.net.Address.parseIp(address, port);
    const sock = try std.os.socket(std.os.AF.INET, std.os.SOCK.DGRAM, std.os.IPPROTO.UDP);
    if (nonblocking)
        try WINDOWS_set_socket_non_blocking(sock);
    return SocketAndAddress{ .socket = sock, .address = addr };
}
// (server usually) bind the specified socket and address for communication
pub fn bind_socket_and_address(socket_and_address: SocketAndAddress) !void {
    try std.os.bind(socket_and_address.socket, &socket_and_address.address.any, socket_and_address.address.getOsSockLen());
}
pub fn network_send_all(commands: *zeng.Commands) !void {
    for (commands.remote_messages[0..commands.remote_messages_len]) |remote_message| {
        _ = try std.os.sendto(remote_message.target.socket, remote_message.payload, 0, &remote_message.target.address.any, remote_message.target.address.getOsSockLen());
        commands.allocator.free(remote_message.payload);
    }
    commands.remote_messages_len = 0;
}
pub fn network_recieve_all(socket: std.os.socket_t, world: *ECS.World) !void {
    var client_addr: std.os.sockaddr = undefined;
    var client_addr_len: std.os.socklen_t = @sizeOf(std.os.sockaddr);

    var recv_read_buf: [4096]u8 = undefined;
    get_messages_loop: while (true) {
        const recv_result = std.os.recvfrom(socket, &recv_read_buf, 0, &client_addr, &client_addr_len);
        if (recv_result) |_| {
            var procedure_code: u32 = undefined;
            @memcpy(@as([*]u8, @ptrCast(&procedure_code)), recv_read_buf[0..4]);

            //@Meta

            inline for (zeng.procs) |proc| {
                if (procedure_code == comptime zeng.GET_PROC_CODE(proc)) {
                    var args: zeng.args_to_serialize[zeng.GET_PROC_CODE(proc)] = undefined;
                    var curr: u32 = 4;
                    zeng.deserialize_from_bytes(zeng.args_to_serialize[zeng.GET_PROC_CODE(proc)], @as([*]u8, @ptrCast(&args)), recv_read_buf[0..], &curr, 0);
                    var captures: zeng.args_to_retrieve[zeng.GET_PROC_CODE(proc)] = undefined;

                    if (@sizeOf(@TypeOf(captures)) > 0) {
                        inline for (&captures) |*cap| {
                            if (@TypeOf(cap.*) == *ECS.World) {
                                cap.* = world;
                            }
                        }
                        const args2 = args ++ captures;
                        @call(.auto, proc, args2);
                    } else {
                        @call(.auto, proc, args);
                        break;
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
