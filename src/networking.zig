const std = @import("std");
const FIONBIO: u32 = 0x8004667e;
pub const ClientInfo = struct {
    client_addr: std.os.sockaddr,
    client_addr_len: std.os.socklen_t,
};
const ecs = @import("ecs.zig");
const Transform = @import("engine.zig").Transform;

pub var ClientMap: std.AutoArrayHashMap(ClientInfo, ?ecs.EntityDataLocation) = undefined;

fn Windows_SetSocketNonBlocking(sock: std.os.socket_t) !void {
    const one: u32 = 1;
    const one_ptr = @as([*]const u8, @ptrCast(&one))[0..4];
    _ = try std.os.windows.WSAIoctl(sock, FIONBIO, one_ptr, ""[0..0], null, null); // windows' way of setting a socket to non blocking (disgusting)
}

pub fn CreateSocketAddress(address: anytype, port: anytype, comptime nonblocking: bool) !struct { std.os.socket_t, std.net.Address } {
    const addr = try std.net.Address.parseIp(address, port);
    const sock = try std.os.socket(std.os.AF.INET, std.os.SOCK.DGRAM, std.os.IPPROTO.UDP);
    if (nonblocking)
        try Windows_SetSocketNonBlocking(sock);
    return .{ sock, addr };
}

pub fn BindSocketAddress(bundle: struct { std.os.socket_t, std.net.Address }) !void {
    try std.os.bind(bundle[0], &bundle[1].any, bundle[1].getOsSockLen());
}

/// looks through and filters all incoming packets, and adds all valid packets to a queue of "commands" which game/engine uses directly
pub fn ServerSiftPackets(existing_players: []struct { ClientInfo, []u8 }, player_update_num: *u32, sock: std.os.socket_t, output: []u8) !void {
    var output_len: u64 = 0;
    var buf: [1024]u8 = undefined;

    var client_addr: std.os.sockaddr = undefined;
    var client_addr_len: std.os.socklen_t = @sizeOf(std.os.sockaddr);

    full_check_loop: while (true) {
        const recv_result = std.os.recvfrom(sock, &buf, 0, &client_addr, &client_addr_len);
        if (recv_result) |recv_len| {
            // we just read from recv, now we put the packet data into a slice of a buffer, and package that slice with the slice of slices
            const client_info = ClientInfo{ .client_addr = client_addr, .client_addr_len = client_addr_len };
            @memcpy(output[output_len .. output_len + recv_len], buf[0..recv_len]);

            const res = try ClientMap.getOrPut(client_info);
            if (res.found_existing) {
                existing_players[player_update_num.*] = .{ client_info, output[output_len .. output_len + recv_len] };
                player_update_num.* += 1;
            } else {
                res.value_ptr.* = null;
            }

            output_len += recv_len;
        } else |err| {
            switch (err) {
                std.os.RecvFromError.WouldBlock => {
                    break :full_check_loop;
                },
                else => return err,
            }
        }
    }
}

pub fn Server() !void {
    const addr = try std.net.Address.parseIp("0.0.0.0", 55555);
    const sock = try std.os.socket(std.os.AF.INET, std.os.SOCK.DGRAM, std.os.IPPROTO.UDP);
    defer std.os.close(sock);
    try Windows_SetSocketNonBlocking(sock);

    try std.os.bind(sock, &addr.any, addr.getOsSockLen());
    var client_addr: std.os.sockaddr = undefined;
    var client_addr_len: std.os.socklen_t = @sizeOf(std.os.sockaddr);

    const message = "Hello, Client!";

    var buf: [1024]u8 = undefined;
    main_loop: while (true) {
        var recv_result: std.os.RecvFromError!usize = 1;
        full_check_loop: while (true) {
            recv_result = std.os.recvfrom(sock, &buf, 0, &client_addr, &client_addr_len);
            if (recv_result) |len| {
                std.debug.print("recieved: '{s}'\n", .{buf[0..len]});
                _ = try std.os.sendto(sock, message, 0, &client_addr, client_addr_len);
            } else |err| {
                switch (err) {
                    std.os.RecvFromError.WouldBlock => {
                        break :full_check_loop;
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
    const addr = try std.net.Address.parseIp("127.0.0.1", 55555);
    const sock = try std.os.socket(std.os.AF.INET, std.os.SOCK.DGRAM, std.os.IPPROTO.UDP);
    defer std.os.close(sock);
    try Windows_SetSocketNonBlocking(sock);

    const message = "Hello, Server!";
    _ = try std.os.sendto(sock, message, 0, &addr.any, addr.getOsSockLen());

    var buf: [1024]u8 = undefined;
    main_loop: while (true) {
        var recv_result: std.os.RecvFromError!usize = 1;
        full_check_loop: while (true) {
            recv_result = std.os.recv(sock, &buf, 0);
            if (recv_result) |len| {
                std.debug.print("recieved: '{s}'\n", .{buf[0..len]});
                _ = try std.os.sendto(sock, message, 0, &addr.any, addr.getOsSockLen());
            } else |err| {
                switch (err) {
                    std.os.RecvFromError.WouldBlock => {
                        break :full_check_loop;
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
