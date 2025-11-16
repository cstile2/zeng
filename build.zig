const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const gl_module = b.addModule("gl", .{
        .root_source_file = b.path("libs/gl41.zig"),
        .target = target,
        .optimize = optimize,
    });
    const zeng_module = b.addModule("zeng", .{
        .root_source_file = b.path("src/engine/zeng.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "gl", .module = gl_module },
        },
    });
    const hot_reload_module = b.addModule("hot_reload", .{
        .root_source_file = b.path("dynamic/hot_reload.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "zeng", .module = zeng_module },
        },
    });

    const exe = b.addExecutable(.{
        .name = "colsengine",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "hot_reload", .module = hot_reload_module },
                .{ .name = "zeng", .module = zeng_module },
            },
        }),
    });
    b.installArtifact(exe);

    // compile the library as a DLL
    const hot_reload_dll = b.addLibrary(.{
        .name = "hot_reload",
        .root_module = hot_reload_module,
        .linkage = .dynamic,
    });
    b.installArtifact(hot_reload_dll);

    zeng_module.linkSystemLibrary("ole32", .{});
    zeng_module.linkSystemLibrary("uuid", .{});
    zeng_module.linkSystemLibrary("Mmdevapi", .{});
    zeng_module.linkSystemLibrary("opengl32", .{});
    zeng_module.linkSystemLibrary("gdi32", .{});
    zeng_module.linkSystemLibrary("ws2_32", .{});

    zeng_module.addIncludePath(b.path("c_libs/"));
    zeng_module.addCSourceFile(.{ .file = b.path("c_libs/stb_image.c") });

    const exe_run = b.addRunArtifact(exe);
    const exe_run_command = b.step("run", "Run the program");
    exe_run_command.dependOn(&exe_run.step);

    const dll_build_command = b.step("hot", "compile hot reload code");
    dll_build_command.dependOn(&hot_reload_dll.step);
}
