const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const gl = b.addModule("gl", .{
        .root_source_file = b.path("libs/gl41.zig"),
        .target = target,
        .optimize = optimize,
    });
    const dyn = b.addModule("dyn", .{
        .root_source_file = b.path("dynamic/lib.zig"),
        .target = target,
        .optimize = optimize,
    });

    const exe = b.addExecutable(.{
        .name = "colsengine",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "gl", .module = gl },
                .{ .name = "dyn", .module = dyn },
            },
        }),
    });
    b.installArtifact(exe);

    const lib = b.addLibrary(.{
        .name = "mylib",
        .root_module = b.createModule(.{
            .root_source_file = b.path("dynamic/lib.zig"),
            .target = target,
            .optimize = optimize,
        }),
        .linkage = .dynamic,
    });
    b.installArtifact(lib);

    exe.linkSystemLibrary("ole32");
    exe.linkSystemLibrary("uuid");
    exe.linkSystemLibrary("Mmdevapi");
    exe.linkSystemLibrary("opengl32");
    exe.linkSystemLibrary("gdi32");
    exe.linkSystemLibrary("ws2_32");

    exe.root_module.addIncludePath(b.path("c_libs/"));
    exe.root_module.addCSourceFile(.{ .file = b.path("c_libs/stb_image.c") });
    exe.root_module.addCSourceFile(.{ .file = b.path("c_libs/clay.c") });

    const exe_run = b.addRunArtifact(exe);
    const exe_run_command = b.step("run", "Run the program");
    exe_run_command.dependOn(&exe_run.step);
}
