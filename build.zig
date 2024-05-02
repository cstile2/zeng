const std = @import("std");

pub fn build(b: *std.Build) !void {
    // END META PROGRAMMING
    // standard stuff
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const exe = b.addExecutable(.{
        .name = "colsengine",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    // use glfw package from package manager via build.zig.zon
    const glfw_dep = b.dependency("mach_glfw", .{ .target = target, .optimize = optimize });
    exe.root_module.addImport("mach-glfw", glfw_dep.module("mach-glfw"));

    // use opengl zig code
    exe.root_module.addImport("gl", b.createModule(.{ .root_source_file = .{ .path = "libs/gl41.zig" } }));

    // use C source file
    exe.root_module.addCSourceFile(.{
        .file = .{
            .path = "c_libs/stb_image.c",
        },
    });

    // include C header files
    exe.root_module.addIncludePath(.{
        .path = "c_libs/",
    });

    // do the stuff
    b.installArtifact(exe);

    // allow "zig build run" to be a thing
    const exe_run = b.addRunArtifact(exe);
    const exe_step = b.step("run", "Run the program");
    exe_step.dependOn(&exe_run.step);
}
