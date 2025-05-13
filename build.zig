const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // main executable
    const exe = b.addExecutable(.{
        .name = "colsengine",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    exe.linkSystemLibrary("ole32");
    exe.linkSystemLibrary("uuid");
    exe.linkSystemLibrary("Mmdevapi");

    // use glfw package from package manager via build.zig.zon - add as import
    const glfw_dep = b.dependency("mach_glfw", .{ .target = target, .optimize = optimize });
    exe.root_module.addImport("mach-glfw", glfw_dep.module("mach-glfw"));

    // use opengl zig code - add as import (is there a way to do it as not a module if its in a different file?)
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

    // install artifact
    b.installArtifact(exe);

    // allow "zig build run" to be a thing
    const exe_run = b.addRunArtifact(exe);
    const exe_run_command = b.step("run", "Run the program");
    exe_run_command.dependOn(&exe_run.step);
}
