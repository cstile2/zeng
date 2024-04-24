const std = @import("std");
pub const Engine = @import("engine.zig");

pub const GlobalData = struct {
    elapsed_time: f32 = 0.0,
    active_window: Engine.glfw.Window,
    active_camera_matrix: *[16]f32,
    active_camera: *Engine.Camera,
    cur_pos: Engine.glfw.Window.CursorPos,
    frame_delta: f64 = 0.001,
    frozen: bool = false,
    shader_program_GPU: u32,
    texture_GPU: u32,
    window_width: u32,
    window_height: u32,
    t_down_last_frame: bool = false,
    allocator: std.mem.Allocator,
};
