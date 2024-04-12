const std = @import("std");
pub const Engine = @import("engine.zig");

pub const GlobalData = struct {
    elapsed_time: f32 = 0.0,
    active_window: Engine.glfw.Window,
    active_camera_matrix: *[16]f32,
    active_camera: *Engine.Camera,
    cur_pos: Engine.glfw.Window.CursorPos,
    entity_slice: []Engine.Entity,
    frame_delta: f64 = 0.001,
    frozen: bool = false,
    shader_program_GPU: u32,
    texture_GPU: u32,
    window_width: u32,
    window_height: u32,
    t_down_consume: bool = false,
    allocator: std.mem.Allocator,
};

pub fn BigUpdate(gd: *GlobalData) void {
    // first system
    gd.cur_pos = gd.active_window.getCursorPos();
    gd.elapsed_time += @floatCast(gd.frame_delta);

    // t button press / import scene
    if (gd.active_window.getKey(Engine.glfw.Key.t) == Engine.glfw.Action.press and !gd.t_down_consume) {
        const input_read: []u8 = Engine.GetBytesFromFile("assets/extras/command_input.txt", std.heap.c_allocator);
        defer std.heap.c_allocator.free(input_read);

        const imported = Engine.ImportModelAsset("assets/blender_files/simple.bin", gd.allocator, gd.shader_program_GPU, gd.texture_GPU, &gd.entity_slice);
        defer gd.allocator.free(imported);
        for (imported) |entity| {
            if (std.mem.eql(u8, entity.name.?, "circ")) {
                entity.transform[13] += 10.0;
                entity.component_flags.sine_mover = true;
            }
        }
    }
    gd.t_down_consume = gd.active_window.getKey(Engine.glfw.Key.t) == Engine.glfw.Action.press;

    // sine mover system
    for (gd.entity_slice) |*entity| {
        if (entity.component_flags.sine_mover) {
            entity.transform[14] = @sin(gd.elapsed_time * 3.0);
            entity.transform[12] = @cos(gd.elapsed_time * 5.0);
        }
    }

    // ghost system
    for (gd.entity_slice) |*entity| {
        if (entity.component_flags.ghost) {
            var speed: f32 = @floatCast(gd.frame_delta * 100.0);
            if (gd.active_window.getKey(Engine.glfw.Key.left_shift) == Engine.glfw.Action.press) {
                speed *= 0.2;
            } else {
                speed *= 0.05;
            }
            if (gd.active_window.getKey(Engine.glfw.Key.a) == Engine.glfw.Action.press) {
                entity.transform[12] -= entity.transform[0] * speed;
                entity.transform[13] -= entity.transform[1] * speed;
                entity.transform[14] -= entity.transform[2] * speed;
            }
            if (gd.active_window.getKey(Engine.glfw.Key.d) == Engine.glfw.Action.press) {
                entity.transform[12] += entity.transform[0] * speed;
                entity.transform[13] += entity.transform[1] * speed;
                entity.transform[14] += entity.transform[2] * speed;
            }
            if (gd.active_window.getKey(Engine.glfw.Key.q) == Engine.glfw.Action.press) {
                entity.transform[12] -= entity.transform[4] * speed;
                entity.transform[13] -= entity.transform[5] * speed;
                entity.transform[14] -= entity.transform[6] * speed;
            }
            if (gd.active_window.getKey(Engine.glfw.Key.e) == Engine.glfw.Action.press) {
                entity.transform[12] += entity.transform[4] * speed;
                entity.transform[13] += entity.transform[5] * speed;
                entity.transform[14] += entity.transform[6] * speed;
            }
            if (gd.active_window.getKey(Engine.glfw.Key.w) == Engine.glfw.Action.press) {
                entity.transform[12] -= entity.transform[8] * speed;
                entity.transform[13] -= entity.transform[9] * speed;
                entity.transform[14] -= entity.transform[10] * speed;
            }
            if (gd.active_window.getKey(Engine.glfw.Key.s) == Engine.glfw.Action.press) {
                entity.transform[12] += entity.transform[8] * speed;
                entity.transform[13] += entity.transform[9] * speed;
                entity.transform[14] += entity.transform[10] * speed;
            }
        }
    }

    // camera mouse look system
    for (gd.entity_slice) |*entity| {
        if (entity.component_flags.camera) {
            var pos: [3]f32 = undefined;
            pos[0] = entity.transform[12];
            pos[1] = entity.transform[13];
            pos[2] = entity.transform[14];
            const rot_mat_hor = Engine.axis_angle_to_matrix(.{ .x = 0, .y = 1, .z = 0 }, @floatCast(gd.cur_pos.xpos * -0.0015));
            const rot_mat_vert = Engine.axis_angle_to_matrix(.{ .x = 1, .y = 0, .z = 0 }, @floatCast(gd.cur_pos.ypos * -0.0015));
            entity.transform = Engine.multiply_matrices(rot_mat_hor, rot_mat_vert);
            entity.transform[12] = pos[0];
            entity.transform[13] = pos[1];
            entity.transform[14] = pos[2];
        }
    }

    // render system
    Engine.gl.clearColor(0.2, 0.3, 0.3, 1.0);
    Engine.gl.clear(Engine.gl.COLOR_BUFFER_BIT | Engine.gl.DEPTH_BUFFER_BIT);
    const inv_camera_matrix: [16]f32 = Engine.InvertMatrix(gd.active_camera_matrix.*);
    for (gd.entity_slice) |*entity| {
        if (entity.component_flags.mesh) {
            Engine.DrawMesh(entity.*, gd.active_camera.projection_matrix, inv_camera_matrix);
        }
    }
    gd.active_window.swapBuffers();
}
