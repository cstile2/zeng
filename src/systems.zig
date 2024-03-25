const std = @import("std");
const glfw = @import("mach-glfw");
const gl = @import("gl");
const c = @cImport({
    @cInclude("stb_image.h");
});
const Engine = @import("engine.zig");

const GlobalData = @import("main.zig").GlobalData;

pub fn SYSTEM_Input(gd: *GlobalData) void {
    gd.cur_pos = gd.active_window.getCursorPos();
}

pub fn SYSTEM_Constant(gd: *GlobalData) void {
    gd.elapsed_time += @floatCast(gd.frame_delta);
}

pub fn SYSTEM_SineMover(gd: *GlobalData) void {
    for (gd.entity_slice) |*entity| {
        if (entity.component_flags.sine_mover) {
            entity.world_matrix[14] = @sin(gd.elapsed_time * 6.28);
            entity.world_matrix[12] = @cos(gd.elapsed_time * 6.28);
        }
    }
}

// used to move spectator-like objects
pub fn SYSTEM_Ghost(gd: *GlobalData) void {
    for (gd.entity_slice) |*entity| {
        if (entity.component_flags.ghost) {
            var speed: f32 = @floatCast(gd.frame_delta * 100.0);
            if (gd.active_window.getKey(glfw.Key.left_shift) == glfw.Action.press) {
                speed *= 0.2;
            } else {
                speed *= 0.05;
            }
            if (gd.active_window.getKey(glfw.Key.a) == glfw.Action.press) {
                entity.world_matrix[12] -= entity.world_matrix[0] * speed;
                entity.world_matrix[13] -= entity.world_matrix[1] * speed;
                entity.world_matrix[14] -= entity.world_matrix[2] * speed;
            }
            if (gd.active_window.getKey(glfw.Key.d) == glfw.Action.press) {
                entity.world_matrix[12] += entity.world_matrix[0] * speed;
                entity.world_matrix[13] += entity.world_matrix[1] * speed;
                entity.world_matrix[14] += entity.world_matrix[2] * speed;
            }
            if (gd.active_window.getKey(glfw.Key.q) == glfw.Action.press) {
                entity.world_matrix[12] -= entity.world_matrix[4] * speed;
                entity.world_matrix[13] -= entity.world_matrix[5] * speed;
                entity.world_matrix[14] -= entity.world_matrix[6] * speed;
            }
            if (gd.active_window.getKey(glfw.Key.e) == glfw.Action.press) {
                entity.world_matrix[12] += entity.world_matrix[4] * speed;
                entity.world_matrix[13] += entity.world_matrix[5] * speed;
                entity.world_matrix[14] += entity.world_matrix[6] * speed;
            }
            if (gd.active_window.getKey(glfw.Key.w) == glfw.Action.press) {
                entity.world_matrix[12] -= entity.world_matrix[8] * speed;
                entity.world_matrix[13] -= entity.world_matrix[9] * speed;
                entity.world_matrix[14] -= entity.world_matrix[10] * speed;
            }
            if (gd.active_window.getKey(glfw.Key.s) == glfw.Action.press) {
                entity.world_matrix[12] += entity.world_matrix[8] * speed;
                entity.world_matrix[13] += entity.world_matrix[9] * speed;
                entity.world_matrix[14] += entity.world_matrix[10] * speed;
            }
        }
    }
}

pub fn SYSTEM_MeshDrawer(gd: *GlobalData) void {
    // set color and clear the screen
    gl.clearColor(0.2, 0.3, 0.3, 1.0);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
    const inv_camera_matrix: [16]f32 = Engine.InvertMatrix(gd.active_camera_matrix.*);
    for (gd.entity_slice) |*entity| {
        if (entity.component_flags.mesh) {
            Engine.DrawMesh(entity.*, gd.active_camera.projection_matrix, inv_camera_matrix);
        }
    }
    // display pixels to the screen
    gd.active_window.swapBuffers();
}

pub fn SYSTEM_CameraControls(gd: *GlobalData) void {
    for (gd.entity_slice) |*entity| {
        if (entity.component_flags.camera) {
            var pos: [3]f32 = undefined;
            pos[0] = entity.world_matrix[12];
            pos[1] = entity.world_matrix[13];
            pos[2] = entity.world_matrix[14];
            const rot_mat_hor = Engine.axis_angle_to_matrix(.{ .x = 0, .y = 1, .z = 0 }, @floatCast(gd.cur_pos.xpos * -0.002));
            const rot_mat_vert = Engine.axis_angle_to_matrix(.{ .x = 1, .y = 0, .z = 0 }, @floatCast(gd.cur_pos.ypos * -0.002));
            entity.world_matrix = Engine.multiply_matrices(rot_mat_hor, rot_mat_vert);
            entity.world_matrix[12] = pos[0];
            entity.world_matrix[13] = pos[1];
            entity.world_matrix[14] = pos[2];
        }
    }
}

pub fn AddSystem(system: *const fn (*GlobalData) void, schedule: *[]*const fn (*GlobalData) void) void {
    schedule.len += 1;
    schedule.*[schedule.len - 1] = system;
}
