const std = @import("std");
const glfw = @import("mach-glfw");
const gl = @import("gl");
const c = @cImport({
    @cInclude("stb_image.h");
});
const Engine = struct {
    usingnamespace @import("render.zig");
    usingnamespace @import("data_types.zig");
};

const DataBase = @import("main.zig").DataBase;

pub fn SYSTEM_Input(db: *DataBase) void {
    db.cur_pos = db.active_window.getCursorPos();
}

pub fn SYSTEM_Constant(db: *DataBase) void {
    db.time += @floatCast(db.frame_delta);
}

pub fn SYSTEM_SineMover(db: *DataBase) void {
    for (db.entity_slice) |*entity| {
        if (entity.component_flags.sine_mover) {
            entity.world_matrix[14] = @sin(db.time);
            entity.world_matrix[12] = @cos(db.time);
        }
    }
}

// used to move spectator-like objects
pub fn SYSTEM_Ghost(db: *DataBase) void {
    for (db.entity_slice) |*entity| {
        if (entity.component_flags.ghost) {
            var speed: f32 = @floatCast(db.frame_delta * 100.0);
            if (db.active_window.getKey(glfw.Key.left_shift) == glfw.Action.press) {
                speed *= 0.2;
            } else {
                speed *= 0.05;
            }
            if (db.active_window.getKey(glfw.Key.a) == glfw.Action.press) {
                entity.world_matrix[12] -= entity.world_matrix[0] * speed;
                entity.world_matrix[13] -= entity.world_matrix[1] * speed;
                entity.world_matrix[14] -= entity.world_matrix[2] * speed;
            }
            if (db.active_window.getKey(glfw.Key.d) == glfw.Action.press) {
                entity.world_matrix[12] += entity.world_matrix[0] * speed;
                entity.world_matrix[13] += entity.world_matrix[1] * speed;
                entity.world_matrix[14] += entity.world_matrix[2] * speed;
            }
            if (db.active_window.getKey(glfw.Key.q) == glfw.Action.press) {
                entity.world_matrix[12] -= entity.world_matrix[4] * speed;
                entity.world_matrix[13] -= entity.world_matrix[5] * speed;
                entity.world_matrix[14] -= entity.world_matrix[6] * speed;
            }
            if (db.active_window.getKey(glfw.Key.e) == glfw.Action.press) {
                entity.world_matrix[12] += entity.world_matrix[4] * speed;
                entity.world_matrix[13] += entity.world_matrix[5] * speed;
                entity.world_matrix[14] += entity.world_matrix[6] * speed;
            }
            if (db.active_window.getKey(glfw.Key.w) == glfw.Action.press) {
                entity.world_matrix[12] -= entity.world_matrix[8] * speed;
                entity.world_matrix[13] -= entity.world_matrix[9] * speed;
                entity.world_matrix[14] -= entity.world_matrix[10] * speed;
            }
            if (db.active_window.getKey(glfw.Key.s) == glfw.Action.press) {
                entity.world_matrix[12] += entity.world_matrix[8] * speed;
                entity.world_matrix[13] += entity.world_matrix[9] * speed;
                entity.world_matrix[14] += entity.world_matrix[10] * speed;
            }
        }
    }
}

pub fn SYSTEM_MeshDrawer(db: *DataBase) void {
    // set color and clear the screen
    gl.clearColor(0.2, 0.3, 0.3, 1.0);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
    const inv_camera_matrix: [16]f32 = Engine.InvertMatrix(db.active_camera_matrix.*);
    for (db.entity_slice) |*entity| {
        if (entity.component_flags.mesh) {
            Engine.DrawMesh(entity.*, db.active_camera.projection_matrix, inv_camera_matrix);
        }
    }
    // display pixels to the screen
    db.active_window.swapBuffers();
}

pub fn SYSTEM_CameraControls(db: *DataBase) void {
    for (db.entity_slice) |*entity| {
        if (entity.component_flags.camera) {
            var pos: [3]f32 = undefined;
            pos[0] = entity.world_matrix[12];
            pos[1] = entity.world_matrix[13];
            pos[2] = entity.world_matrix[14];
            const rot_mat_hor = Engine.axis_angle_to_matrix(.{ .x = 0, .y = 1, .z = 0 }, @floatCast(db.cur_pos.xpos * -0.002));
            const rot_mat_vert = Engine.axis_angle_to_matrix(.{ .x = 1, .y = 0, .z = 0 }, @floatCast(db.cur_pos.ypos * -0.002));
            entity.world_matrix = Engine.multiply_matrices(rot_mat_hor, rot_mat_vert);
            entity.world_matrix[12] = pos[0];
            entity.world_matrix[13] = pos[1];
            entity.world_matrix[14] = pos[2];
        }
    }
}

pub fn AddSystem(system: *const fn (*DataBase) void, schedule: *[]*const fn (*DataBase) void) void {
    schedule.len += 1;
    schedule.*[schedule.len - 1] = system;
}
