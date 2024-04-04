const std = @import("std");

pub const StringHash = [_][]const u8{
    "Transform",
    "Velocity",
    "Mesh",
    "Sine_mover",
    "Ghost",
    "Camera",
};

pub fn ComponentHash(comptime T: type) u64 {
    var curr: u64 = 1;
    for (StringHash) |string| {
        const size: isize = @intCast(string.len);
        const type_size: isize = @intCast(@typeName(T).len);
        const delta: isize = type_size - size;
        if (delta < 0) {
            continue;
        }
        if (std.mem.eql(u8, string, @typeName(T)[@intCast(delta)..])) {
            return curr;
        }
        curr = curr << 1;
    }
    @compileError("Component type is not valid: " ++ @typeName(T));
}

pub fn ComponentIndex(comptime T: type) u64 {
    var curr: u64 = 0;
    for (StringHash) |string| {
        const size: isize = @intCast(string.len);
        const type_size: isize = @intCast(@typeName(T).len);
        const delta: isize = type_size - size;
        if (delta < 0) {
            continue;
        }
        if (std.mem.eql(u8, string, @typeName(T)[@intCast(delta)..])) {
            return curr;
        }
        curr = curr + 1;
    }
    @compileError("Component type is not valid: " ++ @typeName(T));
}

const ArchetypeError = error{
    Invalid,
};

fn AddressOffset(T: type, ptr: anytype, offset: usize) *T {
    return @ptrFromInt(@intFromPtr(ptr) + offset * @sizeOf(T));
}

pub const EntityDataLocation = struct {
    table_index: u64,
    archetype_hash: u64,
};

pub const ECSWorld = struct {
    Tables: std.AutoArrayHashMap(u64, ArchetypeTable),
    pub fn Init(self: *ECSWorld, allocator: std.mem.Allocator) void {
        self.Tables = std.AutoArrayHashMap(u64, ArchetypeTable).init(allocator);
    }
    pub fn AddEntity(self: *ECSWorld, tuple: anytype, allocator: std.mem.Allocator) ?EntityDataLocation {
        comptime var curr_hash = 0;
        inline for (tuple) |field| {
            const hash = comptime ComponentHash(@TypeOf(field));
            curr_hash = curr_hash ^ hash;
        }

        const thing = self.Tables.getOrPut(curr_hash) catch unreachable;

        if (!thing.found_existing) {
            thing.value_ptr.* = ArchetypeTable{};
            thing.value_ptr.Init(32);
            inline for (tuple) |field| {
                thing.value_ptr.AddComponentType(allocator, @TypeOf(field));
            }
            thing.value_ptr.AddEntity(tuple);

            return EntityDataLocation{ .archetype_hash = curr_hash, .table_index = thing.value_ptr.Size - 1 };
        }

        thing.value_ptr.AddEntity(tuple);
        return EntityDataLocation{ .archetype_hash = curr_hash, .table_index = thing.value_ptr.Size - 1 };
    }
    pub fn GetComponent(self: *ECSWorld, T: type, edl: EntityDataLocation) ?*T {
        std.debug.print("Hash: {}\n", .{edl.archetype_hash});
        if (self.Tables.getEntry(edl.archetype_hash)) |entry| {
            return entry.value_ptr.GetComponent(T, edl.table_index);
        } else {
            std.debug.print("Hash was not found\n", .{});
            return null;
        }
    }
    // TODO: implement me
    pub fn SetComponent(self: *ECSWorld, T: type, edl: EntityDataLocation) void {
        _ = self; // autofix
        _ = T; // autofix
        _ = edl; // autofix

    }
};

pub const ArchetypeTable = struct {
    Map: [64]?OpaqueComponentStorage = undefined,
    Size: u64 = undefined,
    Archetype: u64 = undefined,
    Capacity: u64 = undefined,
    pub fn Init(self: *ArchetypeTable, capacity: u64) void {
        self.Map = .{null} ** 64;
        self.Capacity = capacity;
        self.Size = 0;
        self.Archetype = 0;
    }
    pub fn AddComponentType(self: *ArchetypeTable, allocator: std.mem.Allocator, T: type) void {
        if (self.Map[comptime ComponentIndex(T)]) |component_storage| {
            _ = component_storage;
            return;
        }
        self.Map[comptime ComponentIndex(T)] = OpaqueComponentStorage{};
        self.Map[comptime ComponentIndex(T)].?.Create(T, self.Capacity, allocator);
    }
    pub fn AddEntity(self: *ArchetypeTable, tuple: anytype) void {
        inline for (tuple) |field| {
            const index = comptime ComponentIndex(@TypeOf(field));
            if (self.Map[index]) |component_storage| {
                const ptr = @as(*@TypeOf(field), @alignCast(@ptrCast(component_storage.array)));
                AddressOffset(@TypeOf(field), ptr, self.Size).* = field;
            } else {
                std.debug.print("ERROR: Tried set component type that does not exist yet", .{});
                return;
            }
        }
        self.Size += 1;
    }
    pub fn GetComponent(self: *ArchetypeTable, T: type, index: u64) ?*T {
        const row: u64 = comptime ComponentIndex(T);
        if (self.Map[row]) |component_storage| {
            const ptr = @as(*T, @alignCast(@ptrCast(component_storage.array)));
            return AddressOffset(T, ptr, index);
        } else {
            std.debug.print("ERROR: Tried get component type that does not exist yet", .{});
            return null;
        }
    }
};

pub const OpaqueComponentStorage = struct {
    array: *anyopaque = undefined,
    len: u64 = undefined,
    pub fn Create(self: *OpaqueComponentStorage, T: type, len: u64, allocator: std.mem.Allocator) void {
        const guy = allocator.alloc(T, len) catch unreachable;
        self.array = @ptrCast(guy.ptr);
        self.len = len;
    }
};
