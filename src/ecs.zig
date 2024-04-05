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
fn Offset(ptr: anytype, offset: usize) *anyopaque {
    return @ptrFromInt(@intFromPtr(ptr) + offset);
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
    pub fn SpawnEntity(self: *ECSWorld, tuple: anytype, allocator: std.mem.Allocator) ?EntityDataLocation {
        comptime var curr_hash = 0;
        inline for (tuple) |field| {
            const hash = comptime ComponentHash(@TypeOf(field));
            curr_hash = curr_hash | hash;
        }

        const thing = self.Tables.getOrPut(curr_hash) catch unreachable;

        if (!thing.found_existing) {
            thing.value_ptr.* = ArchetypeTable{};
            thing.value_ptr.Init(128);
            inline for (tuple) |field| {
                thing.value_ptr.AddComponentType(allocator, @TypeOf(field));
            }
            thing.value_ptr.SpawnEntity(tuple);

            return EntityDataLocation{ .archetype_hash = curr_hash, .table_index = thing.value_ptr.Size - 1 };
        }

        thing.value_ptr.SpawnEntity(tuple);
        return EntityDataLocation{ .archetype_hash = curr_hash, .table_index = thing.value_ptr.Size - 1 };
    }
    pub fn GetComponent(self: *ECSWorld, T: type, edl: EntityDataLocation) ?*T {
        if (self.Tables.getEntry(edl.archetype_hash)) |entry| {
            return entry.value_ptr.GetComponent(T, edl.table_index);
        }
        return null;
    }
    pub fn SetComponent(self: *ECSWorld, V: anytype, edl: EntityDataLocation) void {
        const new_hash = (comptime ComponentHash(@TypeOf(V))) | edl.archetype_hash; // calculate the new hash
        if (new_hash == edl.archetype_hash) { // test if we stay in same table
            std.debug.print("SAME HASH\n", .{});
            if (self.Tables.getEntry(edl.archetype_hash)) |entry| {
                entry.value_ptr.GetComponent(@TypeOf(V), edl.table_index).?.* = V;
            }
            return;
        }
        // we must jump archetype tables!

        // move to new archetype table - create one if we have to
        const _new_entry = self.Tables.getEntry(new_hash);
        if (_new_entry == null) {
            std.debug.print("No table with requested archetype exists\n", .{});
            // create the new table here!
            return;
        }
        var new_entry = self.Tables.getEntry(new_hash).?;
        const old_entry = self.Tables.getEntry(edl.archetype_hash).?;
        std.debug.print("Table with requested archetype was found, hash: {}\n", .{new_hash});
        for (&new_entry.value_ptr.Map, &old_entry.value_ptr.Map) |*new_storage, *old_storage| {
            if (old_storage.*) |*old_storage_fs| {
                std.mem.copyForwards(u8, new_storage.*.?.At(0), old_storage_fs.At(edl.table_index));
            }
        }
        new_entry.value_ptr.GetComponent(@TypeOf(V), 0).?.* = V;

        // then we simply swap remove this entity out of the table!
        if (self.Tables.getEntry(edl.archetype_hash)) |entry| {
            entry.value_ptr.SwapRemoveEntity(edl.table_index);
        }
    }
    pub fn Print(self: ECSWorld) void {
        for (self.Tables.values()) |*archetype_table| {
            std.debug.print("\n", .{});
            for (&archetype_table.Map) |*maybe_component_storage| {
                if (maybe_component_storage.* != null) {
                    var curr: u64 = 0;
                    while (curr < archetype_table.Size) {
                        std.debug.print("{any}\n", .{maybe_component_storage.*.?.At(curr)});
                        curr += 1;
                    }
                }
            }
        }
    }
};

pub const ArchetypeTable = struct {
    Map: [64]?OpaqueComponentStorage = undefined,
    Size: u64 = undefined,
    archetype_hash: u64 = undefined,
    Capacity: u64 = undefined,
    pub fn Init(self: *ArchetypeTable, capacity: u64) void {
        self.Map = .{null} ** 64;
        self.Capacity = capacity;
        self.Size = 0;
        self.archetype_hash = 0;
    }
    pub fn AddComponentType(self: *ArchetypeTable, allocator: std.mem.Allocator, T: type) void {
        if (self.Map[comptime ComponentIndex(T)]) |component_storage| {
            _ = component_storage;
            return;
        }
        self.Map[comptime ComponentIndex(T)] = OpaqueComponentStorage{};
        self.Map[comptime ComponentIndex(T)].?.Init(T, self.Capacity, allocator);
        self.archetype_hash = self.archetype_hash | comptime ComponentHash(T);
    }
    pub fn SpawnEntity(self: *ArchetypeTable, tuple: anytype) void {
        inline for (tuple) |field| {
            const index = comptime ComponentIndex(@TypeOf(field));
            if (self.Map[index]) |component_storage| {
                const ptr = @as(*@TypeOf(field), @alignCast(@ptrCast(component_storage.array)));
                AddressOffset(@TypeOf(field), ptr, self.Size).* = field;
            } else {
                std.debug.print("ERROR: this archetype table does not have the requeseted component type", .{});
                return;
            }
        }
        self.Size += 1;
    }
    pub fn GetComponent(self: *ArchetypeTable, T: type, row: u64) ?*T {
        const column: u64 = comptime ComponentIndex(T);
        if (self.Map[column]) |*component_storage| {
            return component_storage.GetComponent(T, row);
        }
        std.debug.print("ERROR: Tried get component type that does not exist yet", .{});
        return null;
    }
    pub fn SwapRemoveEntity(self: *ArchetypeTable, row: u64) void {
        for (&self.Map) |*entry| {
            if (entry.*) |*component_storage| {
                const last = component_storage.At(self.Size - 1);
                const curr = component_storage.At(row);
                std.mem.copyForwards(u8, curr, last);
            }
        }
        self.Size -= 1;
    }
};

pub const OpaqueComponentStorage = struct {
    array: []u8 = undefined,
    len: u64 = undefined,
    type_size: u64 = undefined,
    pub fn Init(self: *OpaqueComponentStorage, T: type, len: u64, allocator: std.mem.Allocator) void {
        self.array = allocator.alignedAlloc(u8, @alignOf(T), @sizeOf(T) * len) catch unreachable;
        self.len = len;
        self.type_size = @sizeOf(T);
    }
    pub fn GetComponent(self: *OpaqueComponentStorage, T: type, row: u64) ?*T {
        const ptr = @as(*T, @alignCast(@ptrCast(self.array)));
        return AddressOffset(T, ptr, row);
    }
    pub fn At(self: *OpaqueComponentStorage, row: u64) []u8 {
        return self.array[row * self.type_size .. (row + 1) * self.type_size];
    }
};
