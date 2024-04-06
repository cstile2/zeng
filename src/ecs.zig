const std = @import("std");
const TypeInfo = struct { index: usize, hash: u64, type_size: u64, type_alignment: u8 };
pub const StringHash = [_][]const u8{
    "Transform",
    "Velocity",
    "Mesh",
    "Sine_mover",
    "Ghost",
    "Camera",
};
pub const TypeInfos = [_]TypeInfo{
    .{ .index = 0, .hash = 1 << 0, .type_size = 1, .type_alignment = 0 },
    .{ .index = 1, .hash = 1 << 1, .type_size = 1, .type_alignment = 0 },
    .{ .index = 2, .hash = 1 << 2, .type_size = 3, .type_alignment = 0 },
    .{ .index = 3, .hash = 1 << 3, .type_size = 2, .type_alignment = 0 },
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
pub fn TupleHash(comptime tuple: anytype) u64 {
    comptime var curr_hash = 0;
    for (tuple) |field| {
        curr_hash = curr_hash | comptime ComponentHash(@TypeOf(field));
    }
    return curr_hash;
}
pub fn TupleTypeHash(comptime tuple: anytype) u64 {
    comptime var curr_hash = 0;
    for (tuple) |field| {
        curr_hash = curr_hash | comptime ComponentHash(field);
    }
    return curr_hash;
}
pub const EntityDataLocation = struct {
    row: u64,
    archetype_hash: u64,
};

pub const ECSError = error{
    RequestFailed,
    OutOfBounds,
    RedundantOperation,
};

pub fn Query(data_tuple: anytype, filter_tuple: anytype) type {
    _ = data_tuple; // autofix
    _ = filter_tuple; // autofix
}

pub const QueryIterator = struct {
    _table: *ArchetypeTable,
    _tables: std.AutoArrayHashMap(*ArchetypeTable, void),
    _world: *ECSWorld,
    _tables_index: u64,
    pub fn create(world: *ECSWorld, comptime tuple: anytype) !QueryIterator {
        const hash = comptime TupleTypeHash(tuple);
        var ret: QueryIterator = undefined;
        ret._tables = std.AutoArrayHashMap(*ArchetypeTable, void).init(std.heap.c_allocator);
        ret._world = world;
        ret._tables_index = 0;
        for (world._tables.values()) |*table| {
            if (table.archetype_hash & hash == hash) {
                ret._tables.put(table, void{}) catch unreachable;
            }
        }
        ret._table = ret._tables.keys()[ret._tables_index];
        return ret;
    }
    pub fn next(this: *QueryIterator) bool {
        if (this._tables_index >= this._tables.keys().len) {
            return false;
        }
        this._table = this._tables.keys()[this._tables_index];
        this._tables_index += 1;
        return true;
    }
    pub fn field(this: *QueryIterator, T: type) []T {
        var slice: []T = undefined;
        const field_index = comptime ComponentIndex(T);
        slice.ptr = @ptrCast(this._table.storages[field_index].?.array.ptr);
        slice.len = this._table.count;
        return slice;
    }
};

pub const ECSWorld = struct {
    _tables: std.AutoArrayHashMap(u64, ArchetypeTable),
    _component_dividers: void,
    _allocator: std.mem.Allocator,
    pub fn InitEmptyWorld(self: *ECSWorld, allocator: std.mem.Allocator) void {
        self._allocator = allocator;
        self._tables = std.AutoArrayHashMap(u64, ArchetypeTable).init(allocator);
    }
    pub fn Destroy(self: *ECSWorld) !void {
        for (self._tables.values()) |*value| {
            try value.Destory(self._allocator);
        }
        self._tables.deinit();
    }
    pub fn _GetCreateTableRuntime(self: *ECSWorld, archetype_hash: u64, allocator: std.mem.Allocator) !*ArchetypeTable {
        const table_query = try self._tables.getOrPut(archetype_hash);
        const table = table_query.value_ptr;
        if (!table_query.found_existing) {
            table.InitEmptyTable(128);
            var comphash: u64 = 1;
            var index: u64 = 0;
            while (comphash != 0) {
                if (comphash & archetype_hash != 0) {
                    try table.AddComponentTypeRuntime(allocator, TypeInfos[index]);
                }
                comphash = comphash << 1;
                index += 1;
            }
        }
        return table;
    }
    pub fn SpawnEntity(self: *ECSWorld, tuple: anytype) !EntityDataLocation {
        const tuple_hash = comptime TupleHash(tuple);
        var table = try self._GetCreateTableRuntime(tuple_hash, self._allocator);
        try table.AppendEntityNew(tuple);
        return EntityDataLocation{ .archetype_hash = table.archetype_hash, .row = table.count - 1 };
    }
    pub fn SetComponent(self: *ECSWorld, V: anytype, edl: *EntityDataLocation) !void {
        // calculate the new hash
        const old_hash = edl.archetype_hash;
        const new_hash = (comptime ComponentHash(@TypeOf(V))) | old_hash;

        // test if we stay in same table and exit early
        if (new_hash == old_hash) {
            (try self._tables.getEntry(old_hash).?.value_ptr.GetComponentPtr(@TypeOf(V), edl.row)).* = V;
            return;
        }

        // copy values from old table to new table where the new entity is
        const old_table = self._tables.getEntry(old_hash).?.value_ptr;
        var new_table = try self._GetCreateTableRuntime(new_hash, self._allocator);
        try new_table.AppendEntityCopy(old_table, edl.row);

        // set new value from tuple literal to where the new entity is
        (try new_table.GetComponentPtr(@TypeOf(V), new_table.count - 1)).* = V;

        // swap remove entity from old table
        try old_table.SwapRemoveEntity(edl.row);

        edl.row = new_table.count - 1;
        edl.archetype_hash = new_hash;
    }
    pub fn RemoveComponent(self: *ECSWorld, T: type, edl: *EntityDataLocation) !void {
        // calculate the new hash
        const old_hash = edl.archetype_hash;
        const new_hash = ~(comptime ComponentHash(T)) & old_hash;

        // test if we stay in same table and exit early
        if (new_hash == old_hash) {
            return;
        }

        // copy values from old table to new table where the new entity is
        const old_table = self._tables.getEntry(old_hash).?.value_ptr;
        var new_table = try self._GetCreateTableRuntime(new_hash, self._allocator);
        try new_table.AppendEntityCopy(old_table, edl.row);

        // swap remove entity from old table
        try old_table.SwapRemoveEntity(edl.row);

        edl.row = new_table.count - 1;
        edl.archetype_hash = new_hash;
    }
    pub fn Print(self: ECSWorld) void {
        std.debug.print("=================================", .{});
        for (self._tables.values()) |*archetype_table| {
            std.debug.print("\n--", .{});
            var curr: u64 = 0;
            while (curr < archetype_table.count) {
                defer curr += 1;
                std.debug.print("\n", .{});

                if (archetype_table.archetype_hash == 0) {
                    std.debug.print("*void type*", .{});
                    continue;
                }

                for (&archetype_table.storages) |*maybe_component_storage| {
                    if (maybe_component_storage.* != null) {
                        std.debug.print("*{any}", .{maybe_component_storage.*.?.AtSlice(curr) catch unreachable});
                    }
                }
            }
        }
        std.debug.print("\n\n", .{});
    }
};

pub const ArchetypeTable = struct {
    storages: [64]?OpaqueComponentStorage = undefined,
    count: u64 = undefined,
    archetype_hash: u64 = undefined,
    capacity: u64 = undefined,
    pub fn InitEmptyTable(self: *ArchetypeTable, capacity: u64) void {
        self.storages = .{null} ** 64;
        self.capacity = capacity;
        self.count = 0;
        self.archetype_hash = 0;
    }
    pub fn Destory(self: *ArchetypeTable, allocator: std.mem.Allocator) !void {
        for (self.storages) |maybe_storage| {
            if (maybe_storage) |storage| {
                allocator.free(storage.array);
            }
        }
    }
    pub fn AddComponentTypeRuntime(self: *ArchetypeTable, allocator: std.mem.Allocator, T_run: TypeInfo) !void {
        if (self.storages[T_run.index] != null) {
            return ECSError.RedundantOperation;
        }
        self.storages[T_run.index] = OpaqueComponentStorage{};
        try self.storages[T_run.index].?.InstantiateRuntime(T_run, self.capacity, allocator);
        self.archetype_hash = self.archetype_hash | T_run.hash;
    }
    pub fn AppendEntityNew(self: *ArchetypeTable, tuple: anytype) !void {
        if (self.count >= self.capacity) {
            return ECSError.OutOfBounds;
        }
        inline for (tuple) |field| {
            const column = comptime ComponentIndex(@TypeOf(field));
            (try (self.storages[column] orelse return ECSError.RequestFailed).AtType(self.count, @TypeOf(field))).* = field;
        }
        self.count += 1;
    }
    pub fn AppendEntityCopy(self: *ArchetypeTable, old_table: *ArchetypeTable, old_row: u64) !void {
        if (self.count >= self.capacity) {
            return ECSError.OutOfBounds;
        }
        for (&self.storages, &old_table.storages) |*new_storage, *old_storage| {
            if ((old_storage.* != null) and (new_storage.* != null)) {
                std.mem.copyForwards(u8, try new_storage.*.?.AtSlice(self.count), try old_storage.*.?.AtSlice(old_row));
            }
        }
        self.count += 1;
    }
    pub fn GetComponentPtr(self: *ArchetypeTable, T: type, row: u64) !*T {
        return (self.storages[comptime ComponentIndex(T)] orelse return ECSError.RequestFailed).AtType(row, T);
    }
    pub fn SwapRemoveEntity(self: *ArchetypeTable, row: u64) !void {
        if (row >= self.count) {
            return ECSError.OutOfBounds;
        }
        if (row == self.count - 1) {
            self.count -= 1;
            return;
        }
        for (&self.storages) |*entry| {
            if (entry.*) |*component_storage| {
                const last = try component_storage.AtSlice(self.count - 1);
                const curr = try component_storage.AtSlice(row);
                std.mem.copyForwards(u8, curr, last);
            }
        }
        self.count -= 1;
    }
};

pub const OpaqueComponentStorage = struct {
    array: []u8 = undefined,
    capacity: u64 = undefined,
    type_size: u64 = undefined,
    type_alignment: u8 = undefined,
    pub fn InstantiateRuntime(self: *OpaqueComponentStorage, T_run: TypeInfo, capacity: u64, allocator: std.mem.Allocator) !void {
        self.array = (allocator.vtable.alloc(allocator.ptr, T_run.type_size * capacity, T_run.type_alignment, @returnAddress()) orelse return ECSError.RequestFailed)[0 .. T_run.type_size * capacity];
        self.capacity = capacity;
        self.type_size = T_run.type_size;
        self.type_alignment = T_run.type_alignment;
    }
    pub fn AtSlice(self: *OpaqueComponentStorage, row: u64) ![]u8 {
        if (row >= self.capacity) {
            return ECSError.OutOfBounds;
        }
        return self.array[row * self.type_size .. (row + 1) * self.type_size];
    }
    pub fn AtType(self: *OpaqueComponentStorage, row: u64, T: type) !*T {
        if (row >= self.capacity) {
            return ECSError.OutOfBounds;
        }
        return @as(*T, @ptrFromInt(@intFromPtr(self.array.ptr) + row * self.type_size));
    }
};
