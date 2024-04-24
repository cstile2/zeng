const std = @import("std");
pub const TypeInfo = struct {
    hash: u64,
    type_size: u64,
    type_alignment: u8,
    component_id: ComponentID,
};
pub const EntityDataLocation = struct {
    row: u64,
    archetype_hash: u64,
};
const ComponentID = u64;
const ArchetypeID = u64;
const ECSError = error{
    RequestFailed,
    OutOfBounds,
    RedundantOperation,
    NonexistentComponent,
};

// a struct that is available to all systems
pub fn Resource(comptime T: type) type {
    return struct {
        t: T,
    };
}

pub fn CompileECS(comptime __TypeRegistry: anytype) type {
    return struct {
        // integrate custom type-checking
        const __RuntimeTypeInformation = __GenerateTypeInfos(__TypeRegistry);
        fn __GenerateTypeInfos(comptime TypeReg: anytype) [TypeReg.len]TypeInfo {
            var ret: [TypeReg.len]TypeInfo = undefined;
            var curr = 0;
            for (TypeReg) |type_| {
                ret[curr] = TypeInfo{ .hash = GetComponentHash(type_), .type_size = @sizeOf(type_), .type_alignment = std.math.log2(@alignOf(type_)), .component_id = curr };
                curr += 1;
            }
            return ret;
        }

        // comptime checkers
        fn GetComponentHash(comptime T: type) u64 {
            var curr: u64 = 1;
            for (__TypeRegistry) |type_| {
                if (type_ == T) {
                    return curr;
                }
                curr = curr << 1;
            }
            @compileError("Component type is not valid: " ++ @typeName(T));
        }
        fn GetComponentID(comptime T: type) u64 {
            var curr: u64 = 0;
            for (__TypeRegistry) |type_| {
                if (type_ == T) {
                    return curr;
                }
                curr = curr + 1;
            }
            @compileError("Component type is not valid: " ++ @typeName(T));
        }
        fn GetCombinedComponentHash(comptime tuple_type: type) u64 {
            comptime var curr_hash = 0;
            inline for (std.meta.fields(tuple_type)) |f| {
                curr_hash = curr_hash | comptime GetComponentHash(f.type);
            }
            return curr_hash;
        }
        fn GetCombinedTypeHash(comptime tuple: anytype) u64 {
            comptime var curr_hash = 0;
            for (tuple) |field| {
                curr_hash = curr_hash | comptime GetComponentHash(field);
            }
            return curr_hash;
        }

        /// an object to easily let you iterate through all entities that have a specific set of components
        pub const QueryIterator = struct {
            _current_table: *ArchetypeTable,
            _relevant_tables: std.AutoArrayHashMap(*ArchetypeTable, void),
            _world: *ECSWorld,
            _tables_index: u64,
            /// gathers info from ECS world (slow) and creates an iterator to be used for iteration (fast)
            pub fn create(world: *ECSWorld, comptime tuple_of_types: anytype) !QueryIterator {
                const hash = comptime GetCombinedTypeHash(tuple_of_types);
                var ret: QueryIterator = undefined;
                ret._relevant_tables = std.AutoArrayHashMap(*ArchetypeTable, void).init(world._allocator);
                ret._world = world;
                ret._tables_index = 0;
                for (world._tables.values()) |*table| {
                    if (table.archetype_hash & hash == hash) {
                        ret._relevant_tables.put(table, void{}) catch unreachable;
                    }
                }
                if (ret._relevant_tables.count() > 0) {
                    ret._current_table = ret._relevant_tables.keys()[0];
                }
                return ret;
            }
            /// must be called after the iterator will no longer be used, so that it's memory can be freed
            pub fn destroy(self: *QueryIterator) !void {
                self._relevant_tables.deinit();
            }
            /// finds the next group of entities with the correct components
            pub fn next(this: *QueryIterator) bool {
                if (this._tables_index >= this._relevant_tables.keys().len) {
                    return false;
                }
                this._current_table = this._relevant_tables.keys()[this._tables_index];
                this._tables_index += 1;
                return true;
            }
            /// faster than creating a new iterator if you need to do another pass on the same entities
            pub fn reset(self: *QueryIterator) void {
                self._tables_index = 0;
                self._current_table = self._relevant_tables.keys()[0];
            }
            /// retrieves the array of a component type for a group of entities
            pub fn field(this: *QueryIterator, T: type) ![]T {
                var slice: []T = undefined;
                const field_index = comptime GetComponentID(T);
                slice.ptr = @alignCast(@ptrCast((this._current_table.storages.getEntry(field_index) orelse return ECSError.RequestFailed).value_ptr.array.ptr));
                slice.len = this._current_table.count;
                return slice;
            }
            /// retrieves specified components of a single entity, as either pointers or value copies
            pub fn get(self: *QueryIterator, edl: EntityDataLocation, input: anytype) !void {
                try self._world.Get(edl, input);
            }
        };

        /// Contains all entites for an ECS system and is needed to use the ECS
        pub const ECSWorld = struct {
            _tables: std.AutoArrayHashMap(ArchetypeID, ArchetypeTable),
            _allocator: std.mem.Allocator,
            /// initializes the ECS world - required for use
            pub fn InitEmptyWorld(self: *ECSWorld, allocator: std.mem.Allocator) void {
                self._allocator = allocator;
                self._tables = std.AutoArrayHashMap(ArchetypeID, ArchetypeTable).init(allocator);
            }
            /// deallocates all memory created within this world
            pub fn Destroy(self: *ECSWorld) !void {
                for (self._tables.values()) |*table| {
                    try table.Destroy();
                }
                self._tables.deinit();
            }
            /// internal helper function - retrieve an archetype table and create one if none exists
            pub fn _GetCreateTableRuntime(self: *ECSWorld, archetype_id: ArchetypeID, allocator: std.mem.Allocator) !*ArchetypeTable {
                const table_get_put = try self._tables.getOrPut(archetype_id);
                if (table_get_put.found_existing) return table_get_put.value_ptr;

                const table = table_get_put.value_ptr;
                table.InitEmptyTable(128, allocator);
                var curr_bit_field: u64 = 1;
                var index: u64 = 0;
                while (curr_bit_field != 0 and index < __RuntimeTypeInformation.len) {
                    if (curr_bit_field & archetype_id != 0) {
                        try table.AddComponentTypeRuntime(allocator, (&__RuntimeTypeInformation[index]).*);
                    }
                    curr_bit_field = curr_bit_field << 1;
                    index += 1;
                }
                return table;
            }
            /// spawn an entity with component values specified in a tuple
            pub fn SpawnEntity(self: *ECSWorld, tuple: anytype) !EntityDataLocation {
                const tuple_hash = comptime GetCombinedComponentHash(@TypeOf(tuple));
                var table = try self._GetCreateTableRuntime(tuple_hash, self._allocator);
                try table.AppendEntityNew(tuple);
                return EntityDataLocation{ .archetype_hash = table.archetype_hash, .row = table.count - 1 };
            }
            /// set the value of or add a new component of specified type and value
            pub fn SetComponent(self: *ECSWorld, V: anytype, edl: *EntityDataLocation) !void {
                // calculate the new hash
                const old_hash = edl.archetype_hash;
                const new_hash = (comptime GetComponentHash(@TypeOf(V))) | old_hash;

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

                // update edl
                edl.row = new_table.count - 1;
                edl.archetype_hash = new_hash;
            }
            /// retrieve a specific component or set of components as pointers or copies
            pub fn Get(self: *ECSWorld, edl: EntityDataLocation, input: anytype) !void {
                switch (@typeInfo(@TypeOf(input))) {
                    .Struct => |stru| {
                        if (stru.is_tuple) {
                            inline for (&input) |member| {
                                if (@typeInfo(@TypeOf(member.*)) == .Pointer) {
                                    member.* = try (self._tables.getEntry(edl.archetype_hash) orelse return ECSError.RequestFailed).value_ptr.GetComponentPtr(std.meta.Child(@TypeOf(member.*)), edl.row);
                                } else {
                                    member.* = (try (self._tables.getEntry(edl.archetype_hash) orelse return ECSError.RequestFailed).value_ptr.GetComponentPtr(@TypeOf(member.*), edl.row)).*;
                                }
                            }
                        } else {
                            @compileError("Invalid parameter: Must be a (*)struct, or a tuple of (*)structs");
                        }
                    },
                    .Pointer => {
                        if (@typeInfo(@TypeOf(input.*)) == .Pointer) {
                            input.* = try (self._tables.getEntry(edl.archetype_hash) orelse return ECSError.RequestFailed).value_ptr.GetComponentPtr(std.meta.Child(@TypeOf(input.*)), edl.row);
                        } else {
                            input.* = (try (self._tables.getEntry(edl.archetype_hash) orelse return ECSError.RequestFailed).value_ptr.GetComponentPtr(@TypeOf(input.*), edl.row)).*;
                        }
                    },
                    else => {
                        @compileError("Invalid parameter: Must be a (*)struct, or a tuple of (*)structs");
                    },
                }
            }
            /// removes a component if that component type is on the specified entity
            pub fn RemoveComponent(self: *ECSWorld, T: type, edl: *EntityDataLocation) !void {
                // calculate the new hash
                const old_hash = edl.archetype_hash;
                const new_hash = ~(comptime GetComponentHash(T)) & old_hash;

                // test if we stay in same table and exit early
                if (new_hash == old_hash) return;

                // copy values from old table to new table where the new entity is
                const old_table = self._tables.getEntry(old_hash).?.value_ptr;
                var new_table = try self._GetCreateTableRuntime(new_hash, self._allocator);
                try new_table.AppendEntityCopy(old_table, edl.row);

                // swap remove entity from old table
                try old_table.SwapRemoveEntity(edl.row);

                // update edl
                edl.row = new_table.count - 1;
                edl.archetype_hash = new_hash;
            }
            /// print world information
            pub fn _Print(self: ECSWorld) void {
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
                                std.debug.print("*{any}", .{maybe_component_storage.*.?.ElementAsByteSlice(curr) catch unreachable});
                            }
                        }
                    }
                }
                std.debug.print("\n\n", .{});
            }
        };

        /// holds all of the component storage objects for a given archetype of an entity - allows for simple, fast iteration on arrays
        pub const ArchetypeTable = struct {
            archetype_hash: u64 = 0,
            storages: std.AutoArrayHashMap(ComponentID, ComponentColumn),
            capacity: u64 = 0,
            count: u64 = 0,
            next: ?*ArchetypeTable = null,
            allocator: std.mem.Allocator,
            pub fn InitEmptyTable(self: *ArchetypeTable, capacity: u64, allocator: std.mem.Allocator) void {
                self.allocator = allocator;
                self.archetype_hash = 0;
                self.storages = std.AutoArrayHashMap(ComponentID, ComponentColumn).init(allocator);
                self.capacity = capacity;
                self.count = 0;
                self.next = null;
            }
            pub fn Destroy(self: *ArchetypeTable) !void {
                for (self.storages.values()) |*component_storage| {
                    try component_storage.Destroy(self.allocator);
                }
                self.storages.deinit();
            }
            pub fn AddComponentTypeRuntime(self: *ArchetypeTable, allocator: std.mem.Allocator, T_run: TypeInfo) !void {
                const get_put = try self.storages.getOrPut(T_run.component_id);
                if (get_put.found_existing) return ECSError.RedundantOperation;

                try get_put.value_ptr.InstantiateRuntime(T_run, self.capacity, allocator);
                self.archetype_hash = self.archetype_hash | T_run.hash;
            }
            pub fn _MaybeDoubleSize(self: *ArchetypeTable) !void {
                if (self.count >= self.capacity) {
                    for (self.storages.values()) |*component_storage| {
                        try component_storage.DoubleSize(self.allocator);
                    }
                    self.capacity *= 2;
                }
            }
            pub fn AppendEntityNew(self: *ArchetypeTable, tuple: anytype) !void {
                try self._MaybeDoubleSize();
                inline for (tuple) |field| {
                    (try (self.storages.getEntry(comptime GetComponentID(@TypeOf(field))) orelse return ECSError.RequestFailed).value_ptr.ElementAsTypePtr(self.count, @TypeOf(field))).* = field;
                }
                self.count += 1;
            }
            pub fn AppendEntityCopy(self: *ArchetypeTable, old_table: *ArchetypeTable, old_row: u64) !void {
                try self._MaybeDoubleSize();
                for (self.storages.values()) |*new_storage| {
                    for (old_table.storages.values()) |*old_storage| {
                        if (std.meta.eql(old_storage.type_info, new_storage.type_info)) {
                            const old = try old_storage.ElementAsByteSlice(old_row);
                            const new = try new_storage.ElementAsByteSlice(self.count);
                            @memcpy(new, old);
                        }
                    }
                }
                self.count += 1;
            }
            pub fn GetComponentPtr(self: *ArchetypeTable, T: type, row: u64) !*T {
                return (try self.storages.getEntry(comptime GetComponentID(T)).?.value_ptr.ElementAsTypePtr(row, T));
            }
            pub fn SwapRemoveEntity(self: *ArchetypeTable, row: u64) !void {
                if (row >= self.count) {
                    return ECSError.OutOfBounds;
                }
                if (row == self.count - 1) {
                    self.count -= 1;
                    return;
                }
                for (self.storages.values()) |*component_storage| {
                    const bottom = try component_storage.ElementAsByteSlice(self.count - 1);
                    const upper = try component_storage.ElementAsByteSlice(row);
                    @memcpy(upper, bottom);
                }
                self.count -= 1;
            }
        };

        /// this object is essentially just a pointer to a dynamically allocated array of a singular component type
        pub const ComponentColumn = struct {
            array: []u8 = undefined,
            capacity: u64 = undefined,
            type_info: TypeInfo,
            pub fn InstantiateRuntime(self: *ComponentColumn, T_run: TypeInfo, capacity: u64, allocator: std.mem.Allocator) !void {
                self.capacity = capacity;
                self.type_info = T_run;
                if (self.type_info.type_size == 0) return;
                self.array = (allocator.vtable.alloc(allocator.ptr, T_run.type_size * capacity, T_run.type_alignment, @returnAddress()) orelse return ECSError.RequestFailed)[0 .. T_run.type_size * capacity];
            }
            pub fn Destroy(self: *ComponentColumn, allocator: std.mem.Allocator) !void {
                if (self.type_info.type_size == 0) return;
                allocator.vtable.free(allocator.ptr, self.array, self.type_info.type_alignment, @returnAddress());
            }
            pub fn DoubleSize(self: *ComponentColumn, allocator: std.mem.Allocator) !void {
                self.capacity *= 2;
                if (self.type_info.type_size == 0) return;
                const temp = (allocator.vtable.alloc(allocator.ptr, self.type_info.type_size * self.capacity * 2, self.type_info.type_alignment, @returnAddress()) orelse return ECSError.RequestFailed)[0 .. self.type_info.type_size * self.capacity * 2];
                @memcpy(temp[0..self.array.len], self.array);
                allocator.vtable.free(allocator.ptr, self.array, self.type_info.type_alignment, @returnAddress());
                self.array = temp;
            }
            pub fn ElementAsByteSlice(self: *ComponentColumn, row: u64) ![]u8 {
                if (row >= self.capacity) {
                    return ECSError.OutOfBounds;
                }
                return self.array[row * self.type_info.type_size .. (row + 1) * self.type_info.type_size];
            }
            pub fn ElementAsTypePtr(self: *ComponentColumn, row: u64, T: type) !*T {
                if (row >= self.capacity) {
                    return ECSError.OutOfBounds;
                }
                return @as(*T, @ptrFromInt(@intFromPtr(self.array.ptr) + row * self.type_info.type_size));
            }
        };
    };
}
