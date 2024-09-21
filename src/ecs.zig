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
        pub const __runtime_type_information = __GENERATE_TYPE_INFOS(__TypeRegistry);
        fn __GENERATE_TYPE_INFOS(comptime TypeReg: anytype) [TypeReg.len]TypeInfo {
            var g: usize = 0;
            var ret: [TypeReg.len]TypeInfo = undefined;
            var curr = 0;
            for (TypeReg) |type_| {
                ret[curr] = TypeInfo{ .hash = GET_COMPONENT_HASH(type_), .type_size = @sizeOf(type_), .type_alignment = std.math.log2(@alignOf(type_)), .component_id = curr };
                curr += 1;
                ret[curr - 1].type_size = @sizeOf(type_);
                if (@sizeOf(type_) == 16) {
                    g = 1;
                }
            }
            return ret;
        }

        // comptime checkers
        fn GET_COMPONENT_HASH(comptime T: type) u64 {
            var curr: u64 = 1;
            for (__TypeRegistry) |type_| {
                if (type_ == T) {
                    return curr;
                }
                curr = curr << 1;
            }
            @compileError("Component type is not valid: " ++ @typeName(T));
        }
        fn GET_COMPONENT_ID(comptime T: type) u64 {
            var curr: u64 = 0;
            for (__TypeRegistry) |type_| {
                if (type_ == T) {
                    return curr;
                }
                curr = curr + 1;
            }
            @compileError("Component type is not valid: " ++ @typeName(T));
        }
        fn GET_COMPONENT_COMBO_HASH(comptime tuple_type: type) u64 {
            comptime var curr_hash = 0;
            inline for (std.meta.fields(tuple_type)) |f| {
                curr_hash = curr_hash | comptime GET_COMPONENT_HASH(f.type);
            }
            return curr_hash;
        }
        fn GET_TYPE_COMBO_HASH(comptime tuple: anytype) u64 {
            comptime var curr_hash = 0;
            inline for (tuple) |field| {
                curr_hash = curr_hash | comptime GET_COMPONENT_HASH(field);
            }
            return curr_hash;
        }

        /// an object to easily let you iterate through all entities that have a specific set of components
        pub const QueryIterator = struct {
            _current_table: ?*ArchetypeTable,
            _relevant_tables: std.AutoArrayHashMap(*ArchetypeTable, void),
            _world: *World,
            _tables_index: u64,
            /// gathers info from ECS world (slow) and creates an iterator to be used for iteration (fast)
            pub fn create(world: *World, comptime tuple_of_types: anytype) !QueryIterator {
                const hash = comptime GET_TYPE_COMBO_HASH(tuple_of_types);
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
                } else {
                    ret._current_table = null;
                }
                return ret;
            }
            /// must be called after the iterator will no longer be used, so that it's memory can be freed
            pub fn destroy(self: *QueryIterator) !void {
                self._relevant_tables.deinit();
            }
            /// finds the next group of entities with the correct components
            pub fn next(this: *QueryIterator) bool {
                if (this._relevant_tables.count() < 1) return false;
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
                const field_index = comptime GET_COMPONENT_ID(T);
                slice.ptr = @alignCast(@ptrCast((this._current_table.?.storages.getEntry(field_index) orelse return ECSError.RequestFailed).value_ptr.array.ptr));
                slice.len = this._current_table.?.count;
                return slice;
            }
            /// retrieves specified components of a single entity, as either pointers or value copies
            pub fn get(self: *QueryIterator, edl: EntityDataLocation, input: anytype) !void {
                try self._world.get_component(edl, input);
            }
        };

        pub fn Query(comptime types: anytype) type {
            return struct {
                _relevant_tables: std.AutoArrayHashMap(ArchetypeID, *ArchetypeTable),
                pub const my_types: @TypeOf(types) = types;

                pub fn create(world: *World, allocator: std.mem.Allocator) !@This() {
                    const minimum_set_hash = comptime GET_TYPE_COMBO_HASH(types);
                    var ret: @This() = undefined;
                    ret._relevant_tables = std.AutoArrayHashMap(ArchetypeID, *ArchetypeTable).init(allocator);
                    for (world._tables.values()) |*table| {
                        if (table.archetype_hash & minimum_set_hash == minimum_set_hash) {
                            ret._relevant_tables.put(table.archetype_hash, table) catch unreachable;
                        }
                    }
                    return ret;
                }
                pub fn destroy(self: *@This()) !void {
                    self._relevant_tables.deinit();
                }
                pub fn pointer_fetch(self: @This(), edl: EntityDataLocation, input: anytype) !void {
                    inline for (input) |*member| {
                        member.* = try self.get_component(edl, @TypeOf(member.*.*));
                    }
                }
                pub fn get_component(self: @This(), edl: EntityDataLocation, T: type) !*T {
                    return try self._relevant_tables.getEntry(edl.archetype_hash).?.value_ptr.*.entry_ptr(T, edl.row);
                }
            };
        }

        /// Contains all entites for an ECS system and is needed to use the ECS
        pub const World = struct {
            _tables: std.AutoArrayHashMap(ArchetypeID, ArchetypeTable),
            _allocator: std.mem.Allocator,
            /// initializes the ECS world - required for use
            pub fn init(allocator: std.mem.Allocator) World {
                return .{
                    ._allocator = allocator,
                    ._tables = std.AutoArrayHashMap(ArchetypeID, ArchetypeTable).init(allocator),
                };
            }
            /// deallocates all memory created within this world
            pub fn deinit(self: *World) !void {
                for (self._tables.values()) |*table| {
                    try table.deinit();
                }
                self._tables.deinit();
            }
            /// internal helper function - retrieve an archetype table and create one if none exists
            pub fn _get_create_table_by_id(self: *World, archetype_id: ArchetypeID, allocator: std.mem.Allocator) !*ArchetypeTable {
                const table_get_put = try self._tables.getOrPut(archetype_id);
                if (table_get_put.found_existing) return table_get_put.value_ptr;

                const table = table_get_put.value_ptr;
                table.init(128, allocator);
                var curr_bit_field: u64 = 1;
                var index: u64 = 0;
                while (curr_bit_field != 0 and index < __runtime_type_information.len) {
                    if (curr_bit_field & archetype_id != 0) {
                        try table.add_component_type_id(allocator, __runtime_type_information[index]);
                    }
                    curr_bit_field = curr_bit_field << 1;
                    index += 1;
                }
                return table;
            }
            /// spawn an entity with component values specified in a tuple
            pub fn spawn(self: *World, tuple: anytype) !EntityDataLocation {
                const tuple_hash = comptime GET_COMPONENT_COMBO_HASH(@TypeOf(tuple));
                var table = try self._get_create_table_by_id(tuple_hash, self._allocator);
                try table.insert_entity_tuple(tuple);
                return EntityDataLocation{ .archetype_hash = table.archetype_hash, .row = table.count - 1 };
            }
            /// set the value of or add a new component of specified type and value
            pub fn insert_component(self: *World, V: anytype, edl: *EntityDataLocation) !void {
                // calculate the new hash
                const old_hash = edl.archetype_hash;
                const new_hash = (comptime GET_COMPONENT_HASH(@TypeOf(V))) | old_hash;

                // test if we stay in same table and exit early
                if (new_hash == old_hash) {
                    (try self._tables.getEntry(old_hash).?.value_ptr.entry_ptr(@TypeOf(V), edl.row)).* = V;
                    return;
                }

                // copy values from old table to new table where the new entity is
                const old_table = self._tables.getEntry(old_hash).?.value_ptr;
                var new_table = try self._get_create_table_by_id(new_hash, self._allocator);
                try new_table.insert_entity_copy(old_table, edl.row);

                // set new value from tuple literal to where the new entity is
                (try new_table.entry_ptr(@TypeOf(V), new_table.count - 1)).* = V;

                // swap remove entity from old table
                try old_table.swap_remove_entity(edl.row);

                // update edl
                edl.row = new_table.count - 1;
                edl.archetype_hash = new_hash;
            }
            /// retrieve a specific component or set of components as pointers or copies
            pub fn get_component(self: *World, edl: EntityDataLocation, input: anytype) !void {
                switch (@typeInfo(@TypeOf(input))) {
                    .Struct => |stru| {
                        if (stru.is_tuple) {
                            inline for (&input) |member| {
                                if (@typeInfo(@TypeOf(member.*)) == .Pointer) {
                                    member.* = try (self._tables.getEntry(edl.archetype_hash) orelse return ECSError.RequestFailed).value_ptr.entry_ptr(std.meta.Child(@TypeOf(member.*)), edl.row);
                                } else {
                                    member.* = (try (self._tables.getEntry(edl.archetype_hash) orelse return ECSError.RequestFailed).value_ptr.entry_ptr(@TypeOf(member.*), edl.row)).*;
                                }
                            }
                        } else {
                            @compileError("Invalid parameter: Must be a (*)struct, or a tuple of (*)structs");
                        }
                    },
                    .Pointer => {
                        if (@typeInfo(@TypeOf(input.*)) == .Pointer) {
                            input.* = try (self._tables.getEntry(edl.archetype_hash) orelse return ECSError.RequestFailed).value_ptr.entry_ptr(std.meta.Child(@TypeOf(input.*)), edl.row);
                        } else {
                            input.* = (try (self._tables.getEntry(edl.archetype_hash) orelse return ECSError.RequestFailed).value_ptr.entry_ptr(@TypeOf(input.*), edl.row)).*;
                        }
                    },
                    else => {
                        @compileError("Invalid parameter: Must be a (*)struct, or a tuple of (*)structs");
                    },
                }
            }
            /// removes a component if that component type is on the specified entity
            pub fn remove_component(self: *World, T: type, edl: *EntityDataLocation) !void {
                // calculate the new hash
                const old_hash = edl.archetype_hash;
                const new_hash = ~(comptime GET_COMPONENT_HASH(T)) & old_hash;

                // test if we stay in same table and exit early
                if (new_hash == old_hash) return;

                // copy values from old table to new table where the new entity is
                const old_table = self._tables.getEntry(old_hash).?.value_ptr;
                var new_table = try self._get_create_table_by_id(new_hash, self._allocator);
                try new_table.insert_entity_copy(old_table, edl.row);

                // swap remove entity from old table
                try old_table.swap_remove_entity(edl.row);

                // update edl
                edl.row = new_table.count - 1;
                edl.archetype_hash = new_hash;
            }
            /// print world information
            pub fn _print(self: World) void {
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
                                std.debug.print("*{any}", .{maybe_component_storage.*.?.entry_bytes(curr) catch unreachable});
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
            pub fn init(self: *ArchetypeTable, capacity: u64, allocator: std.mem.Allocator) void {
                self.allocator = allocator;
                self.archetype_hash = 0;
                self.storages = std.AutoArrayHashMap(ComponentID, ComponentColumn).init(allocator);
                self.capacity = capacity;
                self.count = 0;
                self.next = null;
            }
            pub fn deinit(self: *ArchetypeTable) !void {
                for (self.storages.values()) |*component_storage| {
                    try component_storage.deinit(self.allocator);
                }
                self.storages.deinit();
            }
            pub fn add_component_type_id(self: *ArchetypeTable, allocator: std.mem.Allocator, T_run: TypeInfo) !void {
                const get_put = try self.storages.getOrPut(T_run.component_id);
                if (get_put.found_existing) return ECSError.RedundantOperation;
                try get_put.value_ptr.init(T_run, self.capacity, allocator);
                self.archetype_hash = self.archetype_hash | T_run.hash;
            }
            pub fn _maybe_double_capacity(self: *ArchetypeTable) !void {
                if (self.count >= self.capacity) {
                    for (self.storages.values()) |*component_storage| {
                        try component_storage.double_size(self.allocator);
                    }
                    self.capacity *= 2;
                }
            }
            pub fn insert_entity_tuple(self: *ArchetypeTable, tuple: anytype) !void {
                try self._maybe_double_capacity();
                inline for (tuple) |field| {
                    (try (self.storages.getEntry(comptime GET_COMPONENT_ID(@TypeOf(field))) orelse return ECSError.RequestFailed).value_ptr.entry_ptr(self.count, @TypeOf(field))).* = field;
                }
                self.count += 1;
            }
            pub fn insert_entity_copy(self: *ArchetypeTable, old_table: *ArchetypeTable, old_row: u64) !void {
                try self._maybe_double_capacity();
                for (self.storages.values()) |*new_storage| {
                    for (old_table.storages.values()) |*old_storage| {
                        if (std.meta.eql(old_storage.type_info, new_storage.type_info)) {
                            const old = try old_storage.entry_bytes(old_row);
                            const new = try new_storage.entry_bytes(self.count);
                            @memcpy(new, old);
                        }
                    }
                }
                self.count += 1;
            }
            pub fn entry_ptr(self: *ArchetypeTable, T: type, row: u64) !*T {
                return (try self.storages.getEntry(comptime GET_COMPONENT_ID(T)).?.value_ptr.entry_ptr(row, T));
            }
            pub fn swap_remove_entity(self: *ArchetypeTable, row: u64) !void {
                if (row >= self.count) {
                    return ECSError.OutOfBounds;
                }
                if (row == self.count - 1) {
                    self.count -= 1;
                    return;
                }
                for (self.storages.values()) |*component_storage| {
                    const bottom = try component_storage.entry_bytes(self.count - 1);
                    const upper = try component_storage.entry_bytes(row);
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
            pub fn init(self: *ComponentColumn, T_run: TypeInfo, capacity: u64, allocator: std.mem.Allocator) !void {
                self.capacity = capacity;
                self.type_info = T_run;
                if (self.type_info.type_size == 0) return;
                self.array = (allocator.vtable.alloc(allocator.ptr, T_run.type_size * capacity, T_run.type_alignment, @returnAddress()) orelse return ECSError.RequestFailed)[0 .. T_run.type_size * capacity];
            }
            pub fn deinit(self: *ComponentColumn, allocator: std.mem.Allocator) !void {
                if (self.type_info.type_size == 0) return;
                allocator.vtable.free(allocator.ptr, self.array, self.type_info.type_alignment, @returnAddress());
            }
            pub fn double_size(self: *ComponentColumn, allocator: std.mem.Allocator) !void {
                self.capacity *= 2;
                if (self.type_info.type_size == 0) return;
                const temp = (allocator.vtable.alloc(allocator.ptr, self.type_info.type_size * self.capacity * 2, self.type_info.type_alignment, @returnAddress()) orelse return ECSError.RequestFailed)[0 .. self.type_info.type_size * self.capacity * 2];
                @memcpy(temp[0..self.array.len], self.array);
                allocator.vtable.free(allocator.ptr, self.array, self.type_info.type_alignment, @returnAddress());
                self.array = temp;
            }
            pub fn entry_bytes(self: *ComponentColumn, row: u64) ![]u8 {
                if (row >= self.capacity) {
                    return ECSError.OutOfBounds;
                }
                return self.array[row * self.type_info.type_size .. (row + 1) * self.type_info.type_size];
            }
            pub fn entry_ptr(self: *ComponentColumn, row: u64, T: type) !*T {
                if (row >= self.capacity) {
                    return ECSError.OutOfBounds;
                }
                return @as(*T, @ptrFromInt(@intFromPtr(self.array.ptr) + row * self.type_info.type_size));
            }
        };
    };
}
