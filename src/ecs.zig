const std = @import("std");
const utils = @import("utils.zig");
pub const runtime_type_info = struct {
    hash: u64,
    type_size: u64,
    type_alignment: u8,
    component_id: component_id,
};
pub const unstable_entity_location = struct {
    row: u64,
    archetype_hash: u64,
};

const component_id = u64;
const archetype_id = u64;
const ECSError = error{
    RequestFailed,
    OutOfBounds,
    RedundantOperation,
    NonexistentComponent,
};

pub fn CompileECS(comptime __TypeRegistry: anytype) type {
    return struct {
        // integrate custom type-checking
        pub var __runtime_type_information = __GENERATE_TYPE_INFOS(__TypeRegistry);
        fn __GENERATE_TYPE_INFOS(comptime TypeReg: anytype) [TypeReg.len]runtime_type_info {
            var ret: [TypeReg.len]runtime_type_info = undefined;
            var curr = 0;
            for (TypeReg) |type_| {
                ret[curr] = runtime_type_info{ .hash = GET_COMPONENT_HASH(type_), .type_size = @sizeOf(type_), .type_alignment = std.math.log2(@alignOf(type_)), .component_id = curr };
                if (ret[curr].type_size != @sizeOf(type_) or (ret[curr].type_size > 1024)) @compileError("blubber!: " ++ std.fmt.comptimePrint("{} {}", .{ @sizeOf(type_), ret[curr].type_size }));
                curr += 1;
            }
            return ret;
        }

        // comptime checkers
        pub fn GET_COMPONENT_HASH(comptime T: type) u64 {
            var curr: u64 = 1;
            for (__TypeRegistry) |type_| {
                if (type_ == T) {
                    return curr;
                }
                curr = curr << 1;
            }
            @compileError("Component type is not valid: " ++ @typeName(T));
        }
        pub fn GET_COMPONENT_ID(comptime T: type) u64 {
            var curr: u64 = 0;
            for (__TypeRegistry) |type_| {
                if (type_ == T) {
                    return curr;
                }
                curr = curr + 1;
            }
            @compileError("Component type is not valid: " ++ @typeName(T));
        }
        pub fn GET_COMPONENT_COMBO_HASH(comptime tuple_type: type) u64 {
            comptime var curr_hash = 0;
            inline for (std.meta.fields(tuple_type)) |f| {
                curr_hash = curr_hash | comptime GET_COMPONENT_HASH(f.type);
            }
            return curr_hash;
        }
        pub fn GET_TYPE_COMBO_HASH(comptime tuple: anytype) u64 {
            comptime var curr_hash = 0;
            inline for (tuple) |field| {
                curr_hash = curr_hash | comptime GET_COMPONENT_HASH(field);
            }
            return curr_hash;
        }

        pub const entity_id = u64;

        pub const entity_locations = [16384]unstable_entity_location;

        pub fn query(comptime types: anytype) type {
            return struct {
                pub const TYPES: @TypeOf(types) = types;
                _relevant_tables: std.AutoArrayHashMap(archetype_id, *const archetype_table),
                components: std.ArrayList([types.len]component_column), // make columns faster during iteration
                _id_lookup: []const unstable_entity_location,

                pub fn create(w: *world, allocator: std.mem.Allocator) !@This() {
                    const minimum_set_hash = comptime GET_TYPE_COMBO_HASH(types);
                    var ret: @This() = undefined;
                    ret._relevant_tables = std.AutoArrayHashMap(archetype_id, *const archetype_table).init(allocator);
                    ret.components = std.ArrayList([types.len]component_column).initCapacity(allocator, types.len) catch unreachable;
                    ret._id_lookup = w._locations[0..];
                    for (w._tables.values()) |*table| {
                        if (table.archetype_hash & minimum_set_hash == minimum_set_hash) {
                            ret._relevant_tables.put(table.archetype_hash, table) catch unreachable;

                            ret.components.append(undefined) catch unreachable;
                            inline for (comptime 0..types.len) |i| {
                                _ = table.entry_ptr(types[i], 0).?;
                                ret.components.items[ret.components.items.len - 1][i] = table.storages.get(comptime GET_COMPONENT_ID(types[i])).?;
                            }
                        }
                    }
                    return ret;
                }
                pub fn destroy(self: *@This()) !void {
                    self._relevant_tables.deinit();
                }
                pub fn pointer_fetch(self: @This(), edl: unstable_entity_location, input: anytype) !void {
                    inline for (input) |*member| {
                        member.* = try self.get_component(edl, @TypeOf(member.*.*));
                    }
                }
                pub fn get_component(self: @This(), edl: unstable_entity_location, T: type) !*T {
                    return try self._relevant_tables.getEntry(edl.archetype_hash).?.value_ptr.*.entry_ptr(T, edl.row);
                }
                pub fn get_maybe(self: @This(), entity: entity_id, T: type) ?*T {
                    const edl = self._id_lookup[entity];
                    const result = self._relevant_tables.getEntry(edl.archetype_hash);
                    if (result) |r| {
                        return r.value_ptr.*.entry_ptr(T, edl.row);
                    } else return null;
                }
            };
        }

        /// Contains all entites for an ECS system and is needed to use the ECS
        pub const world = struct {
            _tables: std.AutoArrayHashMap(archetype_id, archetype_table),
            _allocator: std.mem.Allocator,
            _count: entity_id = 0,
            _locations: entity_locations = undefined,
            /// initializes the ECS world - required for use
            pub fn init(allocator: std.mem.Allocator) world {
                return .{
                    ._allocator = allocator,
                    ._tables = std.AutoArrayHashMap(archetype_id, archetype_table).init(allocator),
                };
            }
            /// deallocates all memory created within this world
            pub fn deinit(self: *world) !void {
                for (self._tables.values()) |*table| {
                    try table.deinit();
                }
                self._tables.deinit();
                // self._entity_to_location.deinit();
                // self._location_to_entity.deinit();
            }
            /// spawn an entity with component values specified in a tuple
            pub fn spawn(self: *world, tuple: anytype) !entity_id {
                const tuple_hash = comptime GET_COMPONENT_COMBO_HASH(@TypeOf(tuple));
                var table = try self._request_table(tuple_hash, self._allocator);
                table.insert_components_comptime(tuple) catch unreachable;

                self._locations[self._count] = unstable_entity_location{ .archetype_hash = table.archetype_hash, .row = table.count - 1 };
                table.row_to_public_ids[table.count - 1] = self._count;
                self._count += 1;

                return self._count - 1;
            }
            /// spawn an entity with component values specified as types and pointers
            pub fn spawn_runtime(self: *world, ts: []runtime_type_info, ptrs: [][*]u8) !entity_id {
                var tuple_hash = 0;
                for (ts) |t| {
                    tuple_hash = tuple_hash | t.hash;
                }
                var table = try self._request_table(tuple_hash, self._allocator);
                table.insert_components_runtime(ts, ptrs) catch unreachable;

                self._locations[self._count] = unstable_entity_location{ .archetype_hash = table.archetype_hash, .row = table.count - 1 };
                table.row_to_public_ids[table.count - 1] = self._count;
                self._count += 1;

                return self._count - 1;
            }
            /// set the value of or add a new component of specified type and value
            pub fn insert(self: *world, V: anytype, edl: *unstable_entity_location) !void {
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
                var new_table = try self._request_table(new_hash, self._allocator);
                try new_table.insert_existing(old_table, edl.row);

                const public_id = old_table.ptrs_to_public_ids[edl.row];
                self._locations[public_id] = unstable_entity_location{ .archetype_hash = new_hash, .row = new_table.count - 1 };
                new_table.row_to_public_ids[new_table.count - 1] = public_id;

                // set new value from tuple literal to where the new entity is
                (try new_table.entry_ptr(@TypeOf(V), new_table.count - 1)).* = V;

                // swap remove entity from old table
                try old_table.swap_remove_entity(edl.row, self);

                // update edl
                edl.row = new_table.count - 1;
                edl.archetype_hash = new_hash;
            }
            /// set the value of or add a new component of specified type and value - runtime
            pub fn insert_runtime(self: *world, t: runtime_type_info, ptr: [*]u8, edl: unstable_entity_location) !void {
                const old_edl = unstable_entity_location{ .row = edl.row, .archetype_hash = edl.archetype_hash };

                // calculate the new hash
                const old_hash = old_edl.archetype_hash;
                const new_hash = t.hash | old_hash;

                // test if we stay in same table and exit early
                if (new_hash == old_hash) {
                    @memcpy(try self._tables.getEntry(old_hash).?.value_ptr.entry_ptr_runtime(t.component_id, old_edl.row), ptr[0..t.type_size]);
                    return;
                }

                // copy values from old table to new table where the new entity is
                const old_table = self._tables.getEntry(old_hash).?.value_ptr;
                var new_table = try self._request_table(new_hash, self._allocator);
                try new_table.insert_existing(old_table, old_edl.row);

                const public_id = old_table.row_to_public_ids[old_edl.row];
                self._locations[public_id] = unstable_entity_location{ .archetype_hash = new_hash, .row = new_table.count - 1 };
                new_table.row_to_public_ids[new_table.count - 1] = public_id;

                // set new value from tuple literal to where the new entity is
                @memcpy(try new_table.entry_ptr_runtime(t.component_id, new_table.count - 1), ptr[0..t.type_size]);

                // swap remove entity from old table
                try old_table.swap_remove_entity(self, old_edl.row);
            }
            /// retrieve references to components of an entity
            pub fn get(self: *world, edl: unstable_entity_location, Ts: anytype) !utils.tuple_of_ptrs(Ts) {
                var ret: utils.tuple_of_ptrs(Ts) = undefined;
                inline for (&ret) |*member| {
                    member.* = (self._tables.getEntry(edl.archetype_hash) orelse return ECSError.RequestFailed).value_ptr.entry_ptr(@TypeOf(member.*.*), edl.row) orelse return ECSError.RequestFailed;
                }
                return ret;
            }
            /// removes a component if that component type is on the specified entity
            pub fn remove(self: *world, T: type, edl: *unstable_entity_location) !void {
                // calculate the new hash
                const old_hash = edl.archetype_hash;
                const new_hash = ~(comptime GET_COMPONENT_HASH(T)) & old_hash;

                // test if we stay in same table and exit early
                if (new_hash == old_hash) return;

                // copy values from old table to new table where the new entity is
                const old_table = self._tables.getEntry(old_hash).?.value_ptr;
                var new_table = try self._request_table(new_hash, self._allocator);
                try new_table.insert_existing(old_table, edl.row);

                const public_id = old_table.ptrs_to_public_ids[edl.row];
                self._locations[public_id] = unstable_entity_location{ .archetype_hash = new_hash, .row = new_table.count - 1 };
                new_table.row_to_public_ids[new_table.count - 1] = public_id;

                // swap remove entity from old table
                try old_table.swap_remove_entity(edl.row, self);

                // update edl
                edl.row = new_table.count - 1;
                edl.archetype_hash = new_hash;
            }
            /// internal helper function - retrieve an archetype table and create one if none exists
            pub fn _request_table(self: *world, arch_id: archetype_id, allocator: std.mem.Allocator) !*archetype_table {
                const table_get_put = try self._tables.getOrPut(arch_id);
                if (table_get_put.found_existing) {
                    // std.debug.print("table already exists: {b}\n", .{arch_id});
                    return table_get_put.value_ptr;
                }
                // std.debug.print("creating new table: {b}\n", .{arch_id});
                const table = table_get_put.value_ptr;

                table.init(128, allocator);
                var curr_bit_field: u64 = 1;
                var index: u64 = 0;
                while (curr_bit_field != 0 and index < __runtime_type_information.len) {
                    if (curr_bit_field & arch_id != 0) {
                        try table.construct_column(allocator, __runtime_type_information[index]);
                    }
                    curr_bit_field = curr_bit_field << 1;
                    index += 1;
                }
                // std.debug.print("newly created table hash: {b}\n", .{table.archetype_hash});
                return table;
            }
            /// print world information
            pub fn _print(self: world) void {
                std.debug.print("=================================", .{});
                for (self._tables.values()) |*arch_table| {
                    std.debug.print("\n--", .{});
                    var curr: u64 = 0;
                    while (curr < arch_table.count) {
                        defer curr += 1;
                        std.debug.print("\n", .{});

                        if (arch_table.archetype_hash == 0) {
                            std.debug.print("*void type*", .{});
                            continue;
                        }

                        for (&arch_table.storages) |*maybe_component_storage| {
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
        pub const archetype_table = struct {
            archetype_hash: u64 = 0,
            storages: std.AutoArrayHashMap(component_id, component_column),
            row_to_public_ids: []entity_id,
            capacity: u64 = 0,
            count: u64 = 0,
            allocator: std.mem.Allocator,
            pub fn init(self: *archetype_table, capacity: u64, allocator: std.mem.Allocator) void {
                self.allocator = allocator;
                self.archetype_hash = 0;
                self.storages = std.AutoArrayHashMap(component_id, component_column).init(allocator);
                self.row_to_public_ids = allocator.alloc(entity_id, capacity) catch unreachable;
                self.capacity = capacity;
                self.count = 0;
            }
            pub fn deinit(self: *archetype_table) !void {
                for (self.storages.values()) |*component_storage| {
                    try component_storage.deinit(self.allocator);
                }
                self.storages.deinit();
                self.allocator.free(self.row_to_public_ids);
            }
            pub fn construct_column(self: *archetype_table, allocator: std.mem.Allocator, T_run: runtime_type_info) !void {
                const get_put = try self.storages.getOrPut(T_run.component_id);
                if (get_put.found_existing) return ECSError.RedundantOperation;
                try get_put.value_ptr.init(T_run, self.capacity, allocator);
                self.archetype_hash = self.archetype_hash | T_run.hash;
            }
            pub fn insert_components_comptime(self: *archetype_table, tuple: anytype) !void {
                try self._maybe_double_capacity();
                inline for (tuple) |field| {
                    (try (self.storages.getEntry(comptime GET_COMPONENT_ID(@TypeOf(field))) orelse return ECSError.RequestFailed).value_ptr.entry_ptr(self.count, @TypeOf(field))).* = field;
                }
                self.count += 1;
            }
            pub fn insert_components_runtime(self: *archetype_table, ts: []runtime_type_info, ptrs: [][*]u8) !void {
                try self._maybe_double_capacity();
                for (ts, ptrs) |t, ptr| {
                    _ = t; // autofix
                    _ = ptr; // autofix
                    // (try (self.storages.getEntry(t.component_id) orelse return ECSError.RequestFailed).value_ptr.entry_ptr_runtime().* = field;
                }
            }
            pub fn insert_existing(self: *archetype_table, old_table: *archetype_table, old_row: u64) !void {
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
            pub fn entry_ptr(self: *const archetype_table, T: type, row: u64) ?*T {
                return (self.storages.getEntry(comptime GET_COMPONENT_ID(T)) orelse return null).value_ptr.entry_ptr(row, T) catch unreachable;
            }
            pub fn entry_ptr_runtime(self: *archetype_table, id: component_id, row: u64) ![*]u8 {
                return (try self.storages.getEntry(id).?.value_ptr.entry_ptr_runtime(row));
            }
            pub fn swap_remove_entity(self: *archetype_table, w: *world, row: u64) !void {
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

                const public_id = self.row_to_public_ids[self.count - 1];
                w._locations[public_id] = unstable_entity_location{ .archetype_hash = self.archetype_hash, .row = row };
                self.row_to_public_ids[row] = public_id;

                self.count -= 1;
            }
            pub fn _maybe_double_capacity(self: *archetype_table) !void {
                if (self.count >= self.capacity) {
                    for (self.storages.values()) |*component_storage| {
                        try component_storage.double_capacity(self.allocator);
                    }
                    self.row_to_public_ids = self.allocator.realloc(self.row_to_public_ids, self.capacity * 2) catch unreachable;
                    self.capacity *= 2;
                }
            }
        };

        /// this object is essentially just a pointer to a dynamically allocated array of a singular component type
        pub const component_column = struct {
            array: []u8 = undefined,
            capacity: u64 = undefined,
            type_info: runtime_type_info,
            pub fn init(self: *component_column, T_run: runtime_type_info, capacity: u64, allocator: std.mem.Allocator) !void {
                self.capacity = capacity;
                self.type_info = T_run;
                if (self.type_info.type_size == 0) return;
                self.array = (allocator.vtable.alloc(allocator.ptr, T_run.type_size * capacity, T_run.type_alignment, @returnAddress()) orelse return ECSError.RequestFailed)[0 .. T_run.type_size * capacity];
            }
            pub fn deinit(self: *component_column, allocator: std.mem.Allocator) !void {
                if (self.type_info.type_size == 0) return;
                allocator.vtable.free(allocator.ptr, self.array, self.type_info.type_alignment, @returnAddress());
            }
            pub fn double_capacity(self: *component_column, allocator: std.mem.Allocator) !void {
                self.capacity *= 2;
                if (self.type_info.type_size == 0) return;
                const temp = (allocator.vtable.alloc(allocator.ptr, self.type_info.type_size * self.capacity * 2, self.type_info.type_alignment, @returnAddress()) orelse return ECSError.RequestFailed)[0 .. self.type_info.type_size * self.capacity * 2];
                @memcpy(temp[0..self.array.len], self.array);
                allocator.vtable.free(allocator.ptr, self.array, self.type_info.type_alignment, @returnAddress());
                self.array = temp;
            }
            pub fn entry_bytes(self: *component_column, row: u64) ![]u8 {
                if (row >= self.capacity) {
                    return ECSError.OutOfBounds;
                }
                return self.array[row * self.type_info.type_size .. (row + 1) * self.type_info.type_size];
            }
            pub fn entry_ptr(self: *component_column, row: u64, T: type) !*T {
                return @as(*T, @ptrFromInt(@intFromPtr(self.array.ptr) + row * self.type_info.type_size));
            }
            pub fn entry_ptr_runtime(self: *component_column, row: u64) ![*]u8 {
                if (row >= self.capacity) {
                    return ECSError.OutOfBounds;
                }
                return @ptrFromInt(@intFromPtr(self.array.ptr) + row * self.type_info.type_size);
            }
        };
    };
}
