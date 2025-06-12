const std = @import("std");
const utils = @import("utils.zig");
const COMPONENT_TYPES = @import("main.zig").COMPONENT_TYPES;

pub const comp_rtti = struct {
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
pub const entity_id = u64;
const ECSError = error{
    RequestFailed,
    OutOfBounds,
    RedundantOperation,
    NonexistentComponent,
};

// integrate custom type-checking
pub const __runtime_type_information = GENERATE_TYPE_INFOS(COMPONENT_TYPES);

fn GENERATE_TYPE_INFOS(comptime _COMPONENT_TYPES: anytype) [_COMPONENT_TYPES.len]comp_rtti {
    comptime {
        var ret: [_COMPONENT_TYPES.len]comp_rtti = undefined;
        var curr = 0;
        for (_COMPONENT_TYPES) |type_| {
            ret[curr] = comp_rtti{ .hash = COMP_TYPE_TO_HASH(type_), .type_size = @sizeOf(type_), .type_alignment = std.math.log2(@alignOf(type_)), .component_id = curr };
            if (ret[curr].type_size != @sizeOf(type_) or (ret[curr].type_size > 1024)) @compileError("blubber!: " ++ std.fmt.comptimePrint("{} {}", .{ @sizeOf(type_), ret[curr].type_size }));
            curr += 1;
        }
        return ret;
    }
}

// comptime checkers
pub fn COMP_TYPE_TO_HASH(comptime T: type) u64 {
    var curr: u64 = 1;
    for (COMPONENT_TYPES) |type_| {
        if (type_ == T) {
            return curr;
        }
        curr = curr << 1;
    }
    @compileError("Component type is not valid: " ++ @typeName(T));
}
pub fn COMP_TYPE_TO_ID(comptime T: type) u64 {
    var curr: u64 = 0;
    for (COMPONENT_TYPES) |type_| {
        if (type_ == T) {
            return curr;
        }
        curr = curr + 1;
    }
    @compileError("Component type is not valid: " ++ @typeName(T));
}
pub fn COMP_TYPES_TUP_TO_COMBINED_HASH(comptime tuple_type: type) u64 {
    comptime var curr_hash = 0;
    inline for (std.meta.fields(tuple_type)) |f| {
        curr_hash = curr_hash | comptime COMP_TYPE_TO_HASH(f.type);
    }
    return curr_hash;
}
pub fn COMP_TYPELIST_TO_HASH(comptime tuple: anytype) u64 {
    comptime var curr_hash = 0;
    inline for (tuple) |field| {
        curr_hash = curr_hash | comptime COMP_TYPE_TO_HASH(field);
    }
    return curr_hash;
}

// helpers
fn transfer_entity(wrld: *world, start_table: *archetype_table, end_table: *archetype_table, unstable: unstable_entity_location) void {
    end_table.ensure_enough_capacity() catch unreachable;
    for (end_table.storages.values()) |*new_storage| {
        for (start_table.storages.values()) |*old_storage| {
            if (old_storage.type_info.component_id == new_storage.type_info.component_id) {
                const old = old_storage.get_slice(unstable.row);
                const new = new_storage.get_slice(end_table.count);
                @memcpy(new, old);
            }
        }
    }
    end_table.count += 1;

    const public_id = start_table.public_ids[unstable.row];
    wrld.locations.put(public_id, unstable_entity_location{ .archetype_hash = end_table.archetype_hash, .row = end_table.count - 1 }) catch unreachable;
    end_table.public_ids[end_table.count - 1] = public_id;

    try swap_remove_entity(wrld, start_table, unstable.row);
}
fn swap_remove_entity(w: *world, table: *archetype_table, row: u64) !void {
    defer table.count -= 1;
    if (row >= table.count) unreachable;
    if (row == table.count - 1) return;
    for (table.storages.values()) |*component_storage| {
        const bottom = component_storage.get_slice(table.count - 1);
        const upper = component_storage.get_slice(row);
        @memcpy(upper, bottom);
    }
    const public_id = table.public_ids[table.count - 1];
    w.locations.put(public_id, unstable_entity_location{ .archetype_hash = table.archetype_hash, .row = row }) catch unreachable;
    table.public_ids[row] = public_id;
}
pub fn count_component(_world: *world, component_type: type) usize {
    var count: usize = 0;
    for (_world.tables.values()) |table| {
        if ((table.archetype_hash & comptime COMP_TYPE_TO_HASH(component_type)) == comptime COMP_TYPE_TO_HASH(component_type)) count += table.count;
    }
    return count;
}

pub fn query(comptime component_list: anytype) type {
    return struct {
        pub const TYPES: @TypeOf(component_list) = component_list;
        relevant_tables: std.AutoArrayHashMap(archetype_id, *const archetype_table),
        ordered_component_columns: std.ArrayList([component_list.len]component_column), // make columns faster during iteration
        locations: *const std.AutoHashMap(entity_id, unstable_entity_location),

        pub fn create(w: *world, allocator: std.mem.Allocator) !@This() {
            const minimum_set_hash = comptime COMP_TYPELIST_TO_HASH(component_list);
            var ret: @This() = undefined;
            ret.relevant_tables = std.AutoArrayHashMap(archetype_id, *const archetype_table).init(allocator);
            ret.ordered_component_columns = std.ArrayList([component_list.len]component_column).initCapacity(allocator, component_list.len) catch unreachable;
            ret.locations = &w.locations;
            for (w.tables.values()) |*table| {
                if (table.archetype_hash & minimum_set_hash == minimum_set_hash) {
                    ret.relevant_tables.put(table.archetype_hash, table) catch unreachable;
                    ret.ordered_component_columns.append(undefined) catch unreachable;
                    inline for (comptime 0..component_list.len) |i| {
                        ret.ordered_component_columns.items[ret.ordered_component_columns.items.len - 1][i] = table.storages.get(comptime COMP_TYPE_TO_ID(component_list[i])).?;
                    }
                }
            }
            return ret;
        }
        pub fn destroy(self: *@This()) !void {
            self.relevant_tables.deinit();
        }

        pub fn get(self: @This(), entity: entity_id, T: type) ?*T {
            const unstable = self.locations.get(entity).?;
            return (self.relevant_tables.get(unstable.archetype_hash) orelse return null).get(T, unstable.row);
        }
        pub fn iterator(self: *@This()) query_iterator(component_list) {
            return .{ .q = self, .index = 0, .current_table = 0, .q_table_values = self.relevant_tables.values() };
        }
    };
}
pub fn query_iterator(comptime types: anytype) type {
    comptime var tuple_fields: [types.len]std.builtin.Type.StructField = undefined;
    comptime for (types, 0..) |_type, i| {
        tuple_fields[i] = .{
            .type = *_type,
            .name = std.fmt.comptimePrint("{d}", .{i}),
            .default_value = null,
            .is_comptime = false,
            .alignment = @alignOf(_type),
        };
    };
    const ptrs_to_components = @Type(.{ .Struct = .{
        .layout = .Auto,
        .fields = &tuple_fields,
        .decls = &.{},
        .is_tuple = true,
    } });

    return struct {
        q: *const query(types),
        q_table_values: []*const archetype_table,
        index: usize,
        current_table: usize,
        pub const TYPES: @TypeOf(types) = types;
        pub fn next(self: *@This()) ?ptrs_to_components {
            if (self.q_table_values.len == 0) return null;
            while (self.index >= self.q_table_values[self.current_table].count) {
                if (self.current_table + 1 < self.q_table_values.len) {
                    self.current_table += 1;
                    self.index = 0;
                } else return null;
            }

            var current_columns = self.q.ordered_component_columns.items[self.current_table];

            var component_ptrs: ptrs_to_components = undefined;
            inline for (&component_ptrs, comptime 0..) |*component_ptr, i| {
                component_ptr.* = current_columns[i].get(self.index, @TypeOf(component_ptr.*.*));
            }

            self.index += 1;
            return component_ptrs;
        }
        pub fn reset(self: *@This()) void {
            self.index = 0;
            self.current_table = 0;
        }
    };
}

/// Contains all entities for an ECS system and is needed to use the ECS
pub const world = struct {
    tables: std.AutoArrayHashMap(archetype_id, archetype_table),
    allocator: std.mem.Allocator,
    new_public_id: entity_id = 0,
    locations: std.AutoHashMap(entity_id, unstable_entity_location),

    /// initializes the ECS world - required for use
    pub fn init(allocator: std.mem.Allocator) world {
        return .{
            .allocator = allocator,
            .tables = std.AutoArrayHashMap(archetype_id, archetype_table).init(allocator),
            .locations = std.AutoHashMap(entity_id, unstable_entity_location).init(allocator),
        };
    }
    /// deallocates all memory created within this world
    pub fn deinit(self: *world) !void {
        for (self.tables.values()) |*table| {
            try table.deinit();
        }
        self.tables.deinit();
        self.locations.deinit();
    }

    /// this version directly creates destination table
    pub fn alternative_spawn(self: *world, component_values: anytype) entity_id {
        const tuple_hash = comptime COMP_TYPES_TUP_TO_COMBINED_HASH(@TypeOf(component_values));
        var table = self.ensure_table(tuple_hash) catch unreachable;
        table.add_entity_from_components(component_values) catch unreachable;
        self.locations.put(self.new_public_id, unstable_entity_location{ .archetype_hash = table.archetype_hash, .row = table.count - 1 }) catch unreachable;
        table.public_ids[table.count - 1] = self.new_public_id;
        self.new_public_id += 1;
        return self.new_public_id - 1;
    }
    /// this version starts with null table
    pub fn spawn(self: *world, tuple: anytype) entity_id {
        const table = self.ensure_table(0) catch unreachable;
        table.count += 1;
        var edl = unstable_entity_location{ .archetype_hash = 0, .row = table.count - 1 };
        self.locations.put(self.new_public_id, edl) catch unreachable;
        table.public_ids[table.count - 1] = self.new_public_id;
        inline for (tuple) |component| {
            self._internal_faster_add(component, &edl) catch unreachable;
        }
        self.new_public_id += 1;
        return self.new_public_id - 1;
    }
    /// add component - not to be used outside of world context
    pub fn _internal_faster_add(self: *world, V: anytype, edl: *unstable_entity_location) !void {
        const old_hash = edl.archetype_hash;
        const new_hash = (comptime COMP_TYPE_TO_HASH(@TypeOf(V))) | old_hash;
        if (new_hash == old_hash) {
            self.tables.getPtr(old_hash).?.get(@TypeOf(V), edl.row).?.* = V;
            return;
        }
        var new_table = try self.ensure_table(new_hash);
        const old_table = self.tables.getPtr(old_hash).?;
        transfer_entity(self, old_table, new_table, edl.*);
        new_table.get(@TypeOf(V), new_table.count - 1).?.* = V;
        edl.* = unstable_entity_location{ .archetype_hash = new_table.archetype_hash, .row = new_table.count - 1 };
    }
    /// set the value of or add a new component of specified type and value
    pub fn add(self: *world, V: anytype, entity: entity_id) void {
        const unstable = self.locations.get(entity).?;
        const old_hash = unstable.archetype_hash;
        const new_hash = (comptime COMP_TYPE_TO_HASH(@TypeOf(V))) | old_hash;
        if (new_hash == old_hash) {
            self.tables.getPtr(old_hash).?.get(@TypeOf(V), unstable.row).?.* = V;
            return;
        }
        const new_table = self.ensure_table(new_hash) catch unreachable;
        const old_table = self.tables.getPtr(old_hash).?;
        transfer_entity(self, old_table, new_table, unstable);
        new_table.get(@TypeOf(V), new_table.count - 1).?.* = V;
    }
    /// set the value of or add a new component of specified type and value - runtime
    pub fn add_runtime(self: *world, t: comp_rtti, ptr: [*]u8, entity: entity_id) !void {
        const old_edl = self.locations.get(entity).?;
        const old_hash = old_edl.archetype_hash;
        const new_hash = t.hash | old_hash;
        if (new_hash == old_hash) {
            const src = self.tables.getPtr(old_hash).?.get_slice(t.component_id, old_edl.row);
            @memcpy(src, ptr[0..t.type_size]);
            return;
        }
        var new_table = try self.ensure_table(new_hash);
        const old_table = self.tables.getPtr(old_hash).?;
        transfer_entity(self, old_table, new_table, old_edl);
        @memcpy(new_table.get_slice(t.component_id, new_table.count - 1), ptr[0..t.type_size]);
    }
    /// retrieve references to components of an entity
    pub fn get(self: *const world, id: entity_id, T: type) ?*T {
        const unstable = self.locations.get(id) orelse unreachable;
        return (self.tables.getPtr(unstable.archetype_hash) orelse return null).get(T, unstable.row);
    }
    /// removes a component if that component type is on the specified entity
    pub fn _internal_faster_remove(self: *world, T: type, edl: *unstable_entity_location) !void {
        // calculate the new hash
        const old_hash = edl.archetype_hash;
        const new_hash = ~(comptime COMP_TYPE_TO_HASH(T)) & old_hash;

        // test if we stay in same table and exit early
        if (new_hash == old_hash) return;

        // copy values from old table to new table where the new entity is
        const old_table = self.tables.getPtr(old_hash).?;
        var new_table = try self.ensure_table(new_hash, self.allocator);
        try new_table.add_entity_from_copy(old_table, edl.row);

        const public_id = old_table.ptrs_to_public_ids[edl.row];
        self._locations[public_id] = unstable_entity_location{ .archetype_hash = new_hash, .row = new_table.count - 1 };
        new_table.public_ids[new_table.count - 1] = public_id;

        // swap remove entity from old table
        try old_table.swap_remove_entity(edl.row, self);

        // update edl
        edl.row = new_table.count - 1;
        edl.archetype_hash = new_hash;
    }
    pub fn remove(self: *world, T: type, entity: entity_id) void {
        const unstable = self.locations.get(entity).?;
        const old_hash = unstable.archetype_hash;
        const new_hash = ~(comptime COMP_TYPE_TO_HASH(T)) & old_hash;
        if (new_hash == old_hash) return;
        const new_table = self.ensure_table(new_hash) catch unreachable;
        const old_table = self.tables.getPtr(old_hash).?;
        transfer_entity(self, old_table, new_table, unstable);
    }

    /// internal helper function - retrieve an archetype table and create one if none exists
    pub fn ensure_table(self: *world, arch_id: archetype_id) !*archetype_table {
        const table_get_put = try self.tables.getOrPut(arch_id);

        if (table_get_put.found_existing) return table_get_put.value_ptr;

        const table = table_get_put.value_ptr;
        table.init(1000, self.allocator);

        var curr_bit_field: u64 = 1;
        var index: usize = 0;
        while (curr_bit_field != 0 and index < __runtime_type_information.len) {
            if (curr_bit_field & arch_id != 0) {
                try table.construct_column((&__runtime_type_information[index]).*); // TODO: if ecs breaks check this (make a pointer and pointlessly dereference it)
                // try table.construct_column(__runtime_type_information[index]); // TODO: if ecs breaks check this (make a pointer and pointlessly dereference it)

            }
            curr_bit_field = curr_bit_field << 1;
            index += 1;
        }
        return table;
    }
    /// print world information
    pub fn print(self: world) void {
        std.debug.print("=================================", .{});
        for (self.tables.values()) |*arch_table| {
            std.debug.print("\n-----", .{});
            var curr: u64 = 0;
            while (curr < arch_table.count) {
                defer curr += 1;
                std.debug.print("\n", .{});

                if (arch_table.archetype_hash == 0) {
                    std.debug.print("<void>", .{});
                    continue;
                }

                for (arch_table.storages.values()) |_| {
                    std.debug.print("*", .{});
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
    public_ids: []entity_id,
    capacity: u64 = 0,
    count: u64 = 0,
    allocator: std.mem.Allocator,

    pub fn init(self: *archetype_table, capacity: u64, allocator: std.mem.Allocator) void {
        self.allocator = allocator;
        self.archetype_hash = 0;
        self.storages = std.AutoArrayHashMap(component_id, component_column).init(allocator);
        self.public_ids = allocator.alloc(entity_id, capacity) catch unreachable;
        self.capacity = capacity;
        self.count = 0;
    }
    pub fn deinit(self: *archetype_table) !void {
        for (self.storages.values()) |*component_storage| {
            try component_storage.deinit(self.allocator);
        }
        self.storages.deinit();
        self.allocator.free(self.public_ids);
    }

    pub fn construct_column(self: *archetype_table, T_run: comp_rtti) !void {
        self.archetype_hash = self.archetype_hash | T_run.hash;

        var new: component_column = undefined;
        try new.init(T_run, self.capacity, self.allocator);
        try self.storages.putNoClobber(T_run.component_id, new);
    }
    pub fn add_entity_from_components(self: *archetype_table, component_values: anytype) !void {
        self.ensure_enough_capacity() catch unreachable;

        inline for (component_values) |field| {
            const storage = self.storages.getPtr(comptime COMP_TYPE_TO_ID(@TypeOf(field))) orelse return ECSError.RequestFailed;
            storage.get(self.count, @TypeOf(field)).* = field;
        }
        self.count += 1;
    }
    pub fn ensure_enough_capacity(self: *archetype_table) !void {
        if (self.count >= self.capacity) {
            for (self.storages.values()) |*component_storage| {
                try component_storage.double_capacity(self.allocator);
            }
            self.public_ids = self.allocator.realloc(self.public_ids, self.capacity * 2) catch unreachable;
            self.capacity *= 2;
        }
    }
    pub fn get(self: *const archetype_table, T: type, row: u64) ?*T {
        if (row >= self.count) unreachable;
        return (self.storages.getPtr(comptime COMP_TYPE_TO_ID(T)) orelse return null).get(row, T);
    }
    pub fn get_slice(self: *archetype_table, id: component_id, row: u64) []u8 {
        if (row >= self.count) unreachable;
        return self.storages.getPtr(id).?.get_slice(row);
    }
};
/// this object is essentially just a pointer to a dynamically allocated array of a singular component type
pub const component_column = struct {
    array: []u8 = undefined,
    capacity: u64 = undefined,
    type_info: comp_rtti = undefined,

    pub fn init(self: *component_column, T_run: comp_rtti, capacity: usize, allocator: std.mem.Allocator) !void {
        self.capacity = capacity;
        self.type_info = T_run;
        if (self.type_info.type_size == 0) return;
        self.array = (allocator.rawAlloc(T_run.type_size * capacity, T_run.type_alignment, @returnAddress()) orelse return ECSError.RequestFailed)[0 .. capacity * T_run.type_size];
        // self.array = (allocator.vtable.alloc(allocator.ptr, T_run.type_size * capacity, T_run.type_alignment, @returnAddress()) orelse return ECSError.RequestFailed)[0 .. T_run.type_size * capacity];
    }
    pub fn deinit(self: *component_column, allocator: std.mem.Allocator) !void {
        if (self.type_info.type_size == 0) return;
        // allocator.vtable.free(allocator.ptr, self.array, self.type_info.type_alignment, @returnAddress());
        allocator.rawFree(self.array, self.type_info.type_alignment, @returnAddress());
    }

    pub fn double_capacity(self: *component_column, allocator: std.mem.Allocator) !void {
        self.capacity *= 2;
        if (self.type_info.type_size == 0) return;
        const temp = (allocator.vtable.alloc(allocator.ptr, self.type_info.type_size * self.capacity * 2, self.type_info.type_alignment, @returnAddress()) orelse return ECSError.RequestFailed)[0 .. self.type_info.type_size * self.capacity * 2];
        @memcpy(temp[0..self.array.len], self.array);
        allocator.vtable.free(allocator.ptr, self.array, self.type_info.type_alignment, @returnAddress());
        self.array = temp;
    }
    pub fn get(self: *component_column, row: usize, T: type) *T {
        if (row >= self.capacity) unreachable;
        return @as(*T, @ptrFromInt(@intFromPtr(self.array.ptr) + row * self.type_info.type_size));
    }
    pub fn get_slice(self: *component_column, row: usize) []u8 {
        if (row >= self.capacity) unreachable;
        return self.array[row * self.type_info.type_size .. (row + 1) * self.type_info.type_size];
    }
};
