const std = @import("std");
const utils = @import("utils.zig");
const zeng = @import("zeng.zig");
const COMPONENT_TYPES = zeng.COMPONENT_TYPES;

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
const ecs_error = error{
    request_failed,
    out_of_bounds,
    redundant_operation,
    nonexistent_component,
};

// integrate custom type-checking
pub const __runtime_type_information = GENERATE_TYPE_INFOS(COMPONENT_TYPES);

fn GENERATE_TYPE_INFOS(comptime _COMPONENT_TYPES: anytype) [_COMPONENT_TYPES.len]comp_rtti {
    comptime {
        var ret: [_COMPONENT_TYPES.len]comp_rtti = undefined;
        var curr = 0;
        for (_COMPONENT_TYPES) |type_| {
            ret[curr] = comp_rtti{ .hash = COMP_TYPE_TO_HASH(type_), .type_size = @sizeOf(type_), .type_alignment = std.math.log2(@alignOf(type_)), .component_id = curr };
            if (ret[curr].type_size != @sizeOf(type_) or (ret[curr].type_size > 20000)) @compileError("blubber!: " ++ std.fmt.comptimePrint("{} {}", .{ @sizeOf(type_), ret[curr].type_size }));
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
fn transfer_entity(wrld: *world_t, start_table: *archetype_table, end_table: *archetype_table, unstable: unstable_entity_location) void {
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
fn swap_remove_entity(w: *world_t, table: *archetype_table, row: u64) !void {
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
pub fn count_component(_world: *world_t, component_type: type) usize {
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

        pub fn create_and_gather(w: *world_t, allocator: std.mem.Allocator) @This() {
            const minimum_set_hash = comptime COMP_TYPELIST_TO_HASH(component_list);
            var ret: @This() = undefined;
            ret.relevant_tables = std.AutoArrayHashMap(archetype_id, *const archetype_table).init(allocator);
            ret.ordered_component_columns = std.ArrayList([component_list.len]component_column).initCapacity(allocator, component_list.len) catch unreachable;
            ret.locations = &w.locations;
            for (w.tables.values()) |*table| {
                if (table.archetype_hash & minimum_set_hash == minimum_set_hash) {
                    ret.relevant_tables.put(table.archetype_hash, table) catch unreachable;
                    ret.ordered_component_columns.append(allocator, undefined) catch unreachable;
                    inline for (comptime 0..component_list.len) |i| {
                        ret.ordered_component_columns.items[ret.ordered_component_columns.items.len - 1][i] = table.storages.get(comptime COMP_TYPE_TO_ID(component_list[i])).?;
                    }
                }
            }
            return ret;
        }
        pub fn deinit(this: *@This()) void {
            this.relevant_tables.deinit();
        }

        pub fn get(this: @This(), entity: entity_id, T: type) ?*T {
            const unstable = this.locations.get(entity).?;
            return (this.relevant_tables.get(unstable.archetype_hash) orelse return null).get(T, unstable.row);
        }
        pub fn iterator(this: *@This()) query_iterator(component_list) {
            return .{ ._parent_query = this, ._current_table_index = 0, ._current_table = 0, ._parent_query_relevant_tables = this.relevant_tables.values() };
        }
    };
}
pub fn query_iterator(comptime types: anytype) type {
    comptime var tuple_fields2: [types.len]std.builtin.Type.StructField = undefined;
    comptime for (types, 0..) |_type, i| {
        tuple_fields2[i] = .{
            .type = _type,
            .name = std.fmt.comptimePrint("c_{}", .{_type}),
            .default_value_ptr = null,
            .is_comptime = false,
            .alignment = @alignOf(_type),
        };
    };
    comptime var tuple_fields: [types.len]std.builtin.Type.StructField = undefined;
    comptime for (types, 0..) |_type, i| {
        tuple_fields[i] = .{
            .type = *_type,
            .name = std.fmt.comptimePrint("{d}", .{i}),
            .default_value_ptr = null,
            .is_comptime = false,
            .alignment = @alignOf(*_type),
        };
    };
    const ptrs_to_components = @Type(.{ .@"struct" = .{
        .layout = .auto,
        .fields = &tuple_fields,
        .decls = &.{},
        .is_tuple = true,
    } });
    const ptrs_to_components_struct = @Type(.{ .@"struct" = .{
        .layout = .auto,
        .fields = &tuple_fields2,
        .decls = &.{},
        .is_tuple = false,
    } });

    return struct {
        _parent_query: *const query(types),
        _parent_query_relevant_tables: []*const archetype_table,
        _current_table_index: usize,
        _current_table: usize,
        current_entity_id: entity_id = undefined,
        pub const TYPES: @TypeOf(types) = types;
        pub fn next(this: *@This()) ?ptrs_to_components {
            if (this._parent_query_relevant_tables.len == 0) return null;
            while (this._current_table_index >= this._parent_query_relevant_tables[this._current_table].count) {
                if (this._current_table + 1 < this._parent_query_relevant_tables.len) {
                    this._current_table += 1;
                    this._current_table_index = 0;
                } else return null;
            }

            var current_columns = this._parent_query.ordered_component_columns.items[this._current_table];

            var component_ptrs: ptrs_to_components = undefined;
            inline for (&component_ptrs, comptime 0..) |*component_ptr, i| {
                component_ptr.* = current_columns[i].get(this._current_table_index, @TypeOf(component_ptr.*.*));
            }
            this.current_entity_id = this._parent_query_relevant_tables[this._current_table].public_ids[this._current_table_index];

            this._current_table_index += 1;
            return component_ptrs;
        }
        pub fn next_entity(this: *@This()) ?ptrs_to_components_struct {
            if (this._parent_query_relevant_tables.len == 0) return null;
            while (this._current_table_index >= this._parent_query_relevant_tables[this._current_table].count) {
                if (this._current_table + 1 < this._parent_query_relevant_tables.len) {
                    this._current_table += 1;
                    this._current_table_index = 0;
                } else return null;
            }

            var current_columns = this._parent_query.ordered_component_columns.items[this._current_table];

            var component_ptrs: ptrs_to_components = undefined;
            inline for (&component_ptrs, comptime 0..) |*component_ptr, i| {
                component_ptr.* = current_columns[i].get(this._current_table_index, @TypeOf(component_ptr.*.*));
            }
            this.current_entity_id = this._parent_query_relevant_tables[this._current_table].public_ids[this._current_table_index];

            this._current_table_index += 1;
            return component_ptrs;
        }

        pub fn reset(this: *@This()) void {
            this._current_table_index = 0;
            this._current_table = 0;
        }
    };
}

/// Contains all entities for an ECS system and is needed to use the ECS
pub const world_t = struct {
    tables: std.AutoArrayHashMap(archetype_id, archetype_table),
    allocator: std.mem.Allocator,
    new_public_id: entity_id = 0,
    locations: std.AutoHashMap(entity_id, unstable_entity_location),

    /// initializes the ECS world - required for use
    pub fn init(allocator: std.mem.Allocator) world_t {
        return .{
            .allocator = allocator,
            .tables = std.AutoArrayHashMap(archetype_id, archetype_table).init(allocator),
            .locations = std.AutoHashMap(entity_id, unstable_entity_location).init(allocator),
        };
    }
    /// deallocates all memory created within this world
    pub fn deinit(this: *world_t) void {
        for (this.tables.values()) |*table| {
            table.deinit();
        }
        this.tables.deinit();
        this.locations.deinit();
    }

    /// this version directly creates destination table
    pub fn alternative_spawn(this: *world_t, component_values: anytype) entity_id {
        const tuple_hash = comptime COMP_TYPES_TUP_TO_COMBINED_HASH(@TypeOf(component_values));
        var table = this.ensure_table(tuple_hash) catch unreachable;
        table.add_entity_from_components(component_values) catch unreachable;
        this.locations.put(this.new_public_id, unstable_entity_location{ .archetype_hash = table.archetype_hash, .row = table.count - 1 }) catch unreachable;
        table.public_ids[table.count - 1] = this.new_public_id;
        this.new_public_id += 1;
        return this.new_public_id - 1;
    }
    /// this version starts with null table
    pub fn spawn(this: *world_t, tuple: anytype) entity_id {
        const table = this.ensure_table(0) catch unreachable;
        table.count += 1;
        var edl = unstable_entity_location{ .archetype_hash = 0, .row = table.count - 1 };
        this.locations.put(this.new_public_id, edl) catch unreachable;
        table.public_ids[table.count - 1] = this.new_public_id;
        inline for (tuple) |component| {
            this._internal_faster_add(component, &edl) catch unreachable;
        }
        this.new_public_id += 1;
        return this.new_public_id - 1;
    }
    /// delete an entity from the world completely - does not free any memory owned by the entity or its components
    pub fn despawn(this: *world_t, entity: entity_id) void {
        const unstable = this.locations.get(entity).?;
        const table = this.tables.getPtr(unstable.archetype_hash).?;
        swap_remove_entity(this, table, unstable.row) catch unreachable;
        _ = this.locations.remove(entity);
    }
    /// add component - not to be used outside of world context
    pub fn _internal_faster_add(this: *world_t, V: anytype, edl: *unstable_entity_location) !void {
        const old_hash = edl.archetype_hash;
        const new_hash = (comptime COMP_TYPE_TO_HASH(@TypeOf(V))) | old_hash;
        if (new_hash == old_hash) {
            this.tables.getPtr(old_hash).?.get(@TypeOf(V), edl.row).?.* = V;
            return;
        }
        var new_table = try this.ensure_table(new_hash);
        const old_table = this.tables.getPtr(old_hash).?;
        transfer_entity(this, old_table, new_table, edl.*);
        new_table.get(@TypeOf(V), new_table.count - 1).?.* = V;
        edl.* = unstable_entity_location{ .archetype_hash = new_table.archetype_hash, .row = new_table.count - 1 };
    }
    /// set the value of or add a new component of specified type and value
    pub fn add(this: *world_t, V: anytype, entity: entity_id) void {
        const unstable = this.locations.get(entity).?;
        const old_hash = unstable.archetype_hash;
        const new_hash = (comptime COMP_TYPE_TO_HASH(@TypeOf(V))) | old_hash;
        if (new_hash == old_hash) {
            this.tables.getPtr(old_hash).?.get(@TypeOf(V), unstable.row).?.* = V;
            return;
        }
        const new_table = this.ensure_table(new_hash) catch unreachable;
        const old_table = this.tables.getPtr(old_hash).?;
        transfer_entity(this, old_table, new_table, unstable);
        new_table.get(@TypeOf(V), new_table.count - 1).?.* = V;
    }
    /// set the value of or add a new component of specified type and value - runtime
    pub fn add_runtime(this: *world_t, t: comp_rtti, ptr: [*]u8, entity: entity_id) !void {
        const old_edl = this.locations.get(entity).?;
        const old_hash = old_edl.archetype_hash;
        const new_hash = t.hash | old_hash;
        if (new_hash == old_hash) {
            const src = this.tables.getPtr(old_hash).?.get_slice(t.component_id, old_edl.row);
            @memcpy(src, ptr[0..t.type_size]);
            return;
        }
        var new_table = try this.ensure_table(new_hash);
        const old_table = this.tables.getPtr(old_hash).?;
        transfer_entity(this, old_table, new_table, old_edl);
        @memcpy(new_table.get_slice(t.component_id, new_table.count - 1), ptr[0..t.type_size]);
    }
    pub fn runtime_get(this: *const world_t, id: entity_id, name: zeng.component_name) ?*anyopaque {
        const unstable = this.locations.get(id).?;
        const ptr = (this.tables.getPtr(unstable.archetype_hash) orelse return null).get_(name, unstable.row);

        return ptr;
    }
    /// retrieve references to components of an entity
    pub fn get(this: *const world_t, id: entity_id, T: type) ?*T {
        const unstable = this.locations.get(id).?;
        return (this.tables.getPtr(unstable.archetype_hash) orelse return null).get(T, unstable.row);
    }
    pub fn get_checked(this: *const world_t, id: entity_id, T: type) !?*T {
        const my_error = error{just_an_error};
        const unstable = this.locations.get(id) orelse return my_error.just_an_error;
        return (this.tables.getPtr(unstable.archetype_hash) orelse return null).get(T, unstable.row);
    }
    pub fn is_alive(this: *@This(), entity: entity_id) bool {
        if (this.locations.get(entity)) |_| {
            return true;
        } else {
            return false;
        }
    }
    /// removes a component if that component type is on the specified entity
    pub fn _internal_faster_remove(this: *world_t, T: type, edl: *unstable_entity_location) !void {
        // calculate the new hash
        const old_hash = edl.archetype_hash;
        const new_hash = ~(comptime COMP_TYPE_TO_HASH(T)) & old_hash;

        // test if we stay in same table and exit early
        if (new_hash == old_hash) return;

        // copy values from old table to new table where the new entity is
        const old_table = this.tables.getPtr(old_hash).?;
        var new_table = try this.ensure_table(new_hash, this.allocator);
        try new_table.add_entity_from_copy(old_table, edl.row);

        const public_id = old_table.ptrs_to_public_ids[edl.row];
        this._locations[public_id] = unstable_entity_location{ .archetype_hash = new_hash, .row = new_table.count - 1 };
        new_table.public_ids[new_table.count - 1] = public_id;

        // swap remove entity from old table
        try old_table.swap_remove_entity(edl.row, this);

        // update edl
        edl.row = new_table.count - 1;
        edl.archetype_hash = new_hash;
    }
    pub fn remove(this: *world_t, T: type, entity: entity_id) void {
        const unstable = this.locations.get(entity).?;
        const old_hash = unstable.archetype_hash;
        const new_hash = ~(comptime COMP_TYPE_TO_HASH(T)) & old_hash;
        if (new_hash == old_hash) return;
        const new_table = this.ensure_table(new_hash) catch unreachable;
        const old_table = this.tables.getPtr(old_hash).?;
        transfer_entity(this, old_table, new_table, unstable);
    }

    /// internal helper function - retrieve an archetype table and create one if none exists
    pub fn ensure_table(this: *world_t, arch_id: archetype_id) !*archetype_table {
        const table_get_put = try this.tables.getOrPut(arch_id);

        if (table_get_put.found_existing) return table_get_put.value_ptr;

        const table = table_get_put.value_ptr;
        table.init(1000, this.allocator);

        var curr_bit_field: u64 = 1;
        var index: usize = 0;
        while (curr_bit_field != 0 and index < __runtime_type_information.len) {
            if (curr_bit_field & arch_id != 0) {
                // try table.construct_column((&__runtime_type_information[index]).*); // TODO: if ecs breaks check this (make a pointer and pointlessly dereference it)
                try table.construct_column(__runtime_type_information[index]); // TODO: if ecs breaks check this (make a pointer and pointlessly dereference it)

            }
            curr_bit_field = curr_bit_field << 1;
            index += 1;
        }
        return table;
    }
    /// print world information
    pub fn print(this: world_t) void {
        std.debug.print("=================================", .{});
        for (this.tables.values()) |*arch_table| {
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

    pub fn init(this: *archetype_table, capacity: u64, allocator: std.mem.Allocator) void {
        this.allocator = allocator;
        this.archetype_hash = 0;
        this.storages = std.AutoArrayHashMap(component_id, component_column).init(allocator);
        this.public_ids = allocator.alloc(entity_id, capacity) catch unreachable;
        this.capacity = capacity;
        this.count = 0;
    }
    pub fn deinit(this: *archetype_table) void {
        for (this.storages.values()) |*component_storage| {
            component_storage.deinit(this.allocator);
        }
        this.storages.deinit();
        this.allocator.free(this.public_ids);
    }

    pub fn construct_column(this: *archetype_table, T_run: comp_rtti) !void {
        this.archetype_hash = this.archetype_hash | T_run.hash;

        var new: component_column = undefined;
        try new.init(T_run, this.capacity, this.allocator);
        try this.storages.putNoClobber(T_run.component_id, new);
    }
    pub fn add_entity_from_components(this: *archetype_table, component_values: anytype) !void {
        this.ensure_enough_capacity() catch unreachable;

        inline for (component_values) |field| {
            const storage = this.storages.getPtr(comptime COMP_TYPE_TO_ID(@TypeOf(field))) orelse return ecs_error.request_failed;
            storage.get(this.count, @TypeOf(field)).* = field;
        }
        this.count += 1;
    }
    pub fn ensure_enough_capacity(this: *archetype_table) !void {
        if (this.count >= this.capacity) {
            for (this.storages.values()) |*component_storage| {
                try component_storage.double_capacity(this.allocator);
            }
            this.public_ids = this.allocator.realloc(this.public_ids, this.capacity * 2) catch unreachable;
            this.capacity *= 2;
        }
    }
    pub fn get(this: *const archetype_table, T: type, row: u64) ?*T {
        if (row >= this.count) unreachable;
        return (this.storages.getPtr(comptime COMP_TYPE_TO_ID(T)) orelse return null).get(row, T);
    }
    pub fn get_(this: *archetype_table, name: zeng.component_name, row: u64) ?*anyopaque {
        if (row >= this.count) unreachable;
        return (this.storages.getPtr(@intFromEnum(name)) orelse return null).get_(row);
    }
    pub fn get_slice(this: *archetype_table, id: component_id, row: u64) []u8 {
        if (row >= this.count) unreachable;
        return this.storages.getPtr(id).?.get_slice(row);
    }
};
/// this object is essentially just a pointer to a dynamically allocated array of a singular component type
pub const component_column = struct {
    array: []u8 = undefined,
    capacity: u64 = undefined,
    type_info: comp_rtti = undefined,

    pub fn init(this: *component_column, T_run: comp_rtti, capacity: usize, allocator: std.mem.Allocator) !void {
        this.capacity = capacity;
        this.type_info = T_run;
        if (this.type_info.type_size == 0) return;
        this.array = (allocator.rawAlloc(T_run.type_size * capacity, @enumFromInt(this.type_info.type_alignment), @returnAddress()) orelse unreachable)[0 .. capacity * T_run.type_size];
        // this.array = (allocator.vtable.alloc(allocator.ptr, T_run.type_size * capacity, T_run.type_alignment, @returnAddress()) orelse return ecs_error.request_failed)[0 .. T_run.type_size * capacity];
    }
    pub fn deinit(this: *component_column, allocator: std.mem.Allocator) void {
        if (this.type_info.type_size == 0) return;
        // allocator.vtable.free(allocator.ptr, this.array, this.type_info.type_alignment, @returnAddress());
        allocator.rawFree(this.array, @enumFromInt(this.type_info.type_alignment), @returnAddress());
    }

    pub fn double_capacity(this: *component_column, allocator: std.mem.Allocator) !void {
        this.capacity *= 2;
        if (this.type_info.type_size == 0) return;
        const temp = (allocator.vtable.alloc(allocator.ptr, this.type_info.type_size * this.capacity * 2, @enumFromInt(this.type_info.type_alignment), @returnAddress()) orelse return ecs_error.request_failed)[0 .. this.type_info.type_size * this.capacity * 2];
        @memcpy(temp[0..this.array.len], this.array);
        allocator.vtable.free(allocator.ptr, this.array, @enumFromInt(this.type_info.type_alignment), @returnAddress());
        this.array = temp;
    }
    pub fn get(this: *component_column, row: usize, T: type) *T {
        if (row >= this.capacity) unreachable;
        return @as(*T, @ptrFromInt(@intFromPtr(this.array.ptr) + row * this.type_info.type_size));
    }
    pub fn get_(this: *component_column, row: usize) *anyopaque {
        if (row >= this.capacity) unreachable;
        return @as(*anyopaque, @ptrFromInt(@intFromPtr(this.array.ptr) + row * this.type_info.type_size));
    }
    pub fn get_slice(this: *component_column, row: usize) []u8 {
        if (row >= this.capacity) unreachable;
        return this.array[row * this.type_info.type_size .. (row + 1) * this.type_info.type_size];
    }
};
