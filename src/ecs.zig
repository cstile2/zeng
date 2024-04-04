const std = @import("std");

// pub const void_archetype_hash = std.math.maxInt(u64);

// // this is the entire ECS world
// pub const Entities = struct {
//     pub fn init(allocator: std.mem.Allocator) !Entities {
//         var entities = Entities{ .allocator = allocator };

//         try entities.archetypes.put(allocator, void_archetype_hash, ArchetypeStorage{
//             .allocator = allocator,
//             .components = .{},
//             .hash = void_archetype_hash,
//         });

//         return entities;
//     }
//     pub fn initErasedStorage(entities: *const Entities, total_rows: *usize, comptime Component: type) !ErasedComponentStorage {
//         const new_ptr = try entities.allocator.create(ComponentStorage(Component));
//         new_ptr.* = ComponentStorage(Component){ .total_rows = total_rows };

//         return ErasedComponentStorage{
//             .ptr = new_ptr,
//             .deinit = (struct {
//                 pub fn deinit(erased: *anyopaque, allocator: std.mem.Allocator) void {
//                     var ptr = ErasedComponentStorage.cast(erased, Component);
//                     ptr.deinit(allocator);
//                     allocator.destroy(ptr);
//                 }
//             }).deinit,
//         };
//     }
// };

// // this is an archetype table
// pub const ArchetypeStorage = struct {
//     allocator: std.mem.Allocator,

//     /// The hash of every component name in this archetype, i.e. the name of this archetype.
//     hash: u64,

//     /// A string hashmap of component_name -> type-erased *ComponentStorage(Component)
//     components: std.StringArrayHashMapUnmanaged(ErasedComponentStorage),

//     pub fn deinit(storage: *ArchetypeStorage) void {
//         for (storage.components.values()) |erased| {
//             erased.deinit(erased.ptr, storage.allocator);
//         }
//         storage.components.deinit(storage.allocator);
//     }
// };

// // this is an array of a singular component type
// pub fn ComponentStorage(comptime Component: type) type {
//     return struct {
//         /// A reference to the total number of entities with the same type as is being stored here.
//         total_rows: *usize,

//         /// The actual densely stored component data.
//         data: std.ArrayListUnmanaged(Component) = .{},

//         const Self = @This();

//         pub fn deinit(storage: *Self, allocator: std.mem.Allocator) void {
//             storage.data.deinit(allocator);
//         }
//     };
// }

// /// A type-erased representation of ComponentStorage(T) (where T is unknown).
// pub const ErasedComponentStorage = struct {
//     ptr: *anyopaque,

//     // Casts this `ErasedComponentStorage` into `*ComponentStorage(Component)` with the given type
//     // (unsafe).
//     pub fn cast(ptr: *anyopaque, comptime Component: type) *ComponentStorage(Component) {
//         const aligned: *anyopaque = @as(*ComponentStorage(Component), @alignCast(ptr));
//         return @as(*ComponentStorage(Component), @ptrCast(aligned));
//     }
// };

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
    return @ptrFromInt(@intFromPtr(ptr) + offset);
}

pub const EntityDataLocation = struct {
    table_index: u64,
    storage_point: *ArchetypeTable,
};

pub const ArchetypeTable = struct {
    Map: [64]?OpaqueComponentStorage,
    Size: u64,
    Archetype: u64 = 0,
    Capacity: u64,
    pub fn Init(self: *ArchetypeTable, capacity: u64) void {
        self.Map = .{null} ** 64;
        self.Capacity = capacity;
        self.Size = 0;
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
    pub fn GetComponent(self: *ArchetypeTable, T: type, index: u64) !T {
        const row: u64 = comptime ComponentIndex(T);
        if (self.Map[row]) |component_storage| {
            const ptr = @as(*T, @alignCast(@ptrCast(component_storage.array)));
            return AddressOffset(T, ptr, index).*;
        } else {
            std.debug.print("ERROR: Tried get component type that does not exist yet", .{});
            return ArchetypeError.Invalid;
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
