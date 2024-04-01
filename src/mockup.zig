const Engine = @import("engine.zig");

const Tank = struct {};
const Root = struct {};
const Input = struct {};
const Turret = struct {};
const Transform = struct {};
const LocalTransform = struct {};
const Velocity = struct {};
const Entity = struct {};
const Door = struct {};
const Button = struct {};
const Parent = struct {};
const Children = struct {};
const Chassis = struct {};
const Commands = struct {};
const FrameInfo = struct {};
const Character = struct {};
const Target = struct {};
const Mesh = struct {};
const Collider = struct {};
const Burnable = struct {};

pub fn Resource() void {}
pub fn Query() void {}
pub fn EventReader() void {}
pub fn EventWriter() void {}
pub fn Maybe() void {}
pub fn Without() void {}
pub fn With() void {}

fn SyncTransforms(root_entity: anytype, node_query: Query(Transform, LocalTransform)) void {
    for (root_entity[1]) |id| { // loop thru children - use their id's for "dereferencing"
        if (node_query.find(id)) |node_entity| {
            node_entity[0] = Engine.MatrixMultiply(root_entity[1], node_entity[1]);
            SyncTransforms(node_entity, node_query);
        }
    }
}

pub fn SyncTransformsSystem(root_query: Query(Transform, Children, Without(Parent)), node_query: Query(Transform, LocalTransform)) void {
    for (root_query) |root_entity| {
        SyncTransforms(root_entity, node_query);
    }
}

pub fn TankIteration(frame_info: Resource(FrameInfo), commands: Commands, tank_query: Query(Root, Transform, Tank), turret_query: Query(LocalTransform, Turret)) void {
    for (tank_query) |entity| {
        if (turret_query.find(entity[2].turret)) |turret_entity| {
            turret_entity[0] = Engine.RotateTowardsMouse(frame_info.frame_seconds); // aim turret

            if (Engine.ShootPressed()) { // shoot bullets - buffering is needed because it changes world structure
                var bullet_transform = turret_entity[0];
                Engine.translate(&bullet_transform, turret_entity[0].origin);
                commands.spawn(.{bullet_transform});
            }

            entity[1].position += entity[1].forward * frame_info.frame_seconds; // move tank body
        }
    }
}

// Resource(HashMap(EntityID, bool))
pub fn ButtonSystem(button_query: Query(Button, Transform), character_query: Query(Character, Transform), button_event: EventWriter(Entity)) void {
    for (button_query) |button_entity| {
        for (character_query) |character_entity| {
            if (Engine.touching(character_entity, button_entity)) {
                button_event.send(Entity);
                // button_hash_res[button_entity.id()] = true
            }
        }
    }
}

// this system needs to happen after ButtonSystem
pub fn DoorOpenSystem(door_query: Query(Door), button_event_reader: EventReader(Entity)) void {
    for (door_query) |door_entity| {
        for (button_event_reader) |button_event_entity| {
            if (button_event_entity == door_entity[0].listen_entity) {
                door_entity[0].open = true;
            }
        }
    }
}

// if 2 guys need to interact, then just include both in the parameters - make sure to include all dependencies of this interaction
pub fn ButtonSystem2(button_query: Query(Button, Transform, Target), character_query: Query(Character, Transform), door_query: Query(Door)) void {
    for (button_query) |button_entity| {
        for (character_query) |character_entity| {
            if (Engine.touching(character_entity, button_entity)) {
                if (door_query.find(button_entity[2].entity_id)) |door_entity| { // use door
                    Door.DoDoorStuff(&door_entity);
                }
            }
        }
    }
}

// any entity with mesh+transform+burnable will "catch fire" when anything with mesh+transform+burnable touches them that is on fire.
pub fn BurnSystem(burnable_query: Query(Entity, Mesh, Collider, Transform, Burnable)) void {
    for (burnable_query) |entityA| {
        for (burnable_query) |entityB| {
            if (entityA[0] == entityB[0]) continue;

            if (Engine.Colliding(entityA[2], entityA[3], entityA[2], entityA[3])) {
                // if one burns, both of them burn (this method has order-dependent behavior but is faster - use hashmap for order-independence) OOH also consider disjoint sets algo for "instant propogation" we have options!
                entityA[4].burning = entityA[4].burning or entityB[4].burning;
                entityB[4].burning = entityA[4].burning or entityB[4].burning;
            }
        }
    }
}

// system schedule - linked list of function pointers - figure out dependency injection
// entities - simple ids - indexes of an array containing pointers to spot in archetype table? - spot can change as long as it tracks its new position

// events are so different now. they are NOT instant. that stings a little bit for now. I just want to know the truth - which one is
// better and/or which one is NECESSARY when and for what.
