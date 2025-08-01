A handmade game engine written in the Zig programming language. This project was designed to give power to developers without dictating their achitecture. I wanted to give users full-control over the application entry point and main loop, making this project a simple library.

Features:

- OpenGl rendering module
  - Supports GLSL shaders and skeletal animation rendering.
- Entity Component System (ECS) module
  - Archetype-based ECS used to optimize cache locality and make data dependencies explicit and easily parallelizable.
- Audio module
  - Audio runs on a seperate thread and communicates via atomic operations.
- Networking module
  - Includes communication protocol built on top of UDP - includes latency and packet loss simulation for testing netcode robustness.
- Asset loading module
  - JSON parser and importer - includes support for 3D GLTF asset files.
- Dynamic code-reloading
  - Users specify functions that can be swapped at runtime, allowing faster iteration times.
- Optimized collision detection
  - GJK algorithm combined with a spatial hash grid for only testing nearby objects for penetration.

This repo includes the engine modules as well as an example main.zig file to demonstrate the engine in a first-person-shooter context. This example includes first-person-shooter gameplay, multiplayer over UDP, entity interpolation, client-side prediction, and more!

This project currently only works for Windows.
