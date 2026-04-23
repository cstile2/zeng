const std = @import("std");
const c = zeng.c;
const zeng = @import("zeng.zig");

pub const audio_sample_info = struct {
    audio_format: u16, // 1 = PCM
    num_channels: u16,
    sample_rate: u32,
    byte_rate: u32,
    block_align: u16,
    bits_per_sample: u16,
    pcm_data: []const u8, // Slice pointing to the PCM data
};
pub const sound_play_state = struct {
    wav_file: audio_sample_info,
    sample_rate_ratio: f32 = undefined,
    float_counter: f32 = 0.0,
    sample_index: usize = 0,
    mode: loop_mode = .loop,
    alive: bool = true,
};
pub const loop_mode = enum {
    loop,
    one_shot,
};

var FRAME_SIZE: usize = undefined;
var BYTES_PER_SAMPLE: usize = undefined;

pub fn get_audio_file_data(wav_bytes: []const u8) !audio_sample_info {
    if (wav_bytes.len < 44) return error.InvalidWavFile; // Minimum header size

    // Verify RIFF header
    if (!std.mem.eql(u8, wav_bytes[0..4], "RIFF") or !std.mem.eql(u8, wav_bytes[8..12], "WAVE")) {
        return error.InvalidWavFile;
    }

    var offset: usize = 12; // Start after RIFF/WAVE headers
    var fmt_found = false;
    var data_found = false;
    var format: audio_sample_info = undefined;
    var pcm_start: usize = 0;
    var pcm_size: usize = 0;

    while (offset + 8 <= wav_bytes.len) {
        const chunk_id = wav_bytes[offset .. offset + 4]; // Chunk identifier
        const chunk_size = std.mem.bytesToValue(u32, wav_bytes[offset + 4 .. offset + 8]);
        // std.mem.bytesToValue(u16, bytes: anytype)
        offset += 8; // Move to chunk data

        if (std.mem.eql(u8, chunk_id, "fmt ")) {
            if (chunk_size < 16) return error.InvalidFmtChunk;
            format.audio_format = std.mem.bytesToValue(u16, wav_bytes[offset .. offset + 2]);
            format.num_channels = std.mem.bytesToValue(u16, wav_bytes[offset + 2 .. offset + 4]);
            format.sample_rate = std.mem.bytesToValue(u32, wav_bytes[offset + 4 .. offset + 8]);
            format.byte_rate = std.mem.bytesToValue(u32, wav_bytes[offset + 8 .. offset + 12]);
            format.block_align = std.mem.bytesToValue(u16, wav_bytes[offset + 12 .. offset + 14]);
            format.bits_per_sample = std.mem.bytesToValue(u16, wav_bytes[offset + 14 .. offset + 16]);
            fmt_found = true;
        } else if (std.mem.eql(u8, chunk_id, "data")) {
            pcm_start = offset;
            pcm_size = chunk_size;
            data_found = true;
        }

        offset += chunk_size; // Move to the next chunk
        if (offset > wav_bytes.len) return error.InvalidWavFile; // Avoid reading out of bounds
    }

    if (!fmt_found or !data_found) return error.WavFileMissingChunks;

    format.pcm_data = wav_bytes[pcm_start .. pcm_start + pcm_size];

    return format;
}
pub fn play_sound(sample_info: audio_sample_info, mode: loop_mode) void {
    const D = sound_play_state{ .wav_file = sample_info, .mode = mode };
    const atomically_requested_message_spot = @atomicRmw(usize, &message_queue_count_WRITERS, std.builtin.AtomicRmwOp.Add, 1, std.builtin.AtomicOrder.monotonic);
    get_message(atomically_requested_message_spot).* = D;
    _ = @atomicRmw(usize, &message_queue_count_READER, std.builtin.AtomicRmwOp.Add, 1, std.builtin.AtomicOrder.monotonic);
}

fn windows_check_error(ERROR: c.HRESULT) !void {
    // std.debug.print("error: {x}\n", .{@as(u32, @bitCast(ERROR))});
    if (ERROR != c.S_OK) return error.Overflow;
}
fn convert_sample_to_output_format(T: type, in: T) f32 {
    const info = @typeInfo(T);
    const max: f32 = std.math.maxInt(T);

    if (info.int.signedness == .signed) {
        const sample_float: f32 = @as(f32, @floatFromInt(in)) / max;
        return sample_float;
    } else {
        const sample_float: f32 = (@as(f32, @floatFromInt(in)) - (max / 2.0)) / (max / 2.0);
        return sample_float;
    }
}
fn process_sample_and_update_state(frame_num: usize, sample_T: type, audio_output_buffer: [*]c.BYTE, play_state: *sound_play_state) void {
    const sizeof = if (sample_T == i24) 3 else @sizeOf(sample_T);
    if (play_state.wav_file.num_channels > 1) { // file is not mono
        for (0..play_state.wav_file.num_channels) |channel_index| {
            var sample: sample_T = undefined;
            @memcpy(@as([*]u8, @ptrCast(&sample)), play_state.wav_file.pcm_data[play_state.sample_index + channel_index * sizeof .. play_state.sample_index + channel_index * sizeof + sizeof]);
            const formatted_sample = convert_sample_to_output_format(sample_T, sample);
            const output_channel_index_offset = BYTES_PER_SAMPLE * channel_index;
            buffer_sum(audio_output_buffer[frame_num * FRAME_SIZE + output_channel_index_offset ..], formatted_sample);
        }
    } else { // file is in mono
        var sample: sample_T = undefined;
        @memcpy(@as([*]u8, @ptrCast(&sample)), play_state.wav_file.pcm_data[play_state.sample_index .. play_state.sample_index + sizeof]);
        const formatted_sample = convert_sample_to_output_format(sample_T, sample);
        buffer_sum(audio_output_buffer[frame_num * FRAME_SIZE ..], formatted_sample);
        buffer_sum(audio_output_buffer[frame_num * FRAME_SIZE + BYTES_PER_SAMPLE ..], formatted_sample);
    }

    play_state.float_counter += play_state.sample_rate_ratio;
    while (play_state.float_counter > 1.0) {
        play_state.float_counter -= 1.0;
        play_state.sample_index += play_state.wav_file.block_align;
    }
    if (play_state.sample_index >= play_state.wav_file.pcm_data.len) {
        if (play_state.mode == .loop) {
            play_state.sample_index = 0;
        } else if (play_state.mode == .one_shot) {
            play_state.alive = false;
            play_state.sample_index = 0;
        }
    }
}
fn buffer_sum(buffer_data: [*]c.BYTE, formatted_sample: f32) void {
    var current_output: f32 = undefined;
    @memcpy(@as([*]u8, @ptrCast(&current_output)), buffer_data[0..4]);
    current_output += formatted_sample;
    @memcpy(buffer_data[0..4], @as([*]u8, @ptrCast(&current_output)));
}

var play_states: [200]sound_play_state = undefined;
var play_states_count: usize = 0;
pub var message_queue: [32]sound_play_state = undefined;
pub var message_queue_count_WRITERS: usize = 0;
pub var message_queue_count_READER: usize = 0;
var message_queue_start: usize = 0;
fn get_message(i: usize) *sound_play_state {
    return &message_queue[i % 32];
}

pub fn audio_engine_run() !void {
    var windows_error: c.HRESULT = undefined;

    windows_error = c.CoInitializeEx(null, c.COINIT_MULTITHREADED);
    try windows_check_error(windows_error);

    var enumerator: *c.IMMDeviceEnumerator = undefined;
    windows_error = c.CoCreateInstance(&c.CLSID_MMDeviceEnumerator, null, c.CLSCTX_ALL, &c.IID_IMMDeviceEnumerator, @ptrCast(&enumerator));
    try windows_check_error(windows_error);

    var device: *c.IMMDevice = undefined;
    windows_error = enumerator.lpVtbl.*.GetDefaultAudioEndpoint.?(enumerator, c.eRender, c.eConsole, @ptrCast(&device));
    try windows_check_error(windows_error);

    var audio_client: *c.IAudioClient = undefined;
    windows_error = device.lpVtbl.*.Activate.?(device, &c.IID_IAudioClient, c.CLSCTX_ALL, null, @ptrCast(&audio_client));
    try windows_check_error(windows_error);

    var pwfx: *c.WAVEFORMATEX = undefined;
    windows_error = audio_client.lpVtbl.*.GetMixFormat.?(audio_client, @ptrCast(&pwfx));
    try windows_check_error(windows_error);

    var default_period: c.REFERENCE_TIME = undefined;
    var min_period: c.REFERENCE_TIME = undefined;
    windows_error = audio_client.lpVtbl.*.GetDevicePeriod.?(audio_client, &default_period, &min_period);
    try windows_check_error(windows_error);

    windows_error = audio_client.lpVtbl.*.Initialize.?(audio_client, c.AUDCLNT_SHAREMODE_SHARED, c.AUDCLNT_STREAMFLAGS_EVENTCALLBACK, min_period, 0, @ptrCast(pwfx), null);
    try windows_check_error(windows_error);

    var render_service: *c.IAudioRenderClient = undefined;
    windows_error = audio_client.lpVtbl.*.GetService.?(audio_client, &c.IID_IAudioRenderClient, @ptrCast(&render_service));
    try windows_check_error(windows_error);

    var buffer_frame_count: c.UINT32 = undefined;
    windows_error = audio_client.lpVtbl.*.GetBufferSize.?(audio_client, @ptrCast(&buffer_frame_count));
    try windows_check_error(windows_error);

    const audio_event_handle = c.CreateEventA(null, 0, 0, null);
    if (audio_event_handle == null) {
        std.debug.print("Failed to create event\n", .{});
        return;
    }
    windows_error = audio_client.lpVtbl.*.SetEventHandle.?(audio_client, audio_event_handle);
    try windows_check_error(windows_error);

    windows_error = audio_client.lpVtbl.*.Start.?(audio_client);
    try windows_check_error(windows_error);

    BYTES_PER_SAMPLE = (pwfx.wBitsPerSample / 8);
    FRAME_SIZE = @intCast(pwfx.nChannels * BYTES_PER_SAMPLE);
    std.debug.assert(pwfx.nChannels >= 2);

    while (true) {
        _ = c.WaitForSingleObject(audio_event_handle, c.INFINITE);

        // retrieve the memory info for writing audio
        var num_frames_padding: c.UINT32 = undefined;
        windows_error = audio_client.lpVtbl.*.GetCurrentPadding.?(audio_client, @ptrCast(&num_frames_padding));
        try windows_check_error(windows_error);

        const num_frames_writable = buffer_frame_count - num_frames_padding;

        var audio_output_buffer: [*]c.BYTE = undefined;
        windows_error = render_service.lpVtbl.*.GetBuffer.?(render_service, num_frames_writable, @ptrCast(&audio_output_buffer));
        try windows_check_error(windows_error);

        // cleanup playstates
        var curr_play_state: usize = 0;
        while (curr_play_state < play_states_count) {
            if (!play_states[curr_play_state].alive) {
                if (curr_play_state < play_states_count - 1) play_states[curr_play_state] = play_states[play_states_count - 1];
                play_states_count -= 1;
            } else {
                curr_play_state += 1;
            }
        }

        // enact all recieved messages by updating playstates list
        const atomically_requested_message_spot = @atomicLoad(usize, &message_queue_count_READER, std.builtin.AtomicOrder.monotonic);
        var curr: usize = message_queue_start;
        while (curr < atomically_requested_message_spot) {
            if (play_states_count < play_states.len - 1) {
                play_states[play_states_count] = get_message(curr).*;
                play_states[play_states_count].sample_rate_ratio = @as(f32, @floatFromInt(play_states[play_states_count].wav_file.sample_rate)) / @as(f32, @floatFromInt(pwfx.nSamplesPerSec));
                play_states_count += 1;
            }

            curr += 1;
        }
        message_queue_start = atomically_requested_message_spot;

        // actual audio processing - mix all the currently playing sounds into the output audio buffer
        for (0..num_frames_writable) |frame_num| {
            // start with zeros - silent audio
            @memset(audio_output_buffer[frame_num * FRAME_SIZE + 0 .. frame_num * FRAME_SIZE + BYTES_PER_SAMPLE], 0);
            @memset(audio_output_buffer[frame_num * FRAME_SIZE + BYTES_PER_SAMPLE .. frame_num * FRAME_SIZE + BYTES_PER_SAMPLE + BYTES_PER_SAMPLE], 0);
            // mix in the various playing sounds
            for (play_states[0..play_states_count]) |*play_state| {
                std.debug.assert(play_state.wav_file.num_channels <= pwfx.nChannels);
                if (!play_state.alive) continue;
                if (play_state.wav_file.bits_per_sample == 8) {
                    process_sample_and_update_state(frame_num, u8, audio_output_buffer, play_state);
                } else if (play_state.wav_file.bits_per_sample == 16) {
                    process_sample_and_update_state(frame_num, i16, audio_output_buffer, play_state);
                } else if (play_state.wav_file.bits_per_sample == 24) {
                    process_sample_and_update_state(frame_num, i24, audio_output_buffer, play_state);
                } else {
                    std.debug.print("audio file has a samplerate that is not currently supported: {}\n", .{play_state.wav_file.bits_per_sample});
                    unreachable;
                }
            }
        }

        windows_error = render_service.lpVtbl.*.ReleaseBuffer.?(render_service, num_frames_writable, 0);
        try windows_check_error(windows_error);
    }

    windows_error = audio_client.lpVtbl.*.Stop.?(audio_client);
    try windows_check_error(windows_error);
}
pub fn audio_engine_cleanup() void {
    c.CoUninitialize();
}
