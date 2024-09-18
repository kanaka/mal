const Builder = @import("std").Build;

pub fn build(b: *Builder) void {

    //  Two options select the built step.

    const name = b.option([]const u8, "name", "step name (without .zig)")
                 orelse "stepA_mal";

    const root_source_file = b.path(
        b.option([]const u8, "root_source_file", "step name (with .zig)")
        orelse "stepA_mal.zig");

    const exe = b.addExecutable(.{
        .name = name,
        .root_source_file = root_source_file,
        .target = b.standardTargetOptions(.{}),
        .optimize = b.standardOptimizeOption(.{}),
    });

    exe.linkSystemLibrary("c");
    exe.linkSystemLibrary("pcre");
    exe.linkSystemLibrary("readline");
    b.default_step.dependOn(&exe.step);
    b.installArtifact(exe);
}
