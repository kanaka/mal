const std = @import("std");
const LibExeObjStep = std.build.LibExeObjStep;

pub fn build(b: *std.build.Builder) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();

    for ([_]*LibExeObjStep{
        b.addExecutable("step0_repl", "src/step0_repl.zig"),
        b.addExecutable("step1_read_print", "src/step1_read_print.zig"),
        b.addExecutable("step2_eval", "src/step2_eval.zig"),
        b.addExecutable("step3_env", "src/step3_env.zig"),
        b.addExecutable("step4_if_fn_do", "src/step4_if_fn_do.zig"),
        b.addExecutable("step5_tco", "src/step5_tco.zig"),
    }) |exe| {
        exe.setTarget(target);
        exe.setBuildMode(mode);
        exe.setOutputDir(".");
        b.installArtifact(exe);
        const run_cmd = exe.run();
        if (b.args) |args| {
            run_cmd.addArgs(args);
        }
        const run_step = b.step(exe.name, exe.name);
        run_step.dependOn(&run_cmd.step);
    }
}
