const LibExeObjStep = @import("std").build.LibExeObjStep;
const Builder = @import("std").build.Builder;
const builtin = @import("builtin");

const warn = @import("std").debug.warn;

pub fn build(b: *Builder) void {
    const mode = b.standardReleaseOptions();

    const exes = [_] *LibExeObjStep {
        b.addExecutable("step0_repl",       "step0_repl.zig"),
        b.addExecutable("step1_read_print", "step1_read_print.zig"),
        b.addExecutable("step2_eval",       "step2_eval.zig"),
        b.addExecutable("step3_env",        "step3_env.zig"),
        b.addExecutable("step4_if_fn_do",   "step4_if_fn_do.zig"),
        b.addExecutable("step5_tco",        "step5_tco.zig"),
        b.addExecutable("step6_file",       "step6_file.zig"),
        b.addExecutable("step7_quote",      "step7_quote.zig"),
        b.addExecutable("step8_macros",     "step8_macros.zig"),
        b.addExecutable("step9_try",        "step9_try.zig"),
        b.addExecutable("stepA_mal",        "stepA_mal.zig"),
    };

    for(exes) |exe| {
        exe.setBuildMode(mode);
        exe.linkSystemLibrary("c");
        exe.linkSystemLibrary("pcre");
        exe.linkSystemLibrary("readline");
        const run_cmd = exe.run();
        const step = b.step(exe.name, exe.name);
        step.dependOn(&run_cmd.step);
        b.default_step.dependOn(&exe.step);
        b.installArtifact(exe);
    }
}
