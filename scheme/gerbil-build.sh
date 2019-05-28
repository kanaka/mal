#!/bin/sh

export GERBIL_LOADPATH=gxc.out

mkdir -p gxc.out/bin
gxc -d gxc.out -O lib/util.sld lib/types.sld lib/env.sld lib/reader.sld lib/printer.sld lib/core.sld
for x in step0_repl step1_read_print step2_eval step3_env step4_if_fn_do step5_tco step6_file step7_quote step8_macros step9_try stepA_mal; do
    gxc -d gxc.out -O -exe -o gxc.out/bin/$x $x.scm
done
