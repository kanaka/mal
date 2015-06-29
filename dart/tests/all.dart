library all_tests;

import "step0_repl_test.dart" as step0;
import "step1_read_print_test.dart" as step1;
import "step2_eval_test.dart" as step2;
import "step3_env_test.dart" as step3;
import "step4_if_fn_do_test.dart" as step4;
import "step5_tco_test.dart" as step5;
import "step6_file_test.dart" as step6;
import "step7_quote_test.dart" as step7;
import "step8_macros_test.dart" as step8;
import "step9_try_test.dart" as step9;
import "stepA_mal_test.dart" as stepA;

import "env_test.dart" as env;
import "types_test.dart" as types;
import "core_test.dart" as core;

void main() {
  step0.main();
  step1.main();
  step2.main();
  step3.main();
  step4.main();
  step5.main();
  step6.main();
  step7.main();
  step8.main();
  step9.main();
  stepA.main();

  types.main();
  env.main();
  core.main();
}
