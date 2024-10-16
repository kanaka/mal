import Lake
open Lake DSL

package "mal" where
  -- Settings applied to both builds and interactive editing
  leanOptions := #[
    ⟨`pp.unicode.fun, true⟩ -- pretty-prints `fun a ↦ b`
  ]
  -- add any additional package configuration options here

require Parser from git "https://github.com/fgdorais/lean4-parser" @ "main"

@[default_target]
lean_lib LeanMal where
  -- add any library configuration options here

@[default_target]
lean_exe "step0_repl" {
  root := `LeanMal.step0_repl
}

@[default_target]
lean_exe "step1_read_print" {
  root := `LeanMal.step1_read_print
}

@[default_target]
lean_exe "step2_eval" {
  root := `LeanMal.step2_eval
}

@[default_target]
lean_exe "step3_env" {
  root := `LeanMal.step3_env
}

@[default_target]
lean_exe "step4_if_fn_do" {
  root := `LeanMal.step4_if_fn_do
}

@[default_target]
lean_exe "step5_tco" {
  root := `LeanMal.step5_tco
}

@[default_target]
lean_exe "step6_file" {
  root := `LeanMal.step6_file
}

@[default_target]
lean_exe "step7_quote" {
  root := `LeanMal.step7_quote
}

@[default_target]
lean_exe "step8_macros" {
  root := `LeanMal.step8_macros
}

@[default_target]
lean_exe "step9_try" {
  root := `LeanMal.step9_try
}

@[default_target]
lean_exe "stepA_mal" {
  root := `LeanMal.stepA_mal
}

@[default_target]
lean_exe "mal" {
  root := `LeanMal.step1_read_print
}
