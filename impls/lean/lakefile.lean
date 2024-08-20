import Lake
open Lake DSL

package "mal" where
  -- Settings applied to both builds and interactive editing
  leanOptions := #[
    ⟨`pp.unicode.fun, true⟩ -- pretty-prints `fun a ↦ b`
  ]
  -- add any additional package configuration options here

require "leanprover-community" / "mathlib"

require Parser from git "https://github.com/fgdorais/lean4-parser" @ "main"

@[default_target]
lean_lib LeanMal where
  -- add any library configuration options here

@[default_target]
lean_exe "step0_repl" {
  root := `LeanMal.step0_repl
}
