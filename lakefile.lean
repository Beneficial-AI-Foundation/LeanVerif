import Lake
open Lake DSL

require verso from git "https://github.com/leanprover/verso" @ "main"

package LeanVerif where
  leanOptions := #[
    ⟨`doc.verso, true⟩
  ]

@[default_target]
lean_lib LeanVerif

@[default_target]
lean_exe leanverif where
  root := `Main
