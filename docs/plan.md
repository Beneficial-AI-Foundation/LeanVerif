# LeanVerif Port Plan (ProVerif 2.05)

## Scope
Port ProVerif 2.05 to Lean 4 with strict module mapping (one Lean module per OCaml module, same name and behavior), using Lean-native features where they give local wins while preserving semantics.

## Phases
1) **Bootstrap + Helpers**
   - Finish dependency-ordered helpers: `stringPlus`, `parsing_helper`, `pvqueue`, `tree`, `version`, then `myocamlbuild`, `addexpectedtags`, `analyze`, `param`, `proveriftotex`.
   - Keep `Types.Mut` as a placeholder and isolate runtime mutability in `IO.Ref`.

2) **Core Terms + Display Pipeline**
   - Port `terms`, `termslinks`, `move_new_let`, `display`, `display_interact`, `noninterf`, `proswapper`, `spassout`, `termsEq`, `pievent`, `destructor`.
   - Add lightweight tests to keep term normalization and pretty-printing stable.

3) **Reduction + Query Engine**
   - Port `reduction_helper`, `evaluation_helper`, `simplify`, `reduction_bipro`, `database`, `encode_queries`, `rules`, `reduction`, `reduction_interact`, `menu_helper`, `menu_interact`.
   - Introduce explicit state records for mutable algorithmic state.

4) **Parsing + Syntax**
   - Implement `syntax`, `pitsyntax`, `pisyntax`, `tsyntax` and `selfun`.
   - Build the lexer/parser pipeline with Lean Parsec combinators; if needed, fall back to external parser generation/FFI (see `docs/parser-plan.md`).

5) **Front-End + Main**
   - Port `pitranslweak`, `pitransl`, `main_interact`, `piauth`, `main`.
   - Wire CLI + doc output parity with ProVerif 2.05.

## Validation
- Use the Lake scripts (`proverifBuild`, `proverifTest`) to build vendored ProVerif and compare outputs against baseline test cases.
- Expand `tests/proverif/cases.txt` as new modules stabilize.
