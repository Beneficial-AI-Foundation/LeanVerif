# Parser Plan

Goal: Port the lexer/parser pipeline (`lexer.mll`, `parser.mly`, `pilexer.mll`, `piparser.mly`, `pitlexer.mll`, `pitparser.mly`, and TeX lexers) to Lean while preserving ProVerif 2.05 behavior.

## Primary approach: Lean Parsec
- Implement lexers and parsers using Lean's Parsec-based combinators.
- Keep error reporting wired through `ParsingHelper.extent`.
- Encode precedence/associativity explicitly in combinators (or layered parsers).
- Build a shared token layer for PI, PIT, and main syntax where feasible.

## Fallback approach
If Parsec cannot match the grammar complexity (or becomes too slow), we will:
- Either bind to a generated parser (Menhir/ocamlyacc) via a thin FFI boundary, or
- Use a dedicated parser generator in Lean (if available/appropriate) and keep the interface stable.

## Decision criteria
- Fidelity to 2.05 parsing behavior and error localization.
- Maintainability of the grammar in Lean.
- Performance on large inputs.

Short term: keep Parsec as the default plan; reassess after prototyping the main lexer/parser pair.
