# AGENTS

## Project
- Name: LeanVerif
- Owner: Alok Singh
- GitHub org: https://github.com/Beneficial-AI-Foundation

## Goal
Port ProVerif to Lean 4 — all of it. This is an opportunistic port: preserve semantics, but use Lean-native features whenever they provide local wins (types, inductive definitions, termination, proofs, namespaces, and syntactic clarity).

## Documentation System
- Use Verso for docs (`doc.verso` enabled in `lakefile.lean`).
- Keep doc comments (`/-- ... -/`) on all public definitions.

## ProVerif Dependency Graph (port order)
Computed from ProVerif 2.05 using `ocamldep` in `vendor/proverif-2.05/src` (see `docs/proverif-deps.md` and `docs/proverif-deps.dot`).

Foundational interface-only modules (port first):
- types
- ptree
- pitypes
- piptree
- pitptree

Topological port order (from ocamldep):
1. fileprint
2. stringmap
3. funsymbhash
4. stringPlus
5. parsing_helper
6. pvqueue
7. tree
8. version
9. myocamlbuild
10. addexpectedtags
11. analyze
12. param
13. proveriftotex
14. terms
15. termslinks
16. move_new_let
17. display
18. display_interact
19. noninterf
20. proswapper
21. spassout
22. termsEq
23. pievent
24. destructor
25. reduction_helper
26. weaksecr
27. evaluation_helper
28. convert_repl
29. lemma
30. history
31. simplify
32. reduction_bipro
33. database
34. encode_queries
35. menu_helper
36. syntax
37. pitsyntax
38. pisyntax
39. tsyntax
40. selfun
41. reduction_interact
42. pitranslweak
43. rules
44. pitransl
45. menu_interact
46. reduction
47. main_interact
48. piauth
49. main

Parser/lexer pipeline to port after core AST/type modules:
- lexer.mll / parser.mly (main language)
- pilexer.mll / piparser.mly (PI language)
- pitlexer.mll / pitparser.mly (typechecking language)
- lexertotex.mll / pitlexertotex.mll (TeX backends)

## Porting Principles
- Keep semantics faithful; use Lean structures/inductives where it clarifies invariants.
- Prefer computable definitions; avoid noncomputable unless strictly necessary.
- Build modules in dependency order and keep the graph updated as the port evolves.

## Status (initial port)
- ParsingHelper.extent stubbed with minimal structure.
- Types currently defines `nounif_value` only (needed by PTree).
- PTree (untyped/typed parse tree) ported.
- PIPTree and PITPTree (untyped/typed pi-tree) ported.
