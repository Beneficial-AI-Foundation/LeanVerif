# ProVerif Dependency Graph (2.05)

Source snapshot: ProVerif 2.05 vendored at `vendor/proverif-2.05` (tarball stored as `vendor/proverif-2.05.tar.gz`).

How this graph was derived:
- Run `ocamldep -one-line -I src *.ml` in `vendor/proverif-2.05/src`.
- Convert `.cmi`/`.cmx` dependencies into a module DAG.
- Topologically sort that DAG to get a port order.

Notes:
- Interface-only modules (present as `.mli` but no `.ml`) are foundational and should be ported first:
  `types`, `ptree`, `pitypes`, `piptree`, `pitptree`.
- Lexer/parser generators exist in `src/` (`lexer.mll`, `parser.mly`, `pilexer.mll`, `piparser.mly`,
  `pitlexer.mll`, `pitparser.mly`, `lexertotex.mll`, `pitlexertotex.mll`). These should be ported
  as Lean parsers after the core AST/type modules.

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

Graph file: `docs/proverif-deps.dot` (Graphviz DOT).
