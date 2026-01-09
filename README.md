# LeanVerif

LeanVerif is an opportunistic Lean 4 port of ProVerif: full semantics preservation, with Lean-native structures and proofs when they pay off locally.

- GitHub org: https://github.com/Beneficial-AI-Foundation
- Docs: Verso (`doc.verso` enabled in `lakefile.lean`)

See `AGENTS.md` for the porting plan and dependency order.

## ProVerif Baseline Tests

The Lake script `proverifTest` builds the vendored ProVerif (2.05, no-interactive build)
and compares outputs against baseline files in `tests/proverif/baseline`.

```
lake run proverifTest
lake run proverifTest --update
```
