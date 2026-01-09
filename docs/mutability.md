# Mutability Strategy

ProVerif uses mutable fields and global refs in several places. The current port splits mutable state into two tiers:

## Inventory (ProVerif 2.05)
Mutability shows up in these modules (non-exhaustive):
- `types.mli`: mutable fields on `predicate`, `renamable_id`, `binder`, `name_info`, `funsymb`, `fact_tree`.
- `pitypes.mli`: mutable fields on `eventstatus`, `t_process_desc`, and grouped lemma/axiom summaries.
- `pvqueue.ml`: mutable queue pointers.
- `database.ml`: multiple mutable fields in clause elements, queues, and accounting stats.
- `rules.ml`: mutable fields in `t_pred`.
- `analyze.ml`: mutable timing fields.
- `parsing_helper.ml` / `fileprint.ml`: module-level refs for interactive warnings and output state.

(See `rg -n "mutable" vendor/proverif-2.05/src` for exact locations.)

## Decision
1) **Core AST/types stay pure for now.**
   - `Types.Mut` is currently an alias to the underlying value type, so the mutual inductive block remains strictly positive.
   - This keeps type definitions lightweight and allows the rest of the port to proceed.

2) **Module-level runtime state uses `IO.Ref`.**
   - Modules like `ParsingHelper`, `Fileprint`, and `Pvqueue` are ported with explicit `IO.Ref` state.

3) **Algorithmic state will move to explicit state records.**
   - For modules like `database`, `rules`, and `reduction`, we will introduce explicit state records (or a dedicated `StateM`/`IO` layer) to host the mutable fields.
   - When needed, we will externalize `Mut` fields into these state records and access them by stable IDs (e.g. indices on symbols or clauses).

This approach preserves semantics while keeping the core AST definitional and avoiding Lean positivity issues.
