import Lean.Data.RBMap

namespace LeanVerif.Proverif.Tree

structure OrderedType where
  t_fst : Type
  t_snd : Type
  compare_fst : t_fst → t_fst → Int
  compare_snd : t_snd → t_snd → Int

namespace OrderedType
abbrev t (Ord : OrderedType) := Ord.t_fst × Ord.t_snd
end OrderedType

structure OrderedTypeOne where
  t : Type
  compare : t → t → Int

namespace Make

variable (Ord : OrderedType)

local notation "Key" => OrderedType.t Ord

private def compare_lex (k1 k2 : Key) : Ordering :=
  let c1 := Ord.compare_fst k1.1 k2.1
  if c1 < 0 then
    Ordering.lt
  else if c1 > 0 then
    Ordering.gt
  else
    let c2 := Ord.compare_snd k1.2 k2.2
    if c2 < 0 then
      Ordering.lt
    else if c2 > 0 then
      Ordering.gt
    else
      Ordering.eq

abbrev t (α : Type) := Lean.RBMap Key α (compare_lex Ord)

private def empty' {α : Type} : t Ord α :=
  (Lean.RBMap.empty : Lean.RBMap Key α (compare_lex Ord))

/-- The empty map. -/
def empty {α : Type} : t Ord α := empty' (Ord := Ord)

/-- Test whether a map is empty. -/
def is_empty {α : Type} (m : t Ord α) : Bool :=
  m.isEmpty

/-- Update a binding. -/
def update {α : Type} (k : Key) (f : Option α → Option α) (m : t Ord α) : t Ord α :=
  match f (m.find? k) with
  | none => m.erase k
  | some v => m.insert k v

/-- Singleton map. -/
def singleton {α : Type} (k : Key) (v : α) : t Ord α :=
  (empty' (Ord := Ord)).insert k v

/-- Apply {lit}`f` to each value in order. -/
def iter {α : Type} (f : α → Unit) (m : t Ord α) : Unit :=
  m.fold (init := ()) (fun acc _ v =>
    let _ := f v
    acc)

/-- Apply {lit}`f_ge`/{lit}`f_eq` to keys ≥ {lit}`k` with fst > / fst =. -/
def iter_geq {α : Type} (f_ge : α → Unit) (f_eq : α → Unit) (k : Key) (m : t Ord α) : Unit :=
  m.fold (init := ()) (fun acc k' v =>
    let c1 := Ord.compare_fst k'.1 k.1
    if c1 > 0 then
      let _ := f_ge v
      acc
    else if c1 = 0 then
      let c2 := Ord.compare_snd k'.2 k.2
      if c2 >= 0 then
        let _ := f_eq v
        acc
      else
        acc
    else
      acc)

private def remove_until (v1 : Ord.t_fst) : List Key → Option (Ord.t_snd × List Key) × List Key
  | [] => (none, [])
  | l@((v1', v2') :: q) =>
      let c := Ord.compare_fst v1 v1'
      if c = 0 then
        (some (v2', q), l)
      else if c < 0 then
        remove_until (v1 := v1) q
      else
        (none, l)

/-- Existence predicate for {lit}`exists_leq`. -/
def exists_leq {α : Type} (p : List Key → α → Bool) (k : Key) (k_list : List Key) (m : t Ord α) : Bool :=
  let entries := m.toList
  entries.any (fun kv =>
    let (k', v) := kv
    let c1 := Ord.compare_fst k'.1 k.1
    if c1 = 0 then
      let c2 := Ord.compare_snd k'.2 k.2
      c2 <= 0 && p k_list v
    else if c1 < 0 then
      match remove_until (Ord := Ord) k'.1 k_list with
      | (some (k2', tail), _) => Ord.compare_snd k'.2 k2' <= 0 && p tail v
      | _ => false
    else
      false)

/-- Update all bindings by applying {lit}`f`. -/
def update_all {α : Type} (f : α → Option α) (m : t Ord α) : t Ord α :=
  m.fold (init := empty' (Ord := Ord)) (fun acc k v =>
    match f v with
    | none => acc
    | some v' => acc.insert k v')

end Make

namespace MakeOne

variable (Ord : OrderedTypeOne)

local notation "Key" => Ord.t

private def compare_one (a b : Key) : Ordering :=
  let c := Ord.compare a b
  if c < 0 then
    Ordering.lt
  else if c > 0 then
    Ordering.gt
  else
    Ordering.eq

abbrev t (α : Type) := Lean.RBMap Key α (compare_one Ord)

private def empty' {α : Type} : t Ord α :=
  (Lean.RBMap.empty : Lean.RBMap Key α (compare_one Ord))

/-- The empty map. -/
def empty {α : Type} : t Ord α := empty' (Ord := Ord)

/-- Test whether a map is empty. -/
def is_empty {α : Type} (m : t Ord α) : Bool :=
  m.isEmpty

/-- Update a binding. -/
def update {α : Type} (k : Key) (f : Option α → Option α) (m : t Ord α) : t Ord α :=
  match f (m.find? k) with
  | none => m.erase k
  | some v => m.insert k v

/-- Singleton map. -/
def singleton {α : Type} (k : Key) (v : α) : t Ord α :=
  (empty' (Ord := Ord)).insert k v

/-- Find the value bound to {lit}`k`, if any. -/
def find_opt {α : Type} (k : Key) (m : t Ord α) : Option α :=
  m.find? k

/-- Iterate over all bindings in order using {lit}`f`. -/
def iter {α : Type} (f : Key → α → Unit) (m : t Ord α) : Unit :=
  m.fold (init := ()) (fun acc k v =>
    let _ := f k v
    acc)

/-- Update all bindings by applying {lit}`f`. -/
def update_all {α : Type} (f : α → Option α) (m : t Ord α) : t Ord α :=
  m.fold (init := empty' (Ord := Ord)) (fun acc k v =>
    match f v with
    | none => acc
    | some v' => acc.insert k v')

end MakeOne

end LeanVerif.Proverif.Tree
