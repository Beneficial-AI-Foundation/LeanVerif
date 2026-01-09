namespace LeanVerif.Proverif.ParsingHelper

structure Extent where
  source : Option String := none
  startPos : Nat := 0
  endPos : Nat := 0
deriving Repr, BEq, Inhabited

abbrev extent := Extent

def dummy_ext : extent := {}

def merge_ext (a b : extent) : extent :=
  { source := if a.source = b.source then a.source else none
    startPos := Nat.min a.startPos b.startPos
    endPos := Nat.max a.endPos b.endPos }

end LeanVerif.Proverif.ParsingHelper
