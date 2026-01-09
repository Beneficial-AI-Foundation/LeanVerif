namespace LeanVerif.Proverif.Types

-- TODO: port the full Types module. For now, provide minimal definitions
-- needed by the untyped parse tree (PTree).

inductive nounif_value
  | noUnifNegDefault
  | noUnifPosDefault
  | noUnifValue (value : Int)
  deriving Repr, BEq, Inhabited

end LeanVerif.Proverif.Types
