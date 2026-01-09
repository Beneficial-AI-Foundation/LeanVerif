import LeanVerif.Proverif.Types

namespace LeanVerif.Proverif.Funsymbhash

/-- Placeholder hash table for function symbols. -/
abbrev HashtblSymbol (α : Type) := List (LeanVerif.Proverif.Types.funsymb × α)

end LeanVerif.Proverif.Funsymbhash
