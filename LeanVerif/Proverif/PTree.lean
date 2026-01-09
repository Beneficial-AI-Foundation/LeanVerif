import LeanVerif.Proverif.ParsingHelper
import LeanVerif.Proverif.Types

namespace LeanVerif.Proverif.PTree

abbrev ident := String × ParsingHelper.extent

inductive term
  | PIdent : ident → term
  | PFunApp : ident → List term → term
  | PTuple : List term → term
  | PName : ident → List term → term
  deriving Repr, BEq, Inhabited

inductive format
  | PFIdent : ident → format
  | PFFunApp : ident → List format → format
  | PFTuple : List format → format
  | PFName : ident → List format → format
  | PFAny : ident → format
  deriving Repr, BEq, Inhabited

inductive fact
  | PSimpleFact : ident → List term → fact
  | PSNeq : term → term → fact
  deriving Repr, BEq, Inhabited

abbrev fact_e := fact × ParsingHelper.extent
abbrev format_fact := ident × List format

inductive clause
  | Clause : List fact_e → fact_e → clause
  | Equiv : List fact_e → fact_e → Bool → clause
  deriving Repr, BEq, Inhabited

inductive pval
  | S : ident → pval
  | I : Int → pval
  deriving Repr, BEq, Inhabited

inductive decl
  | FunDecl : ident → Nat → decl
  | DataFunDecl : ident → Nat → decl
  | Equation : List (term × term) → decl
  | Query : fact_e → decl
  | NoUnif : format_fact → Types.nounif_value → decl
  | Not : fact_e → decl
  | Elimtrue : fact_e → decl
  | PredDecl : ident → Nat → List ident → decl
  | Param : ident → pval → decl
  | Reduc : List clause → decl
  deriving Repr, BEq, Inhabited

abbrev envdecl := List (ident × ident)

inductive tdecl
  | TTypeDecl : ident → tdecl
  | TNameDecl : ident → ident → tdecl
  | TFunDecl : ident → List ident → ident → List ident → tdecl
  | TConstDecl : ident → ident → List ident → tdecl
  | TEquation : List (envdecl × term × term) → List ident → tdecl
  | TQuery : envdecl → fact_e → tdecl
  | TNoUnif : envdecl → format_fact → Types.nounif_value → tdecl
  | TNot : envdecl → fact_e → tdecl
  | TElimtrue : envdecl → fact_e → tdecl
  | TPredDecl : ident → List ident → List ident → tdecl
  | TSet : ident → pval → tdecl
  | TReduc : List (envdecl × clause) → tdecl
  deriving Repr, BEq, Inhabited

end LeanVerif.Proverif.PTree
