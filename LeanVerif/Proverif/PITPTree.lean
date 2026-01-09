import LeanVerif.Proverif.ParsingHelper
import LeanVerif.Proverif.PTree
import LeanVerif.Proverif.Types

namespace LeanVerif.Proverif.PITPTree

abbrev ident := PTree.ident
abbrev options := ident × Option (List ident)

inductive term
  | PIdent : ident → term
  | PFail : term
  | PFunApp : ident → List (term × ParsingHelper.extent) → term
  | PProj : ident → (term × ParsingHelper.extent) → term
  | PTuple : List (term × ParsingHelper.extent) → term

abbrev term_e := term × ParsingHelper.extent

inductive extended_equation
  | EELet : ident → term_e → extended_equation → extended_equation
  | EETerm : term_e → extended_equation

abbrev equation := term_e × term_e
abbrev fundef := List (term_e × term_e)

inductive gformat
  | PFGIdent : ident → gformat
  | PFGFunApp : ident → List (gformat × ParsingHelper.extent) → gformat
  | PFGTuple : List (gformat × ParsingHelper.extent) → gformat
  | PFGName : ident → List (ident × (gformat × ParsingHelper.extent)) → gformat
  | PFGAny : ident → gformat
  | PFGLet : ident → (gformat × ParsingHelper.extent) → (gformat × ParsingHelper.extent) → gformat

abbrev gformat_e := gformat × ParsingHelper.extent

inductive nounif_t
  | BFLet : ident → gformat_e → nounif_t → nounif_t
  | BFNoUnif : (ident × List gformat_e × Int) → nounif_t

inductive gterm
  | PGIdent : ident → gterm
  | PGFunApp : ident → List (gterm × ParsingHelper.extent) → Option ident → gterm
  | PGPhase : ident → List (gterm × ParsingHelper.extent) → Int → Option ident → gterm
  | PGTuple : List (gterm × ParsingHelper.extent) → gterm
  | PGName : ident → List (ident × (gterm × ParsingHelper.extent)) → gterm
  | PGLet : ident → (gterm × ParsingHelper.extent) → (gterm × ParsingHelper.extent) → gterm

abbrev gterm_e := gterm × ParsingHelper.extent

inductive tquery
  | PPutBegin : Bool → List ident → tquery
  | PRealQuery : gterm_e → List ident → tquery
  | PQSecret : ident → List ident → List options → tquery

abbrev tquery_e := tquery × ParsingHelper.extent

inductive lemma_kind
  | KAxiom
  | KLemma
  | KRestriction

abbrev tlemma := gterm_e × Option (ident × ident) × List ident

inductive tclause
  | PClause : term_e → term_e → tclause
  | PFact : term_e → tclause
  | PEquiv : term_e → term_e → Bool → tclause

mutual
  inductive pterm
    | PPIdent : ident → pterm
    | PPFunApp : ident → List pterm_e → pterm
    | PPTuple : List pterm_e → pterm
    | PPRestr : ident → Option (List ident) → ident → pterm_e → pterm
    | PPTest : pterm_e → pterm_e → Option pterm_e → pterm
    | PPLet : tpattern → pterm_e → pterm_e → Option pterm_e → pterm
    | PPLetFilter : List (ident × ident) → pterm_e → pterm_e → Option pterm_e → pterm
    | PPEvent : ident → List pterm_e → Option (List ident) → pterm_e → pterm
    | PPInsert : ident → List pterm_e → pterm_e → pterm
    | PPGet : ident → List tpattern → Option pterm_e → pterm_e → Option pterm_e → List options → pterm

  structure pterm_e where
    term : pterm
    extent : ParsingHelper.extent

  inductive tpattern
    | PPatVar : ident → Option ident → tpattern
    | PPatAny : ParsingHelper.extent → Option ident → tpattern
    | PPatTuple : List tpattern → tpattern
    | PPatFunApp : ident → List tpattern → tpattern
    | PPatChoice : ident → List tpattern → Option ident → tpattern
    | PPatEqual : pterm_e → tpattern
end

mutual
  inductive tprocess
    | PNil : tprocess
    | PPar : tprocess_e → tprocess_e → tprocess
    | PRepl : tprocess_e → tprocess
    | PRestr : ident → Option (List ident) → ident → tprocess_e → tprocess
    | PLetDef : ident → List pterm_e → Option ident → tprocess
    | PTest : pterm_e → tprocess_e → tprocess_e → tprocess
    | PInput : pterm_e → tpattern → tprocess_e → List options → tprocess
    | POutput : pterm_e → pterm_e → tprocess_e → tprocess
    | PLet : tpattern → pterm_e → tprocess_e → tprocess_e → tprocess
    | PLetFilter : List (ident × ident) → pterm_e → tprocess_e → tprocess_e → List options → tprocess
    | PEvent : ident → List pterm_e → Option (List ident) → tprocess_e → tprocess
    | PPhase : Int → tprocess_e → tprocess
    | PBarrier : Int → Option ident → tprocess_e → tprocess
    | PInsert : ident → List pterm_e → tprocess_e → tprocess
    | PGet : ident → List tpattern → Option pterm_e → tprocess_e → tprocess_e → List options → tprocess

  structure tprocess_e where
    proc : tprocess
    extent : ParsingHelper.extent
end

abbrev envdecl := List (ident × ident)
abbrev may_fail_env_decl := List (ident × ident × Bool)

inductive tdecl
  | TTypeDecl : ident → tdecl
  | TFunDecl : ident → List ident → ident → List options → tdecl
  | TEventDecl : ident → List ident → tdecl
  | TConstDecl : ident → ident → List options → tdecl
  | TReduc : List (envdecl × extended_equation) → List options → tdecl
  | TReducFail : ident → List ident → ident → List (may_fail_env_decl × extended_equation) → List options → tdecl
  | TEquation : List (envdecl × extended_equation) → List options → tdecl
  | TPredDecl : ident → List ident → List options → tdecl
  | TTableDecl : ident → List ident → tdecl
  | TSet : ident → PTree.pval → tdecl
  | TPDef : ident → may_fail_env_decl → tprocess_e → tdecl
  | TQuery : envdecl → List tquery_e → List options → tdecl
  | TNoninterf : envdecl → List (ident × Option (List term_e)) → tdecl
  | TWeaksecret : ident → tdecl
  | TNoUnif : envdecl → nounif_t → Types.nounif_value → List options → tdecl
  | TNot : envdecl → gterm_e → tdecl
  | TElimtrue : may_fail_env_decl → term_e → tdecl
  | TFree : ident → ident → List options → tdecl
  | TClauses : List (may_fail_env_decl × tclause) → tdecl
  | TDefine : ident → List ident → List tdecl → tdecl
  | TExpand : ident → List ident → tdecl
  | TLetFun : ident → may_fail_env_decl → pterm_e → tdecl
  | TLemma : lemma_kind → envdecl → List tlemma → List options → tdecl

end LeanVerif.Proverif.PITPTree
