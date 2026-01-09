import LeanVerif.Proverif.ParsingHelper
import LeanVerif.Proverif.PTree
import LeanVerif.Proverif.Types

namespace LeanVerif.Proverif.PIPTree

abbrev ident := PTree.ident

inductive term
  | PIdent : ident → term
  | PFail : term
  | PFunApp : ident → List (term × ParsingHelper.extent) → term
  | PTuple : List (term × ParsingHelper.extent) → term

abbrev term_e := term × ParsingHelper.extent

abbrev equation := term_e × term_e
abbrev fundef := List (term_e × term_e)

inductive gformat
  | PFGIdent : ident → gformat
  | PFGFunApp : ident → List (gformat × ParsingHelper.extent) → gformat
  | PFGTuple : List (gformat × ParsingHelper.extent) → gformat
  | PFGName : ident → List (ident × (gformat × ParsingHelper.extent)) → gformat
  | PFGAny : ident → gformat

abbrev gformat_e := gformat × ParsingHelper.extent

abbrev gfact_format := ident × List gformat_e × Int

inductive gterm
  | PGIdent : ident → gterm
  | PGFunApp : ident → List (gterm × ParsingHelper.extent) → gterm
  | PGTuple : List (gterm × ParsingHelper.extent) → gterm
  | PGName : ident → List (ident × (gterm × ParsingHelper.extent)) → gterm

abbrev gterm_e := gterm × ParsingHelper.extent

inductive gfact
  | PGSimpleFact : ident → List gterm_e → gfact
  | PGNeq : gterm_e → gterm_e → gfact
  | PGEqual : gterm_e → gterm_e → gfact

abbrev gfact_e := gfact × ParsingHelper.extent

abbrev event := gfact_e × Int

inductive hypelem
  | PQEvent : event → hypelem
  | PNestedQuery : realquery → hypelem
  | POr : hypelem → hypelem → hypelem
  | PAnd : hypelem → hypelem → hypelem
  | PFalse : hypelem

inductive realquery
  | PBefore : event → hypelem → realquery

inductive query
  | PBinding : ident → gterm_e → query
  | PPutBegin : ident → List ident → query
  | PRealQuery : realquery → query

abbrev query_e := query × ParsingHelper.extent

inductive fact
  | PSimpleFact : ident → List term_e → fact
  | PSNeq : term_e → term_e → fact
  | PSEqual : term_e → term_e → fact

abbrev fact_e := fact × ParsingHelper.extent

inductive clause
  | PClause : List fact_e → fact_e → clause
  | PEquiv : List fact_e → fact_e → Bool → clause

inductive pattern
  | PPatVar : ident → pattern
  | PPatTuple : List pattern → pattern
  | PPatFunApp : ident → List pattern → pattern
  | PPatEqual : term_e → pattern

inductive process
  | PNil : process
  | PPar : process → process → process
  | PRepl : process → process
  | PRestr : ident → process → process
  | PLetDef : ident → process
  | PTest : fact_e → process → process → process
  | PInput : term_e → pattern → process → process
  | POutput : term_e → term_e → process → process
  | PLet : pattern → term_e → process → process → process
  | PLetFilter : List ident → fact_e → process → process → process
  | PEvent : ident → List term_e → process → process
  | PPhase : Int → process → process
  | PBarrier : Int → Option ident → process → process

inductive decl
  | FunDecl : ident → Nat → Bool → decl
  | DataFunDecl : ident → Nat → decl
  | Reduc : fundef → Bool → decl
  | ReducFail : List (term_e × term_e × List ident) → Bool → decl
  | Equation : List equation → decl
  | PredDecl : ident → Nat → List ident → decl
  | Param : ident → PTree.pval → decl
  | PDef : ident → process → decl
  | Query : List query_e → decl
  | Noninterf : List (ident × Option (List term_e)) → decl
  | Weaksecret : ident → decl
  | NoUnif : gfact_format → Types.nounif_value → List (ident × gformat_e) → decl
  | Not : (gfact_e × Int) → List (ident × gterm_e) → decl
  | Elimtrue : fact_e → List ident → decl
  | Free : List ident → Bool → decl
  | Clauses : List (clause × List ident) → decl

end LeanVerif.Proverif.PIPTree
