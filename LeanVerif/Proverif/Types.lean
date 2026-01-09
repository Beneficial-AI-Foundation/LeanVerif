import Lean.LibrarySuggestions.Basic
import LeanVerif.Proverif.ParsingHelper
import LeanVerif.Proverif.Stringmap

set_option maxHeartbeats 0
set_library_suggestions Lean.LibrarySuggestions.empty

namespace LeanVerif.Proverif.Types

abbrev StringMap := LeanVerif.Proverif.Stringmap.StringMap
-- TODO: Replace with `IO.Ref` or an explicit state threading once we model mutation.
abbrev Mut (α : Type) := α

structure occurrence where
  occ : Int
  precise : Bool

structure typet where
  tname : String

inductive user_pred_spec
  | Polym : String → Int → List typet → user_pred_spec
  | Monom : user_pred_spec

mutual
  inductive info
    | Attacker : Int → typet → info
    | Mess : Int → typet → info
    | InputP : Int → info
    | OutputP : Int → info
    | AttackerBin : Int → typet → info
    | MessBin : Int → typet → info
    | InputPBin : Int → info
    | OutputPBin : Int → info
    | AttackerGuess : typet → info
    | Compromise : typet → info
    | Equal : typet → info
    | Table : Int → info
    | TableBin : Int → info
    | TestUnifP : typet → info
    | UserPred : user_pred_spec → info
    | Combined : List predicate → info
    | Subterm : typet → typet → info
    | OtherInternalPred : info
    | Block : predicate → info

  structure predicate where
    p_name : String
    p_type : List typet
    p_prop : Int
    p_info : info
    -- mutable in OCaml
    p_record : Mut Int
end

inductive when_include
  | Always
  | IfQueryNeedsIt

inductive eq_info
  | EqNoInfo
  | EqConvergent
  | EqLinear

structure renamable_id where
  orig_name : String
  name : String
  idx : Int
  -- mutable in OCaml
  display : Mut (Option String)

inductive fixed_or_renamable
  | Fixed : String → fixed_or_renamable
  | Renamable : renamable_id → fixed_or_renamable

-- Placeholders for OCaml function-valued fields (to avoid negative occurrences)
abbrev pg_link_ref := Nat
abbrev letfun_ref := Nat

mutual
  structure binder where
    vname : renamable_id
    unfailing : Bool
    btype : typet
    -- mutable in OCaml
    link : Mut linktype

  inductive pattern
    | PatVar : binder → pattern
    | PatTuple : funsymb → List pattern → pattern
    | PatEqual : term → pattern

  inductive linktype
    | NoLink
    | VLink : binder → linktype
    | TLink : term → linktype
    | TLink2 : term → linktype
    | FLink : format → linktype
    | PGLink : pg_link_ref → linktype
    | SLink : Int → linktype
    | ETLink : enriched_term → linktype

  inductive arg_meaning
    | MUnknown
    | MSid : Int → arg_meaning
    | MCompSid
    | MAttSid
    | MVar : binder → Option String → arg_meaning

  structure name_info where
    -- mutable in OCaml
    prev_inputs : Mut (Option term)
    -- mutable in OCaml
    prev_inputs_meaning : Mut (List arg_meaning)

  inductive funcats
    | Red : List (List term × term × constraints) → funcats
    | Eq : List (List term × term) → funcats
    | Tuple
    | Name : name_info → funcats
    | SpecVar : binder → funcats
    | Syntactic : funsymb → funcats
    | General_var
    | General_mayfail_var
    | Choice
    | ChoiceFst
    | ChoiceSnd
    | Failure

  structure constraints where
    neq : List (List (term × term))
    is_nat : List term
    is_not_nat : List term
    geq : List (term × Int × term)

  structure funsymb where
    f_name : fixed_or_renamable
    -- mutable in OCaml
    f_type : Mut (List typet × typet)
    -- mutable in OCaml
    f_cat : Mut funcats
    f_initial_cat : funcats
    f_private : Bool
    f_options : Int
    -- mutable in OCaml
    f_record : Mut Int

  inductive term
    | Var : binder → term
    | FunApp : funsymb → List term → term

  inductive format
    | FVar : binder → format
    | FFunApp : funsymb → List format → format
    | FAny : binder → format

  inductive fact
    | Pred : predicate → List term → fact

  structure enriched_term where
    et_desc : enriched_desc
    et_simple : Bool
    et_simple_no_let : Bool
    et_type : typet
    et_ext : ParsingHelper.extent

  inductive enriched_desc
    | ET_Var : binder → enriched_desc
    | ET_FunApp : funsymb → List enriched_term → enriched_desc
    | ET_Restr : funsymb → new_args → enriched_term → enriched_desc
    | ET_Test : enriched_term → enriched_term → enriched_term → enriched_desc
    | ET_Let : enriched_pattern → enriched_term → enriched_term → enriched_term → enriched_desc
    | ET_Event : enriched_term → new_args → enriched_term → enriched_desc
    | ET_LetFilter : List binder → predicate → List enriched_term → enriched_term → enriched_term → enriched_desc
    | ET_Insert : enriched_term → enriched_term → enriched_desc
    | ET_Get : enriched_pattern → enriched_term → enriched_term → enriched_term → Bool → enriched_desc

  structure enriched_pattern where
    ep_desc : enriched_pat_desc
    ep_simple : Bool
    ep_type : typet

  inductive enriched_pat_desc
    | EP_PatVar : binder → enriched_pat_desc
    | EP_PatTuple : funsymb → List enriched_pattern → enriched_pat_desc
    | EP_PatEqual : enriched_term → enriched_pat_desc

  inductive envElement
    | EFun : funsymb → envElement
    | EVar : binder → envElement
    | EName : funsymb → envElement
    | EPred : predicate → envElement
    | EEvent : funsymb → envElement
    | EType : typet → envElement
    | ETable : funsymb → envElement
    | ELetEq : term → typet → envElement
    | ELetFun : letfun_ref → List typet → typet → envElement
    | EProcess : List binder → process → List (Int × (String × ParsingHelper.extent)) → envElement
    | EReserved : envElement

  structure new_args where
    args : Option (List binder)
    env : List (String × envElement)

  inductive process
    | Nil : process
    | Par : process → process → process
    | Repl : process → occurrence → process
    | Restr : funsymb → new_args → process → occurrence → process
    | Test : term → process → process → occurrence → process
    | Input : term → pattern → process → occurrence → process
    | Output : term → term → process → occurrence → process
    | Let : pattern → term → process → process → occurrence → process
    | LetFilter : List binder → fact → process → process → occurrence → process
    | Event : term → new_args → process → occurrence → process
    | Insert : term → process → occurrence → process
    | Get : pattern → term → process → process → occurrence → process
    | Phase : Int → process → occurrence → process
    | Barrier : Int → (String × ParsingHelper.extent) → process → occurrence → process
    | AnnBarrier : Int → String → funsymb → funsymb → List (binder × term) → process → occurrence → process
    | NamedProcess : String → List term → process → process
end

abbrev fact_format := predicate × List format

abbrev rewrite_rule := List term × term × constraints
abbrev rewrite_rules := List rewrite_rule

inductive nounif_single_op
  | Hypothesis
  | Conclusion
  | InductionVerif
  | InductionSat : List binder → nounif_single_op

abbrev nounif_op := List nounif_single_op

inductive nounif_value
  | NoUnifNegDefault
  | NoUnifPosDefault
  | NoUnifValue : Int → nounif_value
  deriving Repr, BEq, Inhabited

inductive rulespec
  | RElem : List predicate → predicate → rulespec
  | RApplyFunc : funsymb → predicate → rulespec
  | RApplyProj : funsymb → Int → predicate → rulespec
  | RFail : Bool → predicate → rulespec

inductive onestatus
  | No
  | NoOcc
  | WithOcc

inductive hypspec
  | ReplTag : occurrence → Int → hypspec
  | InputTag : occurrence → hypspec
  | PreciseTag : occurrence → hypspec
  | EventTag : occurrence → hypspec
  | BeginFact
  | LetFilterTag : occurrence → hypspec
  | LetFilterTagElse : occurrence → hypspec
  | OutputTag : occurrence → hypspec
  | TestTag : occurrence → hypspec
  | LetTag : occurrence → hypspec
  | TestUnifTag : occurrence → hypspec
  | TestUnifTag2 : occurrence → hypspec
  | InputPTag : occurrence → hypspec
  | OutputPTag : occurrence → hypspec
  | InsertTag : occurrence → hypspec
  | GetTag : occurrence → hypspec
  | GetTagElse : occurrence → hypspec

inductive label
  | ProcessRule : List hypspec → List term → label
  | Apply : funsymb → predicate → label
  | TestApply : funsymb → predicate → label
  | TestEq : predicate → label
  | LblEquiv
  | LblClause
  | LblEq
  | Rl : predicate → predicate → label
  | Rs : predicate → predicate → label
  | Ri : predicate → predicate → label
  | Ro : predicate → predicate → label
  | Rfail : predicate → label
  | TestComm : predicate → predicate → label
  | WeakSecr
  | Rn : predicate → label
  | Init
  | PhaseChange
  | TblPhaseChange
  | LblComp
  | LblNone
  | Elem : List predicate → predicate → label
  | TestUnif
  | TestDeterministic : funsymb → label
  | TestTotal : funsymb → label
  | Goal
  | GoalCombined
  | GoalInjective
  | GenerationNested

inductive injectivity
  | DoubleIndex : Int → Int → injectivity
  | SingleIndex : fact → fact → Int → injectivity
  | NoIndex : fact → injectivity

inductive history
  | Rule : Int → label → List fact → fact → constraints → history
  | Removed : Int → Int → history → history
  | Any : Int → history → history
  | Empty : fact → history
  | HMaxHyp : Int → history → history
  | HEquation : Int → fact → fact → history
  | Resolution : history → Int → history → history
  | TestUnifTrue : Int → history → history
  | HLemma : Int → List (Int × fact) → List (term × term × List Int) → (List fact × constraints × List (term × term)) → history → history
  | HCaseDistinction : fact → List fact → List (term × term) → constraints → history → history
  | HInjectivity : injectivity → history → history
  | HNested : List Int → Int → history → history

abbrev reduction := List fact × fact × history × constraints

inductive order
  | Less
  | Leq

abbrev ordering_function := List (Int × order)

structure ordered_reduction where
  rule : reduction
  order_data : Option (List (ordering_function × Int))

structure saturated_reduction where
  sat_rule : reduction
  sat_generate_ordering_data : (ordering_function × Int) → List (ordering_function × Int)

abbrev equation := term × term

mutual
  inductive fact_tree_desc
    | FHAny
    | FEmpty
    | FRemovedWithMaxHyp
    | FRemovedWithProof : fact_tree → fact_tree_desc
    | FRule : Int → label → constraints → List fact_tree → constraints → List fact_tree → fact_tree_desc
    | FEquation : fact_tree → fact_tree_desc

  structure fact_tree where
    -- mutable in OCaml
    desc : Mut fact_tree_desc
    -- mutable in OCaml
    thefact : Mut fact
end

inductive t_solver_kind
  | Solve_Standard
  | Solve_Equivalence
  | Solve_WeakSecret : List (typet × history) → Int → t_solver_kind
  | Solve_Noninterf : List (funsymb × Option (List term)) → t_solver_kind

inductive t_lemma_application
  | LANone
  | LAOnlyWhenRemove
  | LAOnlyWhenInstantiate
  | LAFull

inductive t_remove_events_for_lemma
  | RENone
  | REKeep
  | RERemove

structure lemma where
  l_index : Int
  l_premise : List fact
  l_subterms : List (term × term)
  l_constra : constraints
  l_conclusion : List (List fact × constraints × List (term × term))
  l_verif_app : t_lemma_application
  l_sat_app : t_lemma_application
  l_induction : Option Int
  l_remove_events : t_remove_events_for_lemma

inductive clauses
  | Given : List reduction → clauses
  | ToGenerate : List reduction → ((reduction → Unit) → Unit) → clauses

structure t_horn_state where
  h_clauses : clauses
  h_equations : List ((List (term × term) × eq_info))
  h_close_with_equations : Bool
  h_not : List fact
  h_elimtrue : List (Int × fact)
  h_equiv : List (List fact × fact × Int)
  h_nounif : List (fact_format × nounif_value × nounif_op)
  h_clauses_to_initialize_selfun : List reduction
  h_solver_kind : t_solver_kind
  h_lemmas : List lemma
  h_pred_prove : List predicate
  h_event_in_queries : List funsymb

inductive precise_info
  | Action : typet → precise_info

inductive rewrite_rules_status
  | ToCheck : Int → Int → rewrite_rules_status
  | ToExecute : Int → rewrite_rules_status

end LeanVerif.Proverif.Types
