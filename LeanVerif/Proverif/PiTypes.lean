import LeanVerif.Proverif.Types
import LeanVerif.Proverif.ParsingHelper
import LeanVerif.Proverif.Stringmap
import LeanVerif.Proverif.Funsymbhash
import LeanVerif.Proverif.PITPTree

namespace LeanVerif.Proverif.PiTypes

open LeanVerif.Proverif.Types

abbrev StringMap := LeanVerif.Proverif.Stringmap.StringMap
abbrev HashtblSymbol := LeanVerif.Proverif.Funsymbhash.HashtblSymbol
abbrev Mut := LeanVerif.Proverif.Types.Mut
abbrev Hashtbl (k v : Type) := List (k × v)

/-- Placeholder for OCaml function-valued fields that capture {lit}`t_pi_state`. -/
abbrev pi_state_fun (_ : Type) := Nat

/-- Term with source extent. -/
abbrev term_e := term × ParsingHelper.extent

inductive event
  | QSEvent : Option Int → ordering_function → Option term → Option term_e → term → event
  | QSEvent2 : term → term → event
  | QFact : predicate → ordering_function → List term → Option term_e → event
  | QNeq : term_e → term_e → event
  | QEq : term_e → term_e → event
  | QGeq : term_e → term_e → event
  | QGr : term_e → term_e → event
  | QIsNat : term → event

mutual
  inductive conclusion_query
    | QTrue
    | QFalse
    | QEvent : event → conclusion_query
    | NestedQuery : realquery → conclusion_query
    | QAnd : conclusion_query → conclusion_query → conclusion_query
    | QOr : conclusion_query → conclusion_query → conclusion_query

  inductive realquery
    | Before : List event → conclusion_query → realquery
end

abbrev realquery_e := realquery × ParsingHelper.extent

inductive q_secret_opt
  | Reachability
  | RealOrRandom

inductive query
  | PutBegin : Bool → List funsymb → query
  | RealQuery : realquery → List term → query
  | QSecret : List term → List term → q_secret_opt → query

abbrev query_e := query × ParsingHelper.extent

structure eventstatus where
  end_status : Mut onestatus
  begin_status : Mut onestatus

inductive term_occ
  | OTest : occurrence → term_occ
  | OLet : occurrence → term_occ
  | OInChannel : occurrence → term_occ
  | OEvent : occurrence → term_occ
  | OLetFilter : occurrence → term_occ

inductive opt_set (α : Type)
  | Unset : opt_set α
  | Set : α → opt_set α

inductive encode_step
  | Public_vars : List (term × funsymb) → encode_step
  | Secret_reach : List term → funsymb → encode_step
  | Secret_ror : List term → funsymb → encode_step

inductive moved_ins
  | MLet
  | MNew
  | MBoth

inductive computable (α : Type)
  | Function : pi_state_fun α → computable α
  | Computed : α → computable α

inductive t_lemmas_application_side
  | AllSide
  | LeftSide
  | RightSide

structure t_one_lemma where
  ql_query : query_e
  ql_real_or_random : Option (List term)
  ql_application_side : t_lemmas_application_side
  ql_original_query : Option query_e
  ql_index_query_for_induction : Option Int

structure t_lemmas_state where
  lemmas : List t_one_lemma
  is_axiom : PITPTree.lemma_kind
  max_subset : Bool
  verif_app : t_lemma_application
  sat_app : t_lemma_application
  induction : Bool
  keep_axiom : Bool
  remove_events : t_remove_events_for_lemma

inductive t_lemmas
  | LemmaToTranslate : pi_state_fun t_lemmas → t_lemmas
  | Lemma : t_lemmas_state → t_lemmas

structure t_solve_corresp_status where
  s_max_subset : Bool
  s_ind_sat_app : t_lemma_application
  s_ind_verif_app : t_lemma_application
  s_induction : Bool

inductive t_query
  | QueryToTranslate : pi_state_fun t_query → t_query
  | CorrespQuery : List query_e → t_solve_corresp_status → t_query
  | CorrespQEnc : List (query_e × query_e) → t_solve_corresp_status → t_query
  | ChoiceQEnc : query_e → t_query
  | ChoiceQuery
  | NonInterfQuery : List (funsymb × Option (List term)) → t_query
  | WeakSecret : funsymb → t_query

mutual
  inductive t_construction
    | Initial_process
    | Initial_process1
    | Initial_process2
    | Merge : t_process_desc → t_process_desc → t_construction
    | Simplify : t_process_desc → t_construction
    | Encode : t_process_desc → List encode_step → t_construction
    | BarrierNoSwap : t_process_desc → t_construction
    | BarrierSwap : t_process_desc → t_construction
    | Move : moved_ins → t_process_desc → t_construction

  structure t_process_desc where
    proc : process
    bi_pro : Bool
    display_num : Mut (Option Int)
    construction : t_construction
end

inductive t_process_query
  | Equivalence : t_process_desc → t_process_desc → t_process_query
  | SingleProcess : t_process_desc → List t_query → t_process_query
  | SingleProcessSingleQuery : t_process_desc → t_query → t_process_query

structure t_pi_state where
  pi_process_query : t_process_query
  pi_global_env : opt_set (StringMap envElement)
  pi_glob_table : opt_set (Hashtbl String funsymb)
  pi_glob_table_var_name : opt_set (Hashtbl String term)
  pi_types : List typet
  pi_funs : List funsymb
  pi_freenames : List funsymb
  pi_events : List funsymb
  pi_equations : List (List (term × term) × eq_info)
  pi_max_used_phase : Int
  pi_all_barrier_tags : List (Int × (String × ParsingHelper.extent))
  pi_input_clauses : List (List fact × fact × constraints × label)
  pi_elimtrue : List fact
  pi_equivalence_clauses : List (List fact × fact × Int)
  pi_destructors_check_deterministic : List funsymb
  pi_disequations : List (term × term)
  pi_event_status_table : opt_set (HashtblSymbol eventstatus)
  -- Function-valued fields encoded as placeholders
  pi_get_not : pi_state_fun (List event)
  pi_get_nounif : pi_state_fun (List (fact_format × nounif_value × nounif_op))
  pi_terms_to_add_in_name_params : opt_set (List term_occ)
  pi_min_choice_phase : opt_set Int
  pi_need_vars_in_names : computable (List (funsymb × String × ParsingHelper.extent))
  pi_name_args_exact : Bool
  pi_lemma : List t_lemmas
  pi_original_axioms : List (realquery_e × Bool)
  pi_precise_actions : List funsymb
  pi_originally_lemma : Bool

inductive div_type
  | DEval : String → Int → term → Int → process → div_type
  | DEvalOut : Int → term → term → Int → process → div_type
  | DEvalLet : Int → term → pattern → Int → process → div_type
  | DEvalFact : Int → fact → Int → process → div_type
  | DTest : Int → Bool → term → term → term → Int → process → div_type
  | DLetFilter : Int → Bool → fact → Int → process → div_type
  | DGet : Int → term → pattern → term → Int → process → div_type
  | DInputPat : Int → term → term → pattern → Int → process → div_type
  | DIOPat : Int → term → term → pattern → Int → process → Int → process → div_type
  | DIO : Int → term → term → Int → process → Int → process → div_type
  | DChannel : String → Int → term → term → term → term → Int → process → div_type
  | DFail : Int → term → term → div_type
  | DEquality : Int → term → term → term → term → div_type
  | DOutputMess : term → term → term → Int → process → div_type → div_type

inductive remove_cause
  | DestrFails
  | TestFails
  | Blocks

inductive reduc_type
  | RNil : Int → reduc_type
  | RPar : Int → reduc_type
  | RRepl : Int → Int → reduc_type
  | RRestrAtt : term → reduc_type
  | RAddpub : List (term × term × term) → reduc_type
  | RRestr : Int → funsymb → term → reduc_type
  | RLet_In : Int → pattern → term → reduc_type
  | RLet_Else : Int → pattern → term → reduc_type
  | RLet_Remove : Int → pattern → term → reduc_type
  | RInput_Success : Int → term → pattern → term → term → reduc_type
  | RInput_PatFails : Int → term → pattern → term → term → reduc_type
  | RInput_Remove : Int → term → pattern → remove_cause → reduc_type
  | ROutput_Success : Int → term → term → term → reduc_type
  | ROutput_Remove : Int → term → term → remove_cause → reduc_type
  | RTest_Then : Int → term → reduc_type
  | RTest_Else : Int → term → reduc_type
  | RTest_Remove : Int → term → remove_cause → reduc_type
  | REvent_Success : Int → term → Bool → reduc_type
  | REvent_Remove : Int → term → remove_cause → reduc_type
  | RPhase : Int → reduc_type
  | RLetFilter_In : Int → List binder → List term → fact → reduc_type
  | RLetFilter_Else : Int → List binder → fact → reduc_type
  | RLetFilter_Remove : Int → List binder → fact → remove_cause → reduc_type
  | RIO : Int → term → pattern → Int → term → Option term → term → Bool → reduc_type
  | RIO_PatRemove : Int → term → pattern → Int → term → Option term → term → Bool → Bool → reduc_type
  | RInsert_Success : Int → term → Bool → reduc_type
  | RInsert_Remove : Int → term → remove_cause → reduc_type
  | RGet_In : Int → pattern → term → term → reduc_type
  | RGet_Else : Int → pattern → term → reduc_type
  | RGet_Remove : Int → pattern → term → reduc_type
  | RNamedProcess : Int → String → List term → reduc_type
  | RInit
  | RDiverges : div_type → reduc_type

abbrev name_param_info := arg_meaning × term × when_include

inductive info (α : Type)
  | InputInfo : List (binder × α) → List (term × α) → α → List name_param_info →
      List hypspec → List (α × Option (term × (List (binder × α)) × List (term × α))) → info α
  | OutputInfo : List (binder × α) → List (term × α) → info α
  | GetInfo : List α → List α → info α
  | Nothing : info α

abbrev sub_proc (α : Type) :=
  process × List name_param_info × List hypspec × List fact × info α

inductive loc_info (α : Type)
  | LocAttacker : term → loc_info α
  | LocProcess : Int → sub_proc α → loc_info α

inductive weak_test
  | FailTest : term → weak_test
  | EqTest : term → term → weak_test

inductive noninterf_test (α : Type)
  | ProcessTest : List hypspec → List term → Option (Int × sub_proc α) → noninterf_test α
  | InputProcessTest : List hypspec → List term → term →
      Option (Int × sub_proc α × loc_info α) → noninterf_test α
  | ApplyTest : funsymb → List (α × Option term) → noninterf_test α
  | NIFailTest : α → Option term → noninterf_test α
  | CommTest : α → α → Option (loc_info α × loc_info α) → noninterf_test α
  | NIEqTest : (α × Option term) → (α × Option term) → noninterf_test α

inductive corresp_goal_t
  | Fact : fact → Option (List term) → Bool → corresp_goal_t
  | EventGoal : fact → Option (occurrence × List term) → corresp_goal_t

inductive goal_t (α : Type)
  | CorrespGoal : List corresp_goal_t → List Int → goal_t α
  | WeakSecrGoal : List (term × binder × Option term) → weak_test → term → term → goal_t α
  | NonInterfGoal : noninterf_test α → goal_t α
  | NoGoal : goal_t α

structure reduc_state (α : Type) where
  goal : goal_t α
  subprocess : List (sub_proc α)
  «public» : List (term × α)
  pub_vars : List term
  tables : List α
  prepared_attacker_rule : List (predicate × List (binder × α) × (term × α))
  io_rule : List (Int × List fact × List hypspec × List term × fact)
  previous_state : Option (reduc_state α)
  hyp_not_matched : List (Option term × fact)
  assumed_false : List (List fact)
  current_phase : Int
  comment : reduc_type
  events : List term
  barriers : List (Int × Int)

inductive io_r_t
  | Other
  | I_O : term → Int → pattern → process → process → io_r_t
  | O_I : term → Int → term → process → process → io_r_t

structure data_model where
  tables_lst : List String
  public_lst : List String
  titles : List (String × io_r_t)
  proc_llst : List (List String)
  no_auto : Bool
  events_lst : List String
  barriers_lst : List String

inductive query_res
  | True
  | False
  | DontKnow

inductive is_error
  | Ok
  | Error

structure group_summary_t where
  sum_queries : List (t_process_query × query_res)
  sum_lemmas : List (t_process_query × is_error × query_res)
  sum_axioms : List (t_process_query × Bool)

structure grouped_axioms_t where
  «axiom» : t_query
  axiom_string : String
  axiom_proc : Mut (List t_process_desc)

structure grouped_lemmas_t where
  «lemma» : t_query
  lemma_string : String
  true_res : Mut (List t_process_desc)
  false_res : Mut (List t_process_desc)
  dont_know_res : Mut (List t_process_desc)
  error : Mut (List t_process_desc)

end LeanVerif.Proverif.PiTypes
