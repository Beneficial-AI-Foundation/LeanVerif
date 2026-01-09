import Init.System.IO

namespace LeanVerif.Proverif.ParsingHelper

structure Position where
  pos_fname : String := ""
  pos_lnum : Int := 0
  pos_bol : Int := 0
  pos_cnum : Int := -1
deriving Repr, BEq, Inhabited

structure Extent where
  source : Option String := none
  startPos : Nat := 0
  endPos : Nat := 0
  start : Position := {}
  endPosn : Position := {}
deriving Repr, BEq, Inhabited

abbrev extent := Extent

def dummy_pos : Position := { pos_cnum := -1 }

def dummy_ext : extent := { start := dummy_pos, endPosn := dummy_pos }

def merge_ext (a b : extent) : extent :=
  { source := if a.source = b.source then a.source else none
    startPos := Nat.min a.startPos b.startPos
    endPos := Nat.max a.endPos b.endPos
    start := a.start
    endPosn := b.endPosn }

structure Lexbuf where
  lexeme_start_p : Position := dummy_pos
  lexeme_end_p : Position := dummy_pos
  lex_abs_pos : Int := 0
  lex_curr_p : Position := dummy_pos
deriving Inhabited

def extent_of_lexbuf (lexbuf : Lexbuf) : extent :=
  let startPos := if lexbuf.lexeme_start_p.pos_cnum < 0 then 0 else Int.toNat lexbuf.lexeme_start_p.pos_cnum
  let endPos := if lexbuf.lexeme_end_p.pos_cnum < 0 then 0 else Int.toNat lexbuf.lexeme_end_p.pos_cnum
  { startPos := startPos
    endPos := endPos
    start := lexbuf.lexeme_start_p
    endPosn := lexbuf.lexeme_end_p }

def set_start (lexbuf : Lexbuf) (loc : extent) : Lexbuf :=
  if loc.start.pos_cnum != -1 then
    { lexbuf with
      lex_abs_pos := loc.start.pos_cnum
      lexeme_start_p := loc.start
      lex_curr_p := loc.start }
  else
    lexbuf

def parse_extent : extent :=
  dummy_ext

structure InputError where
  message : String
  ext : extent
deriving Repr

def internal_error (mess : String) : IO α := do
  IO.eprintln s!"Internal error: {mess}\nPlease report bug to proverif-dev@inria.fr, including input file and output"
  IO.Process.exit 3

def add_point_if_necessary (mess : String) : String :=
  if mess.length > 0 then
    match String.Pos.Raw.get? mess ⟨mess.length - 1⟩ with
    | some c =>
        if c != '.' && c != '?' && c != '!' then "." else ""
    | none => ""
  else
    ""

def get_extent (verbose : Bool) (ext : extent) : Option String :=
  let loc_start := ext.start
  let loc_end := ext.endPosn
  if loc_start.pos_cnum = -1 then
    none
  else
    let ch_start := loc_start.pos_cnum - loc_start.pos_bol + 1
    let ch_end := loc_end.pos_cnum - loc_end.pos_bol
    if !verbose then
      if ch_start >= ch_end then
        some s!"Character {ch_start}"
      else
        some s!"Characters {ch_start}-{ch_end}"
    else
      let line_start := loc_start.pos_lnum
      let line_end := loc_end.pos_lnum
      if line_start = line_end then
        if ch_start >= ch_end then
          some s!"File \"{loc_start.pos_fname}\", line {line_start}, character {ch_start}"
        else
          some s!"File \"{loc_start.pos_fname}\", line {line_start}, characters {ch_start}-{ch_end}"
      else
        some s!"File \"{loc_start.pos_fname}\", line {line_start}, character {ch_start} - line {line_end}, character {ch_end}"

def get_extent_string (verbose : Bool) (ext : extent) : String :=
  match get_extent verbose ext with
  | none => ""
  | some s => s ++ ":\n"

def get_mess_from (verbose : Bool) (pref : String) (mess : String) (ext : extent) : String :=
  (get_extent_string verbose ext) ++ pref ++ mess ++ add_point_if_necessary mess

def input_error (mess : String) (ext : extent) : IO α := do
  throw <| IO.userError (get_mess_from true "Error: " mess ext)

def display_input_error (mess : String) (ext : extent) : IO α := do
  IO.println (get_mess_from true "Error: " mess ext)
  IO.Process.exit 2

initialize interactive_mode : IO.Ref Bool ← IO.mkRef false
initialize warning_list : IO.Ref (List (String × extent)) ← IO.mkRef []

def get_warning_list : IO (List (String × extent)) := do
  let result ← warning_list.get
  warning_list.set []
  pure result

def input_warning (mess : String) (ext : extent) : IO Unit := do
  let interactive ← interactive_mode.get
  if interactive then
    warning_list.modify (fun l => (mess, ext) :: l)
  else
    IO.println (get_mess_from true "Warning: " mess ext)

def user_error (mess : String) : IO α :=
  input_error mess dummy_ext

initialize buf : IO.Ref String ← IO.mkRef ""
initialize start_pos : IO.Ref Position ← IO.mkRef dummy_pos
initialize end_pos : IO.Ref Position ← IO.mkRef dummy_pos

def set_start_pos (lexbuf : Lexbuf) : IO Unit := do
  start_pos.set lexbuf.lexeme_end_p

def set_end_pos (lexbuf : Lexbuf) : IO Unit := do
  end_pos.set lexbuf.lexeme_start_p

def clear_buffer : IO Unit := do
  buf.set ""

def get_string : IO (String × extent) := do
  let s ← buf.get
  clear_buffer
  let start ← start_pos.get
  let stop ← end_pos.get
  let startNat := if start.pos_cnum < 0 then 0 else Int.toNat start.pos_cnum
  let endNat := if stop.pos_cnum < 0 then 0 else Int.toNat stop.pos_cnum
  pure (s, { startPos := startNat, endPos := endNat, start := start, endPosn := stop })

def add_char (c : Char) : IO Unit := do
  buf.modify (fun s => s.push c)

def char_backslash : Char → Char
  | 'n' => '\n'
  | 't' => '\t'
  | 'b' => Char.ofNat 8
  | 'r' => '\r'
  | c => c

end LeanVerif.Proverif.ParsingHelper
