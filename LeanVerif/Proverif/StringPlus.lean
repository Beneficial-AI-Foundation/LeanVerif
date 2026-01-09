namespace LeanVerif.Proverif.StringPlus

private def get_char? (s : String) (n : Nat) : Option Char :=
  String.Pos.Raw.get? s ⟨n⟩

/-- Compare two substrings for equality. -/
def equal_substring (s1 : String) (n1 : Nat) (s2 : String) (n2 : Nat) (len : Nat) : Bool :=
  match len with
  | 0 => true
  | len + 1 =>
      match get_char? s1 n1, get_char? s2 n2 with
      | some c1, some c2 =>
          (c1 == c2) && equal_substring s1 (n1 + 1) s2 (n2 + 1) len
      | _, _ => false

/-- True when {lit}`s` starts with {lit}`sub`. -/
def starts_with (s sub : String) : Bool :=
  let len := sub.length
  (s.length >= len) && equal_substring s 0 sub 0 len

/-- True when {lit}`s` ends with {lit}`sub`. -/
def ends_with (s sub : String) : Bool :=
  let l_s := s.length
  let l_sub := sub.length
  (l_s >= l_sub) && equal_substring s (l_s - l_sub) sub 0 l_sub

/-- True when the substring of {lit}`s` starting at {lit}`n` contains {lit}`sub`. -/
def contains_from (s sub : String) (n : Nat) : Bool :=
  let l_sub := sub.length
  let l_s := s.length
  let rec go : Nat → Nat → Bool
    | 0, _ => false
    | fuel + 1, i =>
        if l_sub + i > l_s then
          false
        else if equal_substring s i sub 0 l_sub then
          true
        else
          go fuel (i + 1)
  go (l_s - n + 1) n

/-- True when {lit}`s` contains {lit}`sub`. -/
def contains (s sub : String) : Bool :=
  contains_from s sub 0

/-- Return the first position of {lit}`sub` in {lit}`s` starting at {lit}`n`, if any. -/
def pos_from (s sub : String) (n : Nat) : Option Nat :=
  let l_sub := sub.length
  let l_s := s.length
  let rec go : Nat → Nat → Option Nat
    | 0, _ => none
    | fuel + 1, i =>
        if l_sub + i > l_s then
          none
        else if equal_substring s i sub 0 l_sub then
          some i
        else
          go fuel (i + 1)
  go (l_s - n + 1) n

/-- Return the first position of {lit}`sub` in {lit}`s`, if any. -/
def pos (s sub : String) : Option Nat :=
  pos_from s sub 0

/-- Compare two substrings for equality, case-insensitively. -/
def case_insensitive_equal_substring (s1 : String) (n1 : Nat) (s2 : String) (n2 : Nat) (len : Nat) : Bool :=
  match len with
  | 0 => true
  | len + 1 =>
      match get_char? s1 n1, get_char? s2 n2 with
      | some c1, some c2 =>
          (Char.toLower c1 == Char.toLower c2) &&
            case_insensitive_equal_substring s1 (n1 + 1) s2 (n2 + 1) len
      | _, _ => false

/-- True when {lit}`s` ends with {lit}`sub`, case-insensitively. -/
def case_insensitive_ends_with (s sub : String) : Bool :=
  let l_s := s.length
  let l_sub := sub.length
  (l_s >= l_sub) && case_insensitive_equal_substring s (l_s - l_sub) sub 0 l_sub

/-- True when the substring of {lit}`s` starting at {lit}`n` contains {lit}`sub`, case-insensitively. -/
def case_insensitive_contains_from (s sub : String) (n : Nat) : Bool :=
  let l_sub := sub.length
  let l_s := s.length
  let rec go : Nat → Nat → Bool
    | 0, _ => false
    | fuel + 1, i =>
        if l_sub + i > l_s then
          false
        else if case_insensitive_equal_substring s i sub 0 l_sub then
          true
        else
          go fuel (i + 1)
  go (l_s - n + 1) n

/-- True when {lit}`s` contains {lit}`sub`, case-insensitively. -/
def case_insensitive_contains (s sub : String) : Bool :=
  case_insensitive_contains_from s sub 0

end LeanVerif.Proverif.StringPlus
