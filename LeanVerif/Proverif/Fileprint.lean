import Lean

namespace LeanVerif.Proverif.Fileprint

/-- True when emitting "nice" TeX. -/
initialize nice_tex : IO.Ref Bool ← IO.mkRef true

/-- LaTeX preamble emitted once. -/
def preamble : String := "\n\\documentclass{article}\n\\usepackage[utf8]{inputenc}\n\\newcommand{\\kwl}[1]{\\mathbf{#1}}\n\\newcommand{\\kwf}[1]{\\mathsf{#1}}\n\\newcommand{\\kwc}[1]{\\mathsf{#1}}\n\\newcommand{\\kwp}[1]{\\mathsf{#1}}\n\\newcommand{\\kwt}[1]{\\mathsf{#1}}\n\\newcommand{\\kwe}[1]{\\mathsf{#1}}\n\\newcommand{\\kwtable}[1]{\\mathsf{#1}}\n\\newcommand{\\var}[1]{\\mathit{#1}}\n\\newcommand{\\getR}{\\stackrel{R}{\\leftarrow}}\n\\begin{document}\n"

/-- LaTeX postamble emitted at close. -/
def postamble : String := "\n\\end{document}\n"

/-- Whether the preamble has been printed. -/
initialize printed_preamble : IO.Ref Bool ← IO.mkRef false

/-- Output file name; empty means stdout. -/
initialize outfile : IO.Ref String ← IO.mkRef ""

/-- Output channel handle. -/
initialize outchannel : IO.Ref IO.FS.Stream ← do
  let h ← IO.getStdout
  IO.mkRef h

/-- Print raw string to the current output handle. -/
def print_string (s : String) : IO Unit := do
  let h ← outchannel.get
  h.putStr s

/-- Print a string, escaping TeX-special characters. -/
def print_sanitize (s : String) : IO Unit := do
  let h ← outchannel.get
  for c in s.toList do
    match c with
    | '\\' => h.putStr "{\\textbackslash}"
    | '&' => h.putStr "\\ensuremath{\\&}"
    | '{' => h.putStr "\\ensuremath{\\{}"
    | '}' => h.putStr "\\ensuremath{\\}}"
    | '_' => h.putStr "{\\_}"
    | '^' => h.putStr "{\\string^}"
    | '#' => h.putStr "\\#"
    | '$' => h.putStr "\\$"
    | '%' => h.putStr "\\%"
    | '@' => h.putStr "{\\string@}"
    | '~' => h.putStr "{\\string~}"
    | '>' => h.putStr "\\ensuremath{>}"
    | '<' => h.putStr "\\ensuremath{<}"
    | ' ' => h.putStr "\\ "
    | '\t' => h.putStr "\\qquad\\qquad "
    | c =>
        if c.toNat < 32 then
          pure ()
        else
          h.putStr (String.singleton c)

/-- Print the preamble once. -/
def print_preamble : IO Unit := do
  let printed ← printed_preamble.get
  if !printed then
    printed_preamble.set true
    print_string preamble

/-- Emit the postamble and close file handles when needed. -/
def close : IO Unit := do
  let printed ← printed_preamble.get
  if printed then
    print_string postamble
  let out ← outfile.get
  if out != "" then
    let h ← outchannel.get
    h.flush

end LeanVerif.Proverif.Fileprint
