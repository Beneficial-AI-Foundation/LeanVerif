import Init.System.IO
import LeanVerif.Proverif.StringPlus

open System (FilePath)

namespace LeanVerif.Proverif.Addexpectedtags

def split_lines (s : String) : List String :=
  if s.isEmpty then
    []
  else
    let lines := s.splitOn "\n"
    if s.endsWith "\n" then
      lines.dropLast
    else
      lines

def add_tags_lines (filename : String) (lines : List String) : List String :=
  let rec go (tag : Nat) (acc : List String) : List String → List String
    | [] => acc.reverse
    | line :: rest =>
        let needsTag := StringPlus.contains line "EXPECTED" || StringPlus.contains line "EXPECTPV"
        let line' := if needsTag then
          s!"{line} FILENAME: {filename} TAG: {tag}"
        else
          line
        let tag' := if needsTag then tag + 1 else tag
        go tag' (line' :: acc) rest
  go 1 [] lines

def add_tags_file (filename : FilePath) : IO Unit := do
  IO.println s!"Adding tags to {filename}"
  let contents ← IO.FS.readFile filename
  let lines := split_lines contents
  let tagged := add_tags_lines filename.toString lines
  let output := if tagged.isEmpty then "" else String.intercalate "\n" tagged ++ "\n"
  let tmp := FilePath.mk (filename.toString ++ ".tmp")
  IO.FS.writeFile tmp output
  IO.FS.rename tmp filename

def add_tags_dir (dirname : FilePath) : IO Unit := do
  let entries := Array.qsort (← dirname.walkDir) (fun a b => a.toString < b.toString)
  for full in entries.toList do
    if !(← full.isDir) then
      let name := full.fileName.getD ""
      if StringPlus.case_insensitive_contains name ".m4." then
        let exts := [".cv", ".ocv", ".pcv", ".pv", ".pi", ".horntype", ".horn"]
        if exts.any (fun ext => StringPlus.case_insensitive_ends_with name ext) then
          add_tags_file full

def usage : IO α := do
  IO.eprintln "Incorrect arguments\nUsage:\naddexpectedtags <directories>"
  IO.Process.exit 0

def main (args : List String) : IO UInt32 := do
  if args.isEmpty then
    usage
  for arg in args do
    let path := FilePath.mk arg
    if ← path.isDir then
      add_tags_dir path
  return 0

end LeanVerif.Proverif.Addexpectedtags
