import Lake
open Lake DSL
open System (FilePath)

require verso from git "https://github.com/leanprover/verso" @ "main"

package LeanVerif where
  leanOptions := #[
    ⟨`doc.verso, true⟩
  ]

@[default_target]
lean_lib LeanVerif

@[default_target]
lean_exe leanverif where
  root := `Main

def vendorDir : FilePath := "vendor/proverif-2.05"
def testsDir : FilePath := "tests/proverif"
def casesFile : FilePath := testsDir / "cases.txt"
def baselineDir : FilePath := testsDir / "baseline"
def proverifExe : FilePath := vendorDir / "proverif"

def ensureDir (dir : FilePath) : IO Unit := do
  if !(← dir.pathExists) then
    IO.FS.createDirAll dir
  if !(← dir.isDir) then
    throw (↑ s!"Not a directory: {dir}")

def encodeCase (s : String) : String :=
  s.toList.foldl (fun acc c => if c = '/' then acc ++ "__" else acc.push c) ""

def readCases : IO (Array String) := do
  let contents ← IO.FS.readFile casesFile
  let lines := contents.splitOn "\n"
  let cases := lines.filterMap fun line =>
    let line := line.trim
    if line.isEmpty || line.startsWith "#" then
      none
    else
      some line
  pure cases.toArray

def buildProverif : IO Int := do
  if !(← vendorDir.pathExists) then
    IO.eprintln s!"Missing vendored ProVerif at {vendorDir}"
    return 1
  if ← proverifExe.pathExists then
    return 0
  IO.println "Building ProVerif (nointeract)..."
  let proc ← IO.Process.spawn {
    cmd := "sh"
    args := #["-lc", "./build -nointeract"]
    cwd := some vendorDir
  }
  let code ← proc.wait
  if code != 0 then
    IO.eprintln "ProVerif build failed."
  return code

def runProverif (caseFile : String) : IO (Int × String) := do
  let result ← IO.Process.output {
    cmd := proverifExe.toString
    args := #[caseFile]
    cwd := some vendorDir
    stdin := .null
  }
  let output := result.stdout ++ result.stderr
  return (result.exitCode, output)

script proverifBuild do
  return (← buildProverif)

script proverifTest (args) do
  let update := args.contains "--update"
  let code ← buildProverif
  if code != 0 then
    return code
  ensureDir baselineDir
  let cases ← readCases
  if cases.isEmpty then
    IO.eprintln s!"No test cases found in {casesFile}"
    return 1
  let mut failed := false
  for caseFile in cases do
    let (exitCode, output) ← runProverif caseFile
    if exitCode != 0 then
      IO.eprintln s!"ProVerif failed on {caseFile} (exit {exitCode})"
      failed := true
    let baselinePath := baselineDir / s!"{encodeCase caseFile}.out"
    if update then
      IO.FS.writeFile baselinePath output
      IO.println s!"Baseline updated: {baselinePath}"
    else
      if !(← baselinePath.pathExists) then
        IO.eprintln s!"Missing baseline: {baselinePath}"
        failed := true
      else
        let baseline ← IO.FS.readFile baselinePath
        if baseline != output then
          IO.eprintln s!"Mismatch: {caseFile}"
          failed := true
  return (if failed then 1 else 0)
