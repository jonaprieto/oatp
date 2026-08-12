/-
Copyright (c) 2026 Jonathan Cubides. All rights reserved.
Released under Apache-2.0 license as described in the file LICENSE.
-/

import OATP.Config

/-!
# OATP.Artifacts: persistent prover-run files

Each transport call keeps the exact TPTP input and captured output in a small
run directory. The working directory is preferred; the OATP config directory
is the fallback for read-only working trees.
-/

namespace OATP.Artifacts

open OATP

structure Run where
  directory : System.FilePath
  deriving Repr

private def safeName (value : String) : String :=
  let name := String.ofList <| value.toList.map fun character =>
    if character.isAlphanum || character == '-' || character == '_' then character else '_'
  if name.isEmpty then "run" else name

private def roots : IO (List System.FilePath) := do
  let current ← IO.currentDir
  let fallback := (← OATP.Config.path).map fun path =>
    path.parent.getD (⟨"."⟩ : System.FilePath)
  pure <| System.FilePath.join current ".oatp" :: fallback.toList

def start (label : String) (problem : Problem) : IO (Option Run) := do
  let stamp ← IO.monoMsNow
  for root in ← roots do
    let directory := System.FilePath.join root s!"run-{stamp}-{safeName label}"
    try
      IO.FS.createDirAll directory
      IO.FS.writeFile (System.FilePath.join directory "problem.tptp") problem.source
      IO.FS.writeFile (System.FilePath.join directory "prover.txt") label
      return some { directory }
    catch _ => pure ()
  pure none

def write (run : Run) (name content : String) : IO Unit := do
  try IO.FS.writeFile (System.FilePath.join run.directory name) content
  catch _ => pure ()

def writeCommand (run : Run) (command : String) : IO Unit :=
  write run "command.txt" command

end OATP.Artifacts
