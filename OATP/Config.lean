/-
Copyright (c) 2026 Jonathan Cubides. All rights reserved.
Released under Apache-2.0 license as described in the file LICENSE.
-/

import Grip.Json
import OATP.TPTP

namespace OATP.Config

open Grip.Json

structure Preferences where
  theory : String := OATP.TPTP.defaultTheory
  defaultProver : String := ""
  enabledProvers : Array String := #[]
  proverSelectionSet : Bool := false
  strategy : String := "all"
  theme : String := "aurora"
  deriving BEq, DecidableEq, Repr

def default : Preferences := {}

private def configRoot : IO (Option System.FilePath) := do
  match ← IO.getEnv "XDG_CONFIG_HOME" with
  | some path => pure (some ⟨path⟩)
  | none =>
      match ← IO.getEnv "HOME" with
      | some path => pure (some (System.FilePath.join ⟨path⟩ ".config"))
      | none => pure none

def directory : IO (Option System.FilePath) := do
  pure <| (← configRoot).map fun root => System.FilePath.join root "oatp"

def path : IO (Option System.FilePath) := do
  pure <| (← directory).map fun root => System.FilePath.join root "config.json"

private def field (json : Json) (name : String) : Option Json := json.get? name

private def stringField (json : Json) (name fallback : String) : String :=
  match field json name with
  | some (.str value) => value
  | _ => fallback

private def stringArrayField (json : Json) (name : String) : Array String :=
  match field json name with
  | some (.arr values) =>
      let result : List String := values.toList.filterMap fun value =>
        match value with
        | .str value => some value
        | _ => none
      result.toArray
  | _ => #[]

private def fromJson : Json → Preferences
  | json => {
      theory := stringField json "theory" default.theory
      defaultProver := stringField json "defaultProver" default.defaultProver
      enabledProvers := stringArrayField json "enabledProvers"
      proverSelectionSet := match field json "proverSelectionSet" with
        | some (.bool value) => value
        | _ => default.proverSelectionSet
      strategy := stringField json "strategy" default.strategy
      theme := stringField json "theme" default.theme
    }

private def toJson (preferences : Preferences) : Json :=
  .obj #[
    ("theory", .str preferences.theory),
    ("defaultProver", .str preferences.defaultProver),
    ("enabledProvers", .arr (preferences.enabledProvers.map Json.str)),
    ("proverSelectionSet", .bool preferences.proverSelectionSet),
    ("strategy", .str preferences.strategy),
    ("theme", .str preferences.theme)
  ]

def load : IO (Preferences × Option String) := do
  match ← path with
  | none => pure (default, none)
  | some file =>
      try
        let source ← IO.FS.readFile file
        match Grip.Json.parseString source with
        | .ok json => pure (fromJson json, none)
        | .error _ => pure (default, some s!"invalid config `{file}`: expected a JSON object")
      catch _ => pure (default, none)

def save (preferences : Preferences) : IO (Option String) := do
  match ← path with
  | none => pure (some "cannot persist preferences: HOME/XDG_CONFIG_HOME is unset")
  | some file =>
      try
        let directory := file.parent.getD ⟨"."⟩
        IO.FS.createDirAll directory
        IO.FS.writeFile file (Json.render (toJson preferences) ++ "\n")
        pure none
      catch error => pure (some s!"could not write `{file}`: {error}")

end OATP.Config
