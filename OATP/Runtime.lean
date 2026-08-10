/-
Copyright (c) 2026 Jonathan Cubides. All rights reserved.
Released under Apache-2.0 license as described in the file LICENSE.
Authors: Jonathan Cubides
-/

import OATP.Http
import OATP.Portfolio
import OATP.SystemOnTPTP

/-!
# OATP.Runtime: shared CLI and REPL policies

This module owns filesystem and catalogue policy. Process and portfolio execution remain in their
dedicated modules so interactive and batch clients share the same boundaries.
-/

namespace OATP.Runtime

open OATP

inductive CatalogueCache where
  | normal
  | refresh
  | noCache

def readProblem (path : String) : IO Problem := do
  pure { name := path, source := ← IO.FS.readFile path }

def localProverCandidates : IO (Array String) := do
  match ← IO.getEnv "OATP_LOCAL_PROVERS" with
  | some value =>
      pure <| value.splitOn "," |>.map (·.trimAscii.toString) |>.filter (!·.isEmpty) |>.toArray
  | none => pure #["eprover", "vampire", "metis"]

def catalogueLocation (cacheNamespace endpoint : String) :
    IO (Option (System.FilePath × System.FilePath)) := do
  let root ← match ← IO.getEnv "XDG_CACHE_HOME" with
    | some path => pure (some (⟨path⟩ : System.FilePath))
    | none => match ← IO.getEnv "HOME" with
      | some path => pure (some (System.FilePath.join (⟨path⟩ : System.FilePath) ".cache"))
      | none => pure none
  let safeNamespace := String.ofList (cacheNamespace.toList.map fun character =>
    if character.isAlphanum || character == '-' || character == '_' then character else '_')
  pure <| root.map fun root =>
    let cacheName := if safeNamespace.isEmpty then "tool" else safeNamespace
    let normalizedEndpoint := endpoint.trimAscii.toString
    let directory := System.FilePath.join root cacheName
    (directory, System.FilePath.join directory s!"systems-{hash normalizedEndpoint}.html")

def httpErrorMessage : OATP.Http.Error → String
  | .io message => s!"HTTP IO failed: {message}"
  | .invalidRequest message => s!"invalid HTTP request: {message}"
  | .transport message => s!"HTTP transport failed: {message}"
  | .malformedStatus output => s!"HTTP response had no usable status: {output}"
  | .requestBodyTooLarge actual limit =>
      s!"HTTP request exceeded {limit} bytes ({actual} captured)"
  | .bodyTooLarge actual limit =>
      s!"HTTP response exceeded {limit} bytes ({actual} captured)"

def fetchCatalogue (endpoint : String)
    (location : Option (System.FilePath × System.FilePath))
    (writeCache : Bool) : IO (Except String (Array SystemOnTPTP.Catalogue.SystemInfo)) := do
  match ← SystemOnTPTP.fetchCatalogue endpoint with
  | .error error => pure (.error (httpErrorMessage error))
  | .ok response =>
      if response.statusCode < 200 || response.statusCode ≥ 300 then
        pure (.error s!"SystemOnTPTP catalogue returned HTTP {response.statusCode}")
      else
        let systems := SystemOnTPTP.Catalogue.parse response.body
        if systems.isEmpty then
          pure (.error "SystemOnTPTP catalogue contained no prover systems")
        else
          if writeCache then
            for (directory, path) in location do
              try
                IO.FS.createDirAll directory
                IO.FS.writeFile path response.body
              catch _ => pure ()
          pure (.ok systems)

def loadCatalogue (cacheNamespace endpoint : String) (mode : CatalogueCache) :
    IO (Except String (Array SystemOnTPTP.Catalogue.SystemInfo)) := do
  let location ← catalogueLocation cacheNamespace endpoint
  match mode with
  | .normal =>
      match location with
      | some (_, path) =>
          try
            let systems := SystemOnTPTP.Catalogue.parse (← IO.FS.readFile path)
            if systems.isEmpty then
              fetchCatalogue endpoint location true
            else pure (.ok systems)
          catch _ => fetchCatalogue endpoint location true
      | none => fetchCatalogue endpoint none false
  | .refresh => fetchCatalogue endpoint location true
  | .noCache => fetchCatalogue endpoint none false

def installedProvers : IO (Array String) := do
  let mut found := #[]
  for executable in ← localProverCandidates do
    if (← Http.commandVersion executable).isSome then
      found := found.push executable
  pure found

def resolveOnline (toolName : String) (systems : Array SystemOnTPTP.Catalogue.SystemInfo)
    (references : List String) : Except String (Array SystemOnTPTP.Catalogue.SystemInfo) := do
  let resolved ← references.mapM fun reference =>
    match SystemOnTPTP.Catalogue.resolve systems reference with
    | some system => pure system
    | none => Except.error (s!"online prover `{reference}` is not in the catalogue; " ++
        s!"run `{toolName} systems --online --refresh`")
  pure resolved.toArray

end OATP.Runtime
