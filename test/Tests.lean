import OATP
import OATP.TPTP
import OATP.SystemOnTPTP

open OATP OATP.TPTP

#guard SZSStatus.toString .theorem == "Theorem"
#guard (parseStatement "fof(goal, conjecture, p)." |>.isOk)
#guard (parseStatement "cnf(c1, axiom, p | ~q)." |>.isOk)
#guard match parseStatement "not-tptp" with
  | .error _ => true
  | .ok _ => false
#guard OATP.SystemOnTPTP.encodeComponent "a b&c" == "a%20b%26c"
#guard OATP.SystemOnTPTP.encodeForm #[
  { name := "x", value := "a b" },
  { name := "y", value := "✓" }
] == "x=a%20b&y=%E2%9C%93"

def main : IO UInt32 := do
  IO.println "OATP tests passed"
  return 0
