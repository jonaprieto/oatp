import OATP
import OATP.TPTP

open OATP OATP.TPTP

#guard SZSStatus.toString .theorem == "Theorem"
#guard (parseStatement "fof(goal, conjecture, p)." |>.isOk)
#guard (parseStatement "cnf(c1, axiom, p | ~q)." |>.isOk)
#guard match parseStatement "not-tptp" with
  | .error _ => true
  | .ok _ => false

def main : IO UInt32 := do
  IO.println "OATP tests passed"
  return 0
