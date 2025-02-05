-- lean doesn't define that "nondeterministic choice" monad for lists so idk make one
inductive Nondet (α : Type) where
| choices: List α → Nondet α

def Nondet.toList : Nondet α → List α
| .choices list => list

instance : Append (Nondet α) where
  append
  | .choices a, .choices b => .choices $ (a ++ b)

instance : Inhabited (Nondet α) where
  default := .choices []

def Nondet.pure : α → Nondet α := (.choices [·])
def Nondet.bind : Nondet α → (α → Nondet β) → Nondet β
| .choices .nil, _         => default
| .choices (.cons a as), f => (f a) ++ Nondet.bind (.choices as) f

instance : Monad Nondet where
  pure := Nondet.pure
  bind := Nondet.bind

def Nondet.dedupe [BEq α]: Nondet α → Nondet α
| .choices f => .choices $ List.eraseDups f
