import Puzzle.Room
import Puzzle.Nondet

inductive PortalState
| noPortals: PortalState
| onePortal: Room → PortalState
-- TODO: include a proof that a ≤ b
| twoPortals : (a: Room) → (b: Room) → PortalState
deriving BEq, Repr, Hashable, Ord

-- TODO: actually prove stuff lol
-- Lean doesn't include any lemmas about LE i can find
def PortalState.fireInto: Room → PortalState → Nondet PortalState
| new, .noPortals => Nondet.pure $ .onePortal new
| new, .onePortal old =>
  if l : new < old then
    Nondet.choices [
      .onePortal new,
      .twoPortals new old
    ]
  else if e : new == old then
    Nondet.pure $ .twoPortals new new
  else Nondet.choices $ [
    .onePortal new,
    .twoPortals old new
  ]
| new, .twoPortals oldA oldB =>
  if oldA == oldB then
    if new ≤ oldA then Nondet.pure $ .twoPortals new oldA
    else Nondet.pure $ .twoPortals oldA new
  else if new ≤ oldA then Nondet.choices [
    .twoPortals new oldA,
    .twoPortals new oldB
  ] else if new ≤ oldB then Nondet.choices [
    .twoPortals oldA new,
    .twoPortals new oldB
  ] else Nondet.choices [
    .twoPortals oldA new,
    .twoPortals oldB new
  ]

instance : ToString PortalState where
  toString
  | .noPortals => ""
  | .onePortal a => s!"a{a}"
  | .twoPortals a b => s!"a{a}b{b}"

instance : Repr PortalState where
  reprPrec s _ := toString s
