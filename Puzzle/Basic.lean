import Puzzle.Nondet
import Puzzle.Portal
import Puzzle.Step
import Puzzle.Room
import Puzzle.Fact

-- Wander around through open space
def walkFree : FactSet → Nondet (Step × FactSet)
| facts => do
  let player := facts.player
  let dst ← Nondet.choices $ facts.freewalkDests player
  pure (Step.walkFree player dst, facts.walk dst)

-- Walk through a fizzler, deleting the portals
def walkFizzle : FactSet → Nondet (Step × FactSet)
| facts => do
  let player := facts.player
  let dst ← Nondet.choices $ facts.fizzlewalkDests player
  pure (Step.walkFizzle player dst, facts.fizzlewalk dst)

-- Travel through a pair of portals
def walkThroughPortals : FactSet → Nondet (Step × FactSet)
| facts => do
  let player := facts.player
  let (pri, alt) ← Nondet.choices $ Option.toList $ facts.getPortalPair
  if player == pri then
    pure (.walkPortal pri alt, facts.walk alt)
  else if player == alt then
    pure (.walkPortal alt pri, facts.walk pri)
  else default

-- Shoot portals
def shootPortals : Step × FactSet → Nondet (Step × FactSet)
| (lastStep, facts) => do
  let player := facts.player
  let color ← Nondet.choices $ [.primary, .alternate]

  if lastStep.firedThisPortal color then
    -- Don't trivially reshoot same portal twice in a row
    default

  let existingPortal := facts.getPortal color

  if let none := facts.getPortal color.compliment then
    if color == .alternate then
      -- Don't shoot alternate portal unless the primary exists
      -- this is sound UNTIL there's a way to lose only one portal at a time
      default

  let target ← Nondet.choices $ facts.visiblePortalSurfaces player
  if existingPortal == target then
    -- Don't reshoot portals into rooms they're already in
    -- Also that's Option Bool == Bool and it just works. Ok
    default

  pure (.firePortal target color, facts.shootPortal target color)

-- Take one action in the puzzle
def move : Step × FactSet → Nondet (Step × FactSet)
| (last, facts) => walkThroughPortals facts ++
                walkFree facts ++
                walkFizzle facts ++
                shootPortals (last, facts)

-- Take n actions in the puzzle
def move' : Nat → Nondet (Step × FactSet) → Nondet (Step × FactSet)
| 0,       state => state
| .succ i, state => Nondet.dedupe $ move' i (state >>= move)

def puzzle : FactSet := {
  player := 0,
  primaryPortal := none,
  alternatePortal := none,
  portalSurfaces := [0, 1, 2, 3],
  freeConnections := [],
  fizzleConnections := [(1, 2)],
  sightlines := [(0, 1), (2, 3)]
}

#eval move' 7 $ Nondet.pure (Step.start, puzzle)
