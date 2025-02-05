import Puzzle.Nondet
import Puzzle.Portal
import Puzzle.Room
import Puzzle.Fact

import Std.Data.HashMap.Basic

-- Wander around through open space
def walkFree : FactSet → Nondet FactSet
| facts => do
  let player := facts.player
  let dst ← Nondet.choices $ facts.freewalkDests player
  pure (facts.walk dst)

-- Walk through a fizzler, deleting the portals
def walkFizzle : FactSet → Nondet FactSet
| facts => do
  let player := facts.player
  let dst ← Nondet.choices $ facts.fizzlewalkDests player
  pure (facts.fizzlewalk dst)

-- Travel through a pair of portals
def walkThroughPortals : FactSet → Nondet FactSet
| facts => do
  let player := facts.player
  let (pri, alt) ← Nondet.choices $ Option.toList $ facts.getPortalPair
  if player == pri then
    pure (facts.walk alt)
  else if player == alt then
    pure (facts.walk pri)
  else default

-- Shoot portals
def shootPortals : FactSet → Nondet FactSet
| facts => do
  let player := facts.player
  let color ← Nondet.choices $ [.primary, .alternate]

  let existingPortal := facts.getPortal color

  if color == .alternate then
    if let none := facts.getPortal color.complement then
      -- Don't shoot alternate portal unless the primary exists
      -- this is sound UNTIL there's a way to lose only one portal at a time
      default

  let target ← Nondet.choices $ facts.visiblePortalSurfaces player
  if existingPortal == target then
    -- Don't reshoot portals into rooms they're already in
    -- Also that's Option Bool == Bool and it just works. Ok
    default

  pure (facts.shootPortal target color)

-- Take one action in the puzzle
def move : FactSet → Nondet FactSet
| facts =>
  walkThroughPortals facts ++
  walkFree facts ++
  walkFizzle facts ++
  shootPortals facts

-- Take n actions in the puzzle
def move' : Nat → Nondet FactSet → Nondet FactSet
| 0,       state => state
| .succ i, state => move' i (state >>= move)

--------- PUZZLE STATE MANAGEMENT
def StateGraph := Std.HashMap FactSet (List FactSet)

instance : EmptyCollection StateGraph where
  emptyCollection := Std.HashMap.empty

--i implemented Ord /and/ LE why cant it find it
def sortDammit : List FactSet → List FactSet
| list => List.mergeSort list (Ordering.isLE $ compare · ·)

def toDot : StateGraph → String
| graph =>
  let connections := Nondet.choices graph.keys >>= (fun key =>
    Nondet.choices (Std.HashMap.get! graph key) >>= (fun val => (
      Nondet.pure $ "  " ++ key.microString ++ " -> " ++ val.microString ++ ";"
    )))
  let connections' := String.join $ List.intersperseTR "\n" connections.toList
  "digraph g {\n" ++ connections' ++ "\n}"

-- instance : Repr StateGraph where
--   reprPrec g prec := let one := (fun (f : FactSet) =>
--     -- reprPrec f prec
--     f.shortString
--     ++ " → "
--     ++ reprPrec (List.length $ Std.HashMap.get! g f) prec
--     ++ " states")
--   Std.Format.join $ List.intersperseTR "\n" $ one <$> (sortDammit $ g.keys)
instance : Repr StateGraph where
  reprPrec g _prec := toDot g

partial def buildGraph : FactSet → StateGraph → StateGraph
| facts, graph =>
  let steppedFacts := Nondet.toList (move facts)
  let newFacts := steppedFacts.filter (!graph.contains ·)
  Id.run do
    let mut graph := graph.insert facts steppedFacts;
    for recurFact in newFacts do
      graph := buildGraph recurFact graph
    graph
  -- lean do sugar is actually the funniest thing in the world
  -- this is ridiculous. whats the "correct" functional way to do this


def puzzle : FactSet := {
  player := 0,
  primaryPortal := none,
  alternatePortal := none,
  portalSurfaces := [0, 1, 2, 3],
  freeConnections := [(3, 4)],
  fizzleConnections := [(1, 2)],
  sightlines := [(0, 1), (2, 3)]
}

#eval move' 2 $ Nondet.pure puzzle

#eval buildGraph puzzle ∅
