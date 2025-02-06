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
  if let (.twoPortals a b) := facts.portal then
    if a == b then default
    else if player == a then pure (facts.walk b)
    else if player == b then pure (facts.walk a)
    else default
  else default

-- Shoot portals
def shootPortals : FactSet → Nondet FactSet
| facts => do
  let player := facts.player
  let target ← Nondet.choices $ facts.visiblePortalSurfaces player
  let newPortal ← facts.portal.fireInto target
  if facts.portal != newPortal then
    pure (facts.changePortals newPortal)
  else default

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
  let keys := sortDammit graph.keys
  let connections := Nondet.choices keys >>= (fun key =>
    Nondet.choices (sortDammit (Std.HashMap.get! graph key)) >>= (fun val => (
      let most := "  \"" ++ key.microString ++ "\" -> \"" ++ val.microString ++ "\"";
      if (Std.HashMap.getD graph val []).contains key then
        if key ≤ val then
          Nondet.pure $ most ++ " [dir=both];"
        else
          default
      else Nondet.pure $ most ++ ";"
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
  portal := .noPortals,
  portalSurfaces := [0, 1, 2, 3],
  freeConnections := [(3, 4)],
  fizzleConnections := [(1, 2)],
  sightlines := [(0, 1), (2, 3)]
}

def puzzle' : FactSet := {
  player := 0,
  portal := .noPortals,
  portalSurfaces := [3, 4, 7, 8],
  freeConnections := [(0, 1), (1,0), (1,2), (2,1), (2,3), (3,2), (4,5), (5,4), (5,6), (6,5), (6, 7), (7,6)],
  fizzleConnections := [],
  sightlines := [(3, 4), (4,3), (7,8)]
}

#eval move' 2 $ Nondet.pure puzzle
#eval buildGraph puzzle ∅
#eval buildGraph puzzle' ∅
