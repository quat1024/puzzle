import Puzzle.Nondet
import Puzzle.Portal
import Puzzle.Room
import Puzzle.Fact

import Std.Data.HashMap.Basic

-- Wander through open space.
def walkFree : FactSet → Nondet FactSet
| facts => do
  let player := facts.player
  let dst ← Nondet.choices $ facts.freewalkDests player
  pure (facts.walk dst)

-- Push cubes through open space.
def cubeFree : FactSet → Nondet FactSet
| facts => do
  let player := facts.player
  let dst ← Nondet.choices $ facts.freewalkDests player
  let cubeToPush ← Nondet.choices $ facts.grabbableCubes player
  pure (facts.moveCube cubeToPush (.inRoom dst))

-- Travel through a pair of portals.
def walkPortal : FactSet → Nondet FactSet
| facts => do
  let player := facts.player
  if let (.twoPortals a b) := facts.portal then
    if a == b then default
    else if player == a then pure (facts.walk b)
    else if player == b then pure (facts.walk a)
    else default
  else default

-- Push a cube through a pair of portals.
def cubePortal : FactSet → Nondet FactSet
| facts => do
  let player := facts.player
  let cubeToPush ← Nondet.choices $ facts.grabbableCubes player
  if let (.twoPortals a b) := facts.portal then
    if a == b then default
    else if player == a then pure (facts.moveCube cubeToPush (.inRoom b))
    else if player == b then pure (facts.moveCube cubeToPush (.inRoom a))
    else default
  else default

-- Walk through a fizzler, deleting portals
def walkFizzle : FactSet → Nondet FactSet
| facts => do
  let player := facts.player
  let dst ← Nondet.choices $ facts.fizzlewalkDests player
  pure (facts.fizzlewalk dst)

-- Destroy cubes in fizzlers
def cubeFizzle : FactSet → Nondet FactSet
| facts => do
  let player := facts.player

  -- Take cubes in this room, destroy them in an adjacent fizzler
  let fizzleHere := do
    -- need at least one fizzler in the room but i don't care which one
    -- todo: approximation; what about grated fizzlers
    let _ ← Nondet.choices $ facts.fizzlewalkDests player |> List.take 1
    let cubeToFizzle ← Nondet.choices $ facts.grabbableCubes player
    pure (facts.destroyCube cubeToFizzle)

  -- Grab cubes through fizzlers in adjacent rooms and destroy them through the fizzler
  let fizzleThere := do
    let thruFizzlerDest ← Nondet.choices $ facts.fizzlewalkDests player
    let cubeThruFizzler ← Nondet.choices $ facts.dissolvableThruFizzlerCubes thruFizzlerDest
    pure (facts.destroyCube cubeThruFizzler)

  fizzleHere ++ fizzleThere

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
  Nondet.choices [
    walkFree, cubeFree,
    walkPortal, cubePortal,
    walkFizzle, cubeFizzle,
    shootPortals
  ] >>= (· facts)

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

instance : Repr StateGraph where
  reprPrec g _prec := toDot g

partial def buildGraph : StateGraph → FactSet → StateGraph
| graph, facts =>
  Id.run do
    let steppedFacts := Nondet.toList (move facts)
    let mut graph := graph.insert facts steppedFacts;
    for recurFact in steppedFacts do
      if !graph.contains recurFact then
        graph := buildGraph graph recurFact
    graph
  -- lean do sugar is actually the funniest thing in the world
  -- this is ridiculous. whats the "correct" functional way to do this

def buildGraph': FactSet → StateGraph := buildGraph ∅

def puzzle : FactSet := {
  player := 0,
  portal := .noPortals,
  portalSurfaces := [0, 1, 2, 3],
  freeConnections := [(3, 4), (4, 3)],
  fizzleConnections := [(1, 2)],
  sightlines := [(0, 1), (1,0), (2, 3), (3, 2)],
  cubes := [{
    id := 0,
    autorespawn := some (.inRoom 0),
    position := .inRoom 0
  }]
}

def puzzle' : FactSet := {
  player := 0,
  portal := .noPortals,
  portalSurfaces := [3, 4, 7, 8],
  freeConnections := [(0, 1), (1,0), (1,2), (2,1), (2,3), (3,2), (4,5), (5,4), (5,6), (6,5), (6, 7), (7,6)],
  fizzleConnections := [],
  sightlines := [(3, 4), (4,3), (7,8)],
  cubes := [{
    id := 0,
    autorespawn := some (.inRoom 3),
    position := .inRoom 1
  },{
    id := 1,
    autorespawn := some (.inRoom 3),
    position := .inRoom 1
  }]
}

def puz : FactSet := {
  player := 0,
  portal := .noPortals,
  portalSurfaces := [],
  freeConnections := [(0, 1), (1, 0)],
  fizzleConnections := [(1, 2), (2, 1)],
  sightlines := [],
  cubes := [{
    id := 0,
    autorespawn := none,
    position := .inRoom 0
  }],
}
#eval buildGraph' puz

#eval Nondet.everyCombination $ Nondet.choices $ puz.grabbableCubes 0

-- #eval move' 2 $ Nondet.pure puzzle
#eval buildGraph' puzzle
#eval buildGraph' puzzle'
