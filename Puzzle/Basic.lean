import Puzzle.Nondet

abbrev Room := Nat

/-- A portal! -/
inductive Portal
| primary
| alternate
deriving BEq, Repr

def Portal.compliment : Portal → Portal
| primary => alternate
| alternate => primary

/-- Things that are true about the current puzzle state -/
inductive Fact
| playerIn (pos: Room) : Fact
| portalIn (pos: Room) (portal: Portal) : Fact
| portalSurfaceIn (pos: Room) : Fact

| connectionFree (src: Room) (dst: Room) : Fact
| connectionFizzle (src: Room) (dst: Room) : Fact

| sightline (src: Room) (dst: Room) : Fact
deriving BEq, Repr

/-- Things you can do in the puzzle -/
inductive Step
| start : Step
| walkPortal : Room → Room → Step
| walkFree : Room → Room → Step
| walkFizzle : Room → Room → Step
| firePortal : Room → Portal → Step
deriving BEq, Repr

def Step.firedThisPortal : Portal → Step → Bool
| portal, (.firePortal _ portal') => portal == portal'
| _, _ => false

def Fact.keptOnFizzleWalk : Fact → Bool
| .portalIn _ _ => false
| _ => true

-- helpers

def findPlayers : List Fact → List Room
| (.playerIn room :: fs) => room :: findPlayers fs --keep looking because idk coop?
| (_ :: fs) => findPlayers fs
| [] => []

def findPortal : Portal → List Fact → Option Room
| color, (.portalIn room color' :: fs) => if color == color' then some room else findPortal color fs
| color, (_ :: fs) => findPortal color fs
| _, [] => none

def movePlayer : Room → Room → List Fact → List Fact
| src, dst, facts => facts.replace (.playerIn src) (.playerIn dst)

def firePortal : Room → Portal → List Fact → List Fact
| loc, portal, facts => (.portalIn loc portal) :: facts.filter (match · with
  | .portalIn _ portal' => portal != portal' -- keep only the other portal
  | _ => true)

-- these do linear scans over the fact lists, which is Not super great
-- Probably should upgrade List Fact to a nicer data structure

def freeWalkDests : Room → List Fact → List Room
| _, [] => []
| src, (.connectionFree src' dst :: fs) => if src == src' then dst :: freeWalkDests src fs
                                           else freeWalkDests src fs
| src, (_ :: fs) => freeWalkDests src fs

def fizzleWalkDests : Room → List Fact → List Room
| _, [] => []
| src, (.connectionFizzle src' dst :: fs) => if src == src' then dst :: fizzleWalkDests src fs
                                             else fizzleWalkDests src fs
| src, (_ :: fs) => fizzleWalkDests src fs

def lookDests : Room → List Fact → List Room
| src, [] => [src] -- can always see into your own room
| src, (.sightline src' dst :: fs) => if src == src' then dst :: lookDests src fs
                                      else lookDests src fs
| src, (_ :: fs) => lookDests src fs

def hasPortalSurface : Room → List Fact → Bool
| _, [] => false
| src, (.portalSurfaceIn src' :: fs) => if src == src' then true else hasPortalSurface src fs
| src, (_ :: fs) => hasPortalSurface src fs

def portalableLookDests : Room → List Fact → List Room
| src, fs => List.filter (hasPortalSurface · fs) (lookDests src fs)

-- Wander around through open space
def walkFree : List Fact → Nondet (Step × List Fact)
| facts => do
  let player ← Nondet.choices $ findPlayers facts
  let dst ← Nondet.choices $ freeWalkDests player facts
  pure (Step.walkFree player dst, movePlayer player dst facts)

-- Travel through a pair of portals
def walkThroughPortals : List Fact → Nondet (Step × List Fact)
| facts => do
  let player ← Nondet.choices $ findPlayers facts
  let priPortal ← Nondet.choices $ Option.toList $ findPortal .primary facts
  let altPortal ← Nondet.choices $ Option.toList $ findPortal .alternate facts
  if player == priPortal then
    pure (.walkPortal priPortal altPortal, movePlayer priPortal altPortal facts)
  else if player == altPortal then
    pure (.walkPortal altPortal priPortal, movePlayer altPortal priPortal facts)
  else default

-- Walk through a fizzler, deleting the portals
def walkFizzle : List Fact → Nondet (Step × List Fact)
| facts => do
  let player ← Nondet.choices $ findPlayers facts
  let dst ← Nondet.choices $ fizzleWalkDests player facts
  pure (Step.walkFizzle player dst, List.filter Fact.keptOnFizzleWalk $ movePlayer player dst facts)

-- Shoot portals
def shootPortals : Step × List Fact → Nondet (Step × List Fact)
| (lastStep, facts) => do
  let player ← Nondet.choices $ findPlayers facts
  let color ← Nondet.choices $ [.primary, .alternate]

  if lastStep.firedThisPortal color then
    -- Don't trivially reshoot same portal twice in a row
    default

  let existingPortal := findPortal color facts
  let otherPortal := findPortal (color.compliment) facts

  if let none := otherPortal then
    if color == .alternate then
      -- Don't shoot alternate portal unless the primary exists
      -- this is sound UNTIL there's a way to lose only portal at a time
      default

  let target ← Nondet.choices $ portalableLookDests player facts
  if existingPortal == target then
    -- Don't reshoot portals into rooms they're already in
    -- Also that's Option Bool == Bool and it just works. Ok
    default

  pure (.firePortal target color, firePortal target color facts)

-- Take one action in the puzzle
def move : Step × List Fact → Nondet (Step × List Fact)
| last@(_, facts) => walkThroughPortals facts ++
                walkFree facts ++
                walkFizzle facts ++
                shootPortals last

def move' : Nat → Nondet (Step × List Fact) → Nondet (Step × List Fact)
| 0,       state => state
| .succ i, state => Nondet.dedupe $ move' i (state >>= move)

def puzzle: List Fact := [
  .playerIn 0,
  .sightline 0 1,
  .portalSurfaceIn 0,
  .portalSurfaceIn 1,

  .connectionFizzle 1 2
]

#eval move' 2 (Nondet.pure (Step.start, puzzle))
