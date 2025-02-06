import Puzzle.Room
import Puzzle.Portal

abbrev PlayerFact := Room
abbrev PortalStateFact := PortalState
abbrev PortalSurfaceFact := Room

structure FreeConnectionFact where
  src: Room
  dst: Room
deriving BEq, Ord, Hashable, Repr

structure FizzleConnectionFact where
  src: Room
  dst: Room
deriving BEq, Ord, Hashable, Repr

structure SightlineFact where
  src: Room
  dst: Room
deriving BEq, Ord, Hashable, Repr

-- easy typing
instance : Coe (Nat × Nat) FreeConnectionFact where
  coe pair := let (fst, snd) := pair; {src := fst, dst := snd}
instance : Coe (Nat × Nat) FizzleConnectionFact where
  coe pair := let (fst, snd) := pair; {src := fst, dst := snd}
instance : Coe (Nat × Nat) SightlineFact where
  coe pair := let (fst, snd) := pair; {src := fst, dst := snd}

inductive CubePositionFact where
| nowhere: CubePositionFact
| inRoom: Room → CubePositionFact
| inHands: CubePositionFact
deriving BEq, Ord, Hashable, Repr

def CubePositionFact.isHeld : CubePositionFact → Bool
| .inHands => true
| _ => false

def CubePositionFact.isInRoom : Room → CubePositionFact → Bool
| _, .nowhere => false
| _, .inHands => true
| r1, .inRoom r2 => r1 == r2

instance : ToString CubePositionFact where
  toString
  | .nowhere => "×"
  | .inHands => "held"
  | .inRoom r => toString r

structure CubeFact where
  id: CubeId
  autorespawn: Option CubePositionFact
  position: CubePositionFact
deriving BEq, Ord, Hashable, Repr

def CubeFact.isHeld := CubePositionFact.isHeld ∘ CubeFact.position
def CubeFact.isInRoom : Room → CubeFact → Bool
| room, fact => fact.position.isInRoom room

def CubeFact.destroy : CubeFact → CubeFact
| c =>
  if let some respawn := c.autorespawn then
    {c with position := respawn}
  else
    {c with position := .nowhere}

instance : ToString CubeFact where
  toString f := s!"{f.id}{f.position}"

def CubeFact.compareLists : List CubeFact → List CubeFact → Ordering
| [], [] => .eq
| _x, [] => .gt
| [], _y => .lt
| x :: xs, y :: ys => (compare x y).then (compareLists xs ys)

instance : Ord (List CubeFact) where
  compare := CubeFact.compareLists

structure FactSet where
  player: PlayerFact
  portal: PortalStateFact

  portalSurfaces: List PortalSurfaceFact
  freeConnections: List FreeConnectionFact
  fizzleConnections: List FizzleConnectionFact
  sightlines: List SightlineFact

  cubes: List CubeFact
deriving BEq, Repr, Hashable

instance : ToString FactSet where
  toString f := Std.Format.pretty $ Repr.reprPrec f 0

instance : Ord FactSet where
  compare a b := ((compare a.player b.player).then (compare a.portal b.portal)).then (compare a.cubes b.cubes)

instance : LE FactSet := leOfOrd

-- printing
def FactSet.shortString : FactSet → String
| set => s!"player {set.player}, p {set.portal}, c {String.join $ List.intersperse "/" $ toString <$> set.cubes}"

def FactSet.microString : FactSet → String
| set =>
  "p" ++ toString set.player ++ toString set.portal ++ "c" ++ (String.join $ List.intersperse "/" $ toString <$> set.cubes)

-- cubes
def FactSet.destroyCube : CubeFact → FactSet → FactSet
| toDestroy, set => { set with cubes := (fun x => if x == toDestroy then CubeFact.destroy x else x) <$> set.cubes}

def FactSet.destroyHeldCube : FactSet → FactSet
| set => { set with cubes := (fun x => if CubeFact.isHeld x then CubeFact.destroy x else x) <$> set.cubes}

def FactSet.getHeldCube : FactSet → Option CubeFact
| set => List.head? $ List.filter CubeFact.isHeld set.cubes

def FactSet.isHoldingCube : FactSet → Bool := Option.isSome ∘ FactSet.getHeldCube

def FactSet.moveCube : CubeFact → CubePositionFact → FactSet → FactSet
| toPlace, dest, set => { set with cubes :=
  (fun x => if x == toPlace then {x with position := dest} else x) <$> set.cubes}

-- portals
def FactSet.changePortals : PortalState → FactSet → FactSet
| newState, set => { set with portal := newState}

-- walking
def FactSet.walk : Room → FactSet → FactSet
| room, set => {set with player := room}

def FactSet.fizzlewalk : Room → FactSet → FactSet
| room => .walk room ∘ .changePortals .noPortals ∘ FactSet.destroyHeldCube

-- helpers
def FactSet.freewalkDests : Room → FactSet → List Room
| room, set => set.freeConnections.filterMap (fun x =>
  if x.src == room then some x.dst else none)

def FactSet.fizzlewalkDests : Room → FactSet → List Room
| room, set => set.fizzleConnections.filterMap (fun x =>
  if x.src == room then some x.dst else none)

def FactSet.sightlineDests : Room → FactSet → List Room
| room, set => set.sightlines.filterMap (fun x =>
  if x.src == room then some x.dst else none)

def FactSet.cubeDropDests : Room → FactSet → List CubePositionFact
| room, _ => [.inRoom room]

def FactSet.grabbableCubes : Room → FactSet → List CubeFact
| room, set => List.filter (CubeFact.isInRoom room) set.cubes

def FactSet.visiblePortalSurfaces : Room → FactSet → List Room
| room, set =>
  let destinations := List.insert room $ set.sightlineDests room;
  set.portalSurfaces.filter (destinations.contains)
