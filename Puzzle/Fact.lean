import Puzzle.Room
import Puzzle.Portal

abbrev PlayerFact := Room
abbrev PortalStateFact := PortalState
abbrev PortalSurfaceFact := Room

-- -- this is annoying
-- instance : OfNat PlayerFact a where ofNat := a
-- instance : OfNat PortalFact a where ofNat := a
-- instance : OfNat PortalSurfaceFact a where ofNat := a

-- -- feel like im doing something wrong
-- instance : Coe PlayerFact Nat := ⟨id⟩
-- instance : ToString PlayerFact where
--   toString pf := let foo : Nat := ↑pf; toString foo
-- instance : Coe PortalFact Nat := ⟨id⟩
-- instance : ToString PortalFact where
--   toString pf := let foo : Nat := ↑pf; toString foo
-- instance : Coe PortalSurfaceFact Nat := ⟨id⟩
-- instance : ToString PortalSurfaceFact where
--   toString pf := let foo : Nat := ↑pf; toString foo

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

structure FactSet where
  player: PlayerFact
  portal: PortalStateFact

  portalSurfaces: List PortalSurfaceFact
  freeConnections: List FreeConnectionFact
  fizzleConnections: List FizzleConnectionFact
  sightlines: List SightlineFact
deriving BEq, Repr, Hashable

instance : ToString FactSet where
  toString f := Std.Format.pretty $ Repr.reprPrec f 0

instance : Ord FactSet where
  compare a b := (compare a.player b.player).then (compare a.portal b.portal)

instance : LE FactSet := leOfOrd

def FactSet.shortString : FactSet → String
| set => s!"player {set.player}, p {set.portal}"

def FactSet.microString : FactSet → String
| set =>
  "p" ++ toString set.player ++ toString set.portal

def FactSet.clearPortals : FactSet → FactSet
| old => { old with portal := .noPortals}

def FactSet.changePortals : PortalState → FactSet → FactSet
| newState, old => { old with portal := newState}

-- def FactSet.getPortal : Portal → FactSet → Option Room
-- | .primary, set => set.primaryPortal
-- | .alternate, set => set.alternatePortal

-- def FactSet.getPortalPair : FactSet → Option (PortalFact × PortalFact)
-- | set => do ((← set.primaryPortal), (← set.alternatePortal))

-- def FactSet.shootPortal : Room → Portal → FactSet → FactSet
-- | room, .primary  , set => {set with primaryPortal   := some room}
-- | room, .alternate, set => {set with alternatePortal := some room}

def FactSet.walk : Room → FactSet → FactSet
| room, set => {set with player := room}

def FactSet.fizzlewalk : Room → FactSet → FactSet
| room => .walk room ∘ .clearPortals

def FactSet.freewalkDests : Room → FactSet → List Room
| room, set => set.freeConnections.filterMap (fun x =>
  if x.src == room then some x.dst else none)

def FactSet.fizzlewalkDests : Room → FactSet → List Room
| room, set => set.fizzleConnections.filterMap (fun x =>
  if x.src == room then some x.dst else none)

def FactSet.sightlineDests : Room → FactSet → List Room
| room, set => set.sightlines.filterMap (fun x =>
  if x.src == room then some x.dst else none)

def FactSet.visiblePortalSurfaces : Room → FactSet → List Room
| room, set =>
  let destinations := List.insert room $ set.sightlineDests room;
  set.portalSurfaces.filter (destinations.contains)

-- todo
