import Puzzle.Room

inductive CubePosition where
| nowhere: CubePosition
| inRoom: Room → CubePosition
| inHands: CubePosition
deriving BEq, Ord, Hashable, Repr

def CubePosition.isHeld : CubePosition → Bool
| .inHands => true
| _ => false

def CubePosition.isInRoom : Room → CubePosition → Bool
| _, .nowhere => false
| _, .inHands => true
| r1, .inRoom r2 => r1 == r2

instance : ToString CubePosition where
  toString
  | .nowhere => "×"
  | .inHands => "held"
  | .inRoom r => toString r

structure CubeFact where
  id: CubeId
  autorespawn: Option CubePosition
  position: CubePosition
deriving BEq, Ord, Hashable, Repr

def CubeFact.isHeld := CubePosition.isHeld ∘ CubeFact.position
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
