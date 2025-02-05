import Puzzle.Room
import Puzzle.Portal

/-- Things you can do in the puzzle -/

----- TODOOOOOOOOOOOO not used

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
