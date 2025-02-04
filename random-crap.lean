
-- Arith example just to learn about monads lol

inductive Expr (op : Type) where
| const: Int → Expr op
| binary : op → Expr op → Expr op → Expr op
deriving Repr

inductive Arith where
| plus
| minus
| times
| div
deriving Repr

def evalOpt : Arith → Int → Int → Option Int
| .plus , x, y => some $ x + y
| .minus, x, y => some $ x - y
| .times, x, y => some $ x * y
| .div  , x, y => if y == 0 then none else some $ x / y

def evalM [Monad m] (evalBin: Arith → Int → Int → m Int): Expr Arith → m Int
| .const i => pure i
| .binary op x y => do
  let x ← evalM evalBin x
  let y ← evalM evalBin y
  evalBin op x y

-- value on left, log on right

structure Writer (ε α : Type) where
  log : ε
  val : α
deriving Repr

def Writer.of [Inhabited ε] : α → Writer ε α
| a => { val := a, log := default }

def Writer.flatMap [Append ε] : Writer ε α → (α → Writer ε β) → Writer ε β
| prev, nextfn => let next := nextfn prev.val;
                  {val := next.val, log := (prev.log ++ next.log)}

instance [Inhabited ε] [Append ε] : Monad (Writer ε) where
  pure := Writer.of
  bind := Writer.flatMap

def noisyAddOne : Nat → Writer (List String) Nat
| x => {log := ["Added one"], val := x + 1}

def noisyTimesTwo : Nat → Writer (List String) Nat
| x => {log := ["Times two"], val := x * 2}

#eval Writer.of 0 >>= noisyAddOne >>= noisyAddOne >>= noisyTimesTwo >>= noisyAddOne

-- ok lets get even simpler
-- the "state" is a number and the actions are "halve" and "multiply by three add one" as appropriate

abbrev CollatzState := Nat

inductive CollatzAction
| halve : CollatzAction
| timesThreePlusOne : CollatzAction
deriving Repr

-- take one action in the game and return state × action i took
def cstep : CollatzState → CollatzState × CollatzAction
| state => if state % 2 == 0 then (state / 2, .halve)
           else (state * 3 + 1, .timesThreePlusOne)

-- take one action in the game, threading the current history through
def cstep_once : CollatzState × List CollatzAction → CollatzState × List CollatzAction
| (oldState, oldActions) =>
  let stepped := cstep oldState;
  (stepped.fst, List.concat oldActions stepped.snd)

-- repeatedly take actions in the game until reaching state `1`
-- removing this "partial" will prove a little difficult
partial def cstep_till_one : CollatzState × List CollatzAction → CollatzState × List CollatzAction
| curr@(1, _) => curr
| curr => cstep_till_one $ cstep_once curr

#eval cstep_till_one (27, [])

def step' : CollatzState → Writer (List CollatzAction) CollatzState
| state => if state % 2 == 0 then {val := state / 2, log := [.halve]}
           else {val := state * 3 + 1, log := [.timesThreePlusOne]}

def stepN' : Nat → Writer (List CollatzAction) CollatzState → Writer (List CollatzAction) CollatzState
| 0, state => state
| .succ j, state => stepN' j (state >>= step')

-- remove 'partial' from this function and earn 1 million dollars!
partial def stepToOne' : Writer (List CollatzAction) CollatzState → Writer (List CollatzAction) CollatzState
| state => if state.val == 1 then state else stepToOne' (state >>= step')

#eval stepToOne' (Writer.of 27)



-- todo: List.eraseDups (requires α is BEq)

def choiches : Nat → Nondet Nat
| n => .choices [n + 1, n * 3]

#eval Nondet.pure 5 >>= choiches >>= choiches >>= choiches >>= choiches >>= choiches >>= choiches
