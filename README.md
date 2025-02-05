# puzzle

All the good stuff is in Puzzle/Basic.lean right now

## how should this work

I think i should ignore "step types" right now and focus on only the connectivity of the state space.I dont want to confuse myself trying to weigh "reach X from Y with a fancy move" against "reach X from Y by walking along the ground"

I think at a high level the puzzle-finding algorithm should be
* generate puzzle
* try to solve puzzle (keeping track of only the state-space shape)
* if unsolvable, continue, otherwise
* evaluate the shape of the state space. (graph theory shit)
  * no dead-ends?
  * number of "important" nodes (all paths from the beginning to the end share). i think this is called the min vertex cut or something?
* after that i can worry about taking 2 states and reverse engineering what move goes from A to B
* or maybe other things, like adding sightlines until the solution changes (kinda like a reverse quickcheck)

so the solver part should work like this

* start with state
* nondeterministically take all paths out of this state
* add edges (original state → new state)
* for each state that does not exist in "the current graph":
  * recur.
  * this "recur" also modifies "the current graph" and might render more states redundant
