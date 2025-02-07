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

(later)

makes sense to do this i think

- explore all "walk around" moves until there aren't any more
- then start picking up cubes and shooting portals

hoping for early-cutoff if you walk into the exit door

## regarding grabbing cubes

currently i model cubes being in three kinds of position: nowhere, in your hands, or on the ground. (later i will add "on button" and stuff.) you can do four things

- pick a cube up from the current room and move it to your hands
- pick a cube up from an adjacent room blocked by a fizzler and dissolve it
- drop a cube from your hands to somewhere in the current room
- drop a cube from your hands into an adjacent fizzler 

if you walk through a fizzler while a cube is in your hands, the cube is destroyed

might want to change this into

- while you move through open space, you can take *any combination* of cubes with you
- if there is a fizzler in the current room, you can dissolve any cube in the room
- if there is a fizzler blocking an adjacent room, you can dissolve cubes in that room that aren't busy resting on a button or something (same concept)

basically this removes the "cube in hands" state, which will halve the number of puzzle states (`player in room 0, cube in room 0` vs `player in room 0, cube in hands`), and i think it's also more realistic (if you have a one-way ledge, you can bring more than one cube by tossing them down the ledge)

## todo

ok that opens up a different idea: what about dropping cubes off ledges without actually going there yourself

so "moving cubes" and "moving the player" are actually 2 different actions that don't need to happen in concert

so maybe walking with cubes is actually alternating between first kicking the cube into that room, and then following it with your player. same for putting cubes through portals.

and then! we don't actually need to take "every combination" of cubes anymore, since you can just repeat the action of kicking cubes! so taking every combination will only add more edges and make the graph denser. (might not be a terrible thing)