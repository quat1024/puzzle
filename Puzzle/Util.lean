def Util.compareLists [Ord α]: List α → List α → Ordering
| [], [] => .eq
| _x, [] => .gt
| [], _y => .lt
| x :: xs, y :: ys => (compare x y).then (compareLists xs ys)

def Util.lexicographical [Ord α] : Ord (List α) := ⟨compareLists⟩
