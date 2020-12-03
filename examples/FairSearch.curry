-- Examples for showing operational completeness of a strategy.

data Nat = O | S Nat

-- A non-deterministically looping identity on natural numbers:
f :: Nat -> Nat
f n = loop n ? n ? loop n
 where
  loop n = loop (S n)

main = f O
-- DFS: no result
-- BFS: one result, try: curry2go -r -i --bfs FairSearch
-- FS : one result, try: curry2go -r -i --fs FairSearch
