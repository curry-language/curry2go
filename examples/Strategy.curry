-- Example showing the influence of strategies to computed results

aBool :: Bool
aBool = False ? True

aBoolList :: [Bool]
aBoolList = [] ? aBool:aBoolList
-- DFS: [], [False], [False,False], [False,False,False],...
-- BFS: [], [False], [True], [False,False], [False,True],...

-- alternative definition with base >= 3.0.0
--aBoolList = aValue :: [Bool]

firstTrue (True  : _ ) = True
firstTrue (False : bs) = firstTrue bs

-- DFS does not terminate with a value for this expression:
main = firstTrue aBoolList
-- BFS yields values, try: curry2goc -r --bfs --first Strategy.curry
