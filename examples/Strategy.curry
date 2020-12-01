-- Example showing the influence of strategies to computed results

aBool = False ? True

aBoolList = [] ? aBool:aBoolList
-- DFS: [], [False], [False,False], [False,False,False],...
-- BFS: [], [False], [True], [False,False], [False,True],...

firstTrue (True  : _ ) = True
firstTrue (False : bs) = firstTrue bs

-- DFS does not terminate with a value for this expression:
main = firstTrue aBoolList
-- BFS yields values, try: jucs -x --bfs --first Strategy.curry
