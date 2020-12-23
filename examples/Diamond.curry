-- Curry's solution to the "diamond" problem of the
-- Prolog programming contest at JICSLP'98 in Manchester

main :: IO ()
main = diamond 10

diamond :: Int -> IO ()
diamond n = lineloop1 1 1 >> lineloop2 1 (n*n-n+2)  where

 lineloop1 i j = if i<=n then line j i >> lineloop1 (i+1) (j+n)
                         else return ()

 lineloop2 i j = if i<n then line j (n-i) >> lineloop2 (i+1) (j+1)
                        else return ()

 line s e = tab ((n-e) * (size(n*n) + 1)) >> lineloop 1 s
   where
      lineloop i t =
             if i<=e
             then putValue t >> tab (size(n*n)+1) >> lineloop (i+1) (t-n+1)
             else putChar '\n'

      putValue v = tab((size(n*n)+1)-size(v)) >> putStr (show v)


tab :: Int -> IO ()
tab n = if n==0 then return () else putChar ' ' >> tab (n-1)

-- number of characters for the string representation of a number:
size :: Int -> Int
size n = if n<10 then 1 else size (n `div` 10) + 1
