import System.CPUTime         (getCPUTime)

external_d_C_getCPUTime :: Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Int
external_d_C_getCPUTime _ _ = toCurry (getCPUTime >>= return . (`div` (10 ^ 9)))

external_d_C_getElapsedTime :: Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Int
external_d_C_getElapsedTime _ _ = toCurry (return 0 :: IO Int)
