import System.IO.Unsafe (unsafePerformIO)

import KiCS2Debug (internalError)

external_d_C_unsafePerformIO :: Curry_Prelude.C_IO a -> Cover -> ConstStore -> a
external_d_C_unsafePerformIO io cd cs = unsafePerformIO (toIO errSupply cd cs io)
  where errSupply = internalError "Unsafe.unsafePerformIO: ID supply used"

external_nd_C_unsafePerformIO :: Curry_Prelude.C_IO a -> IDSupply -> Cover -> ConstStore -> a
external_nd_C_unsafePerformIO io s cd cs = unsafePerformIO (toIO s cd cs io)
