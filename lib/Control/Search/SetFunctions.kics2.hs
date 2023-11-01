external_d_C_incDepth :: (a -> Cover -> ConstStore -> b) ->  a -> Cover -> ConstStore -> b
external_d_C_incDepth f v cd c = f v (incCover cd) c

external_nd_C_incDepth :: (Func a b) -> a -> IDSupply -> Cover -> ConstStore -> b
external_nd_C_incDepth (Func f) x s cd c = f x s (incCover cd) c
external_nd_C_incDepth _ _ _ _ _ = error
  "External_SetFunctions.external_nd_C_incDepth: \
                \functional argument no ground term"
