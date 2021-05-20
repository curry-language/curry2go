
tak :: Int -> Int -> Int -> Int
tak x y z = if x <= y
              then z
              else tak (tak (x-1) y z)
                       (tak (y-1) z x)
                       (tak (z-1) x y)

takInt_24_16_8 = tak 24 16 8
takInt_27_16_8 = tak 27 16 8
takInt_33_17_8 = tak 33 17 8

main = takInt_24_16_8
