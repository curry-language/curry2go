-- Testing user-defined data types.

data BW = Black | White

invBW :: BW -> BW
invBW Black = White
invBW White = Black

aBW :: BW
aBW = White
aBW = Black

invSomeBW :: BW
invSomeBW = invBW aBW

main = invSomeBW
