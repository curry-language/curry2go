-- Testing user-defined data types.

data BW = Black | White

invBW Black = White
invBW White = Black

aBW = White
aBW = Black

invSomeBW = invBW aBW

main = invSomeBW
