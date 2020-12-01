-- Example to show why it is neceesary to consider finger prints
-- for choice decisions only if the stack is empty (and not
-- in intermediate hnf computations).

not False = True
not True  = False

False && _ = False
True  && x = x

f x y = x && y ? y

g x = f x (not x)

main = g (False ? True)
-- -> False ? False ? True ? False
