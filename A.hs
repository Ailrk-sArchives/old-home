module A where



x = 0

data A = A Int Int Int

foo (A 1 x 2) = x
foo (A 1 y 1) = y
