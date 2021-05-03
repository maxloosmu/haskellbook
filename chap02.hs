
-- Type in REPL
-- let x = 3; y = 1000 in x + 3
-- (^) 10 $ 1 + 1
-- 2^2*4^5+1
-- (-) 37 400
-- 100 / 3

-- Type in REPL
-- z = 7
-- y = z + 8
-- x = y ^ 2
-- waxOn = x * 5
-- triple x = x * 3
-- triple waxOn

waxOn = x * 5 where
  z = 7
  y = z + 8
  x = y ^ 2
triple x = x * 3
waxOff x = triple x


