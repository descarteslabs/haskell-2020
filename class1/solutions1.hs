f3 x =
  if x == 1
  then 0
  else 1

fib 1 = 1
fib 2 = 1
fib n = fib (n - 1) + fib (n - 2)

fact 1 = 1
fact n = n * fact (n - 1)

abs' x =
  if x < 0
  then -x
  else x

exp' b 0 = 1
exp' b n = b * exp' b (n-1)

isSqrt s x =
  if abs (s*s - x) < 0.01
  then True
  else False

  
