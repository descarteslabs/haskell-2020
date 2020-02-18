
factors n = filter dividesN [2..(n `div` 2)]
  where
    dividesN x = n `mod` x == 0

primeFactors n =
  if fn == []
  then [n]
  else hfn:(primeFactors $ n `div` hfn)
  where
    fn = factors n
    hfn = head fn

-- fib1 is a speedy fibonacci number generator.
findFib n fs =
    let ms = filter (\p -> fst p == n) fs
    in if 0 == length ms
       then Nothing
       else Just $ snd $ head ms

ensure n fs =
    case (findFib n fs) of
      Just p -> fs
      Nothing -> addFib n fs

addFib n fs =
    let m1 = n `div` 2
        m2 = n - m1
        fs' = ensure m1 $ ensure (m2-1) $ ensure (m1+1) $ ensure m2 fs
        Just fm1   = findFib m1     fs'
        Just fm1m1 = findFib (m1-1) fs'
        Just fm2   = findFib m2     fs'
        Just fm2p1 = findFib (m2+1) fs'
    in if m1 == m2
       then (n, fm1*(fm1m1+fm2p1)) : fs'
       else (n, fm1*fm1 + fm2*fm2) : fs'

fib1 n = f
    where Just f = findFib n $ addFib n [(1,1),(2,1)]

