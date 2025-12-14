main :: IO ()
main = do 
  print (pyr (3) )
  print (fib2 (4) )
  print (fab (4) )
  print (fac (5) )
  print (faktorial (5) )

fib2 :: Int -> Int
fib2 n = fib (0, 1, 1, n)

fib :: (Int, Int, Int, Int) -> Int
fib (i, f1, f2, n)
  | i == n    = f1
  | otherwise = fib (i + 1, f2, f1 + f2, n)
  

fab :: Int -> Int
fab 0 = 1
fab 1 = 1
fab n = fab(n-1)+fab(n-2)

fac :: Int -> Maybe Int
fac n
  | n < 0     = Nothing
  | n == 0    = Just 1
  | otherwise = fmap (n *) (fac (n - 1))


faktorial n | n < 0 = error "neexistuje"
            | n == 0 = 1
            | otherwise = n * faktorial (n - 1)

pyr :: Int -> Int
pyr 1 = 1
pyr n = n * n + pyr( n - 1 )
