main :: IO ()
main = do
  print (mensiA 0 [10,-2,5,-2,3,6,-8,-4,-10,0] )
  print (vetsiA 0 [10,-2,5,-2,3,6,-8,-4,-10,0] )
  print (stejnaA 0 [10,-2,5,-2,3,6,-8,-4,-10,0] )
  print (srovnej 6 [10,-2,5,-2,3,6,-8,-4,-10,0] )
  print (qsortLog [10,-2,5,-2,3,6,-8,-4,-10,0] )
  print (nejvetsi [10,-2,5,-2,3,6,-8,-4,-10,0] )
  print (qsort   [10,-2,5,-2,3,6,-8,-4,-10,0] )
  print (maxSort [10,-2,5,-2,3,6,-8,-4,-10,0] )
  print (odstran 10 [10,-2,5,-2,3,6,-8,-4,-10,0] )
  
maxSort [] = []
maxSort h = [nejmensi h] ++ maxSort ( odstran (nejmensi h ) h)

nejvetsi :: [Int] -> Int
nejvetsi (h:t) = nejvetsi2 h (h:t)

nejvetsi2 :: Int -> [Int] -> Int
nejvetsi2 n [] = n
nejvetsi2 n (h:t)
  | h > n     = nejvetsi2 h t
  | otherwise = nejvetsi2 n t

nejmensi :: [Int] -> Int
nejmensi (h:t) = nejmensi2 h (h:t)

nejmensi2 :: Int -> [Int] -> Int
nejmensi2 n [] = n
nejmensi2 n (h:t)
  | h < n     = nejmensi2 h t
  | otherwise = nejmensi2 n t

odstran :: Int -> [Int] -> [Int]
odstran _ [] = []
odstran n (h:t)
  | h == n     = odstran n t
  | otherwise = h:odstran n t
  

qsortLog :: [Int] -> ([Int], [String])
qsortLog [] = ([], [])
qsortLog (p:xs) =
  let
    mensi  = mensiA p xs
    stejna = stejnaA p (p:xs)
    vetsi  = vetsiA p xs

    (s1, l1) = qsortLog mensi
    (s2, l2) = qsortLog vetsi

    logBlock = unlines
      [ "pivot: " ++ show p
      , "  <  " ++ show mensi
      , "  =  " ++ show stejna
      , "  >  " ++ show vetsi
      ]
  in
    ( s1 ++ stejna ++ s2
    , logBlock : (l1 ++ l2)
    )


qsort :: [Int] -> [Int]
qsort [] = []
qsort (p:xs) =
  qsort (mensiA p xs)
  ++ stejnaA p (p:xs)
  ++ qsort (vetsiA p xs)


srovnej :: Int -> [Int] -> [Int] 
srovnej n t =  spoj (spoj (stejnaA n t) (mensiA n t) )  (vetsiA n t)

spoj :: [Int] -> [Int] -> [Int] 
spoj [] t = t
spoj (a:b) t = spoj b (a : t)

vratVetsi :: Int -> Int -> Int
vratVetsi x y | x < y = y | otherwise = x

mensiA :: Int -> [Int] -> [Int]
mensiA _ [] = []
mensiA n (h:t)
  | h < n = h:mensiA n t
  | otherwise = mensiA n t

vetsiA :: Int -> [Int] -> [Int]
vetsiA _ [] = []
vetsiA n (h:t)
  | h > n = h:vetsiA n t
  | otherwise = vetsiA n t

stejnaA :: Int -> [Int] -> [Int]
stejnaA _ [] = []
stejnaA n (h:t)
  | h == n = h:stejnaA n t
  | otherwise = stejnaA n t