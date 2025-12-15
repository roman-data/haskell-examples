main :: IO ()
main = do
  print (maxI [5,2,3,6,8,4,10] )
  print (takeI 4 [5,2,3,6,8,4,9] )
  print (remove 4 [5,2,3,6,8,4,9] )
  print (removeV2 4 [5,2,3,6,8,4,9] )
  print (len [5,2,3,6,8,4,9] )
  print (lasT [5,2,3,6,8,4] )
  print (nTy 4 [5,2,3,6,8,4] )
  print (nTy 5 [5,2,3,6,8,4] )

getMax :: Int -> Int -> Int
getMax a b | a > b = a | otherwise = b

maxI :: [Int] -> Int
maxI [h] = h
maxI (h:t) 
  | t == [] = h
  | otherwise = getMax h (maxI t) 

takeI :: Int -> [a] -> [a]
takeI _ [] = []
takeI n (h:t) 
      | n == 0 = [h]
      | otherwise = h: takeI (n-1) (t)
  
removeV2 :: Int -> [a] -> [a]
removeV2 _ [] = []
removeV2 n (h:t) 
      | n == 0 = (h:t)
      | otherwise = removeV2 (n-1) t

remove :: Int -> [a] -> [a]
remove _ [] = []
remove n (h:t) = 
      if n == 0 then (h:t)
      else remove (n-1) t

len :: [a] -> Int
len [] = 0
len (h:t) = 1 + len t

nTy :: Int -> [a] -> a
nTy 1 (x:_)  = x
nTy n (_:xs) = nTy (n - 1) xs

lasT :: [a] -> Maybe a
lasT []     = Nothing
lasT (x:[]) = Just x
lasT (_:t)  = lasT t
