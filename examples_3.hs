main :: IO ()
main = do
  print (removeEvenI [10,-2,5,-2,3,6,-8,-4,-10] )
  print (minI [10,-2,5,-2,3,6,-8,-4,-10] )
  print (removeMinus [10,-2,5,-2,3,6,-8,-4,-10] )
  print (flipIv2 [10,2,5,2,3,6,8,4,10] )
  print (flipI [2,5,2,3,6,8,4,10] )
  print (mergeI [1,0] [2,5,2,3,6,8,4,10] )
  print (removeI 2 [2,5,2,3,6,8,4,10] )

removeEvenI :: [Int] -> [Int]
removeEvenI [] = []
removeEvenI (h:t) 
  | h `mod` 2 == 0 = h:removeEvenI t
  | otherwise = removeEvenI t

minI :: [Int] -> Int 
minI [h] = h
minI (h:t) = getMin (minI t) h

getMin :: Int -> Int -> Int  
getMin x y | x < y = x | otherwise = y

removeMinus :: [Int] -> [Int] 
removeMinus [] = []
removeMinus (h:t)
  | h < 0 = removeMinus t
  | otherwise = h:removeMinus t


flipIv2 :: [Int] -> [Int] 
flipIv2 x = flipI_v2 x []

flipI_v2 :: [Int] -> [Int] -> [Int] 
flipI_v2 [] r = r
flipI_v2 (h:t) r = flipI_v2 t (h:r)


flipI :: [Int] -> [Int] 
flipI [r] = [r]
flipI (h:t) = mergeI (flipI (t) ) [h]

mergeI :: [Int] -> [Int] -> [Int] 
mergeI [] r = r
mergeI (h:t) x = h:mergeI (t) x

removeI :: Int -> [Int] -> [Int]
removeI _ [] = []
removeI n (h:t) 
  | n == h = removeI n t
  | otherwise = h:removeI n t
