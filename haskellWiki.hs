cons8 :: [Int] -> [Int]
cons8 [] = 8:[]
cons8 (x:xs) = 8:(x:xs) 

cons8_end :: [Int] -> [Int]
cons8_end [] = 8:[]
cons8_end (x:xs) = (x:xs) ++ 8:[]

myCons :: [a] -> a -> [a]
myCons (x:xs) q = q:(x:xs)

listToTup :: [a] -> (a,[a])
listToTup (x:xs) = (x,xs)

grab5th :: [a] -> a
grab5th (x:xs) = head ( tail ( tail ( tail ( xs ) ) ) )
