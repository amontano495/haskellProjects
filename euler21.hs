divs :: Int -> Int -> [Int]
divs x 0 = []
divs x y
	| (x `mod` y) == 0 = y:divs x (y - 1)
	| otherwise = divs x (y - 1)

divPairs :: Int -> Int -> Bool
divPairs x y
	| (sum (divs x (x-1))) == y = True
	| otherwise = False

genPairs :: Int -> [(Int, Int)]
genPairs x = [(a, b)| a <-[1..x], b <-[1..a], divPairs a b == True]

sumDeux :: (Int, Int) -> Int
sumDeux (x,y) = x+y

detupper :: [(Int, Int)] -> [Int]
detupper ((x,y):xs) = x : y : detupper xs
detupper [] = []

remDup :: (Eq a, Num a) => [a] -> [a]
remDup [] = []
remDup (x:xs)
	| x `elem` xs = remDup xs
	| otherwise = x:(remDup xs)

main = print (sum (remDup (detupper (genPairs 10000))))
