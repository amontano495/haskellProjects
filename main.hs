isSet :: (Eq a) => [a] -> Bool
isSet [] = True
isSet [x] = True
isSet (x:xs)
	| x `elem` xs = False
	| otherwise = isSet xs

makeSet :: (Eq a) => [a] -> [a]
makeSet [] = []
makeSet (x:xs)
	| x `elem` xs = makeSet xs
	| otherwise = x:(makeSet xs)

subSet :: (Eq a) => [a] -> [a] -> Bool
subset x [] = True
subSet [] x = True
subSet x y
	| x == y = True
	| head x `elem` y = subSet (tail x) y
	| otherwise = False

makeUnion :: (Eq a) => [a] -> [a] -> [a]
makeUnion x [] = x ++ []
makeUnion [] x = x ++ []
makeUnion x y
	| head x `elem` y = makeUnion (tail x) y
	| head y `elem` x = makeUnion (tail y) x
	| otherwise = x ++ y

makeIntersect :: (Eq a) => [a] -> [a] -> [a]
makeIntersect [] y = []
makeIntersect x [] = []
makeIntersect x y
	| head x `elem` y = (head x):(makeIntersect (tail x) y)
	| otherwise = makeIntersect (tail x) y

val :: [a] -> a
val a = head a

left :: [a] -> a
left a = head (tail a)

right :: [a] -> a
right a = head (tail (tail a))

{-
treeMember :: (Eq a) => a -> [a] -> Bool
treeMember a [] = False
treeMember a [b]
	| a == (val [b]) = True
	| treeMember a [(left [b])]
	| otherwise = treeMember a [(right [b])]
-}

findDup :: (Eq a, Num a) => [a] -> a
findDup [] = -1
findDup (x:xs)
	| x `elem` xs = x
	| otherwise = findDup xs

myGCD :: Int -> Int -> Int
myGCD a b
	| b == 0 = a
	| otherwise = myGCD b (a `mod` b)


