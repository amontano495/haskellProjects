import Data.List (permutations)
import Data.Char

data Tree = Leaf | Node Int Tree Tree deriving Show

---Tree Functions---

getVal :: Tree -> Int
getVal (Node x _ _) = x

insert :: Tree -> Int -> Tree
insert Leaf a = (Node a Leaf Leaf)
insert (Node x l r) a
    | a < x = (Node x (insert l a) r)
    | a > x = (Node x l (insert r a))
    | a == x = (Node x l r)

buildTree :: Tree -> [Int] -> Tree
buildTree t [] = t
buildTree t (x:xs) = buildTree (insert t x) xs 

preOrder :: Tree -> [Int]
preOrder Leaf = []
preOrder (Node x l r)  = [x] ++ preOrder l ++ preOrder r

validTrees :: [Int] -> [Tree]
validTrees arr = (map (buildTree Leaf) (permutations arr))

validTreePreOrder :: [Int] -> Bool
validTreePreOrder arr = (arr == (preOrder (buildTree Leaf arr)))

--Format/IO Functions---


--Credit to Stack Exchange user: snak
second :: [a] -> [a]
second xs = map fst $ filter (even . snd) $ zip xs [1..]

--Credit to Stack Exchange user: Martin Geisler
formatter :: String -> [Int]
formatter str = map read $ words str :: [Int]

yesOrNo :: Bool -> String
yesOrNo True = "YES"
yesOrNo False = "NO" 


main = do
    totalLists <- readLn :: IO Int
    inputData <- getContents
    let
	treeContents = map formatter (second (lines inputData))
	solutions = map validTreePreOrder treeContents
	answer = map yesOrNo solutions	
	
    mapM_ putStrLn answer
