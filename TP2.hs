--myHead :: [element] -> element
myHead :: [a] -> a
--myHead :: [Int] -> Int
myHead (x:_) = x

myTail :: [a] -> [a]
myTail (_:xs) = xs

myAppend :: [a] -> [a] -> [a]
myAppend xs ys = myAppend' xs 
    where --myAppend' :: [b] -> [b]
          myAppend' (x:xs) = x:myAppend' xs
          myAppend' [] = ys  

myInit :: [a] -> [a]
myInit [_] = []
myInit (x:xs) = x:(myInit xs)

myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs

-- O(1)
myNull :: [a] -> Bool
myNull [] = True
myNull _ = False

myNull' :: [a] -> Bool  -- O(n)
myNull' xs = length xs == 0

myLength :: [a] -> Int
myLength (_:xs) = 1 + myLength xs
myLength [] = 0

myReverse :: [a] -> [a]
myReverse (x:xs) = myAppend (myReverse xs) [x]
myReverse xs = xs

myConcat :: [[a]] -> [a]
myConcat (xs:xss) = xs ++ myConcat xss
myConcat [] = []

myTake :: Int -> [a] -> [a]
myTake 0 _  = []
myTake n [] = []
myTake n (x:xs) = x:myTake (n-1) xs

myDrop :: Int -> [a] -> [a]
myDrop 0 xs = xs
myDrop n [] = []
myDrop n (x:xs) = myDrop (n-1) xs

myBangBang :: [a] -> Int -> a
myBangBang (x:xs) 0 = x
myBangBang (x:xs) n = myBangBang xs (n-1)

myInsert :: Ord a => a -> [a] -> [a]
myInsert x [] = [x]
myInsert x (y:ys) | x>y       = y:myInsert x ys
                  | otherwise = x:y:ys

mySort :: Ord b => [b] -> [b]
mySort (x:xs) = myInsert x (mySort xs)
mySort [] = []

myNull'' :: Eq a => [a] -> Bool
myNull'' xs = xs==[]

-- NEW STUFF

-- ordre superieur

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile f (x:xs) | f x = x:myTakeWhile f xs
                     | otherwise = []
myTakeWhile f [] = []

-- donner le type de la fonction, notation infixe versus prefixe
myCompose ::  (b -> c) -> (a -> b) -> a -> c
myCompose f g x = f (g x)

myMap :: (a -> b) -> [a] -> [b]
myMap f (x:xs) = f x:myMap f xs
myMap f []     = []

test1 = myMap odd [1..10]

-- calcul des sous liste en utilisant map

sousListes :: [a] -> [[a]]
sousListes (x:xs) = map (x:) (sousListes xs) ++ sousListes xs
sousListes [] = [[]]

-- 1) ne pas tenter de derouler une fonction ecrite

-- 2) ne pas tenter de derouler une fonction pour l'ecrire

-- 3) faire confiance (hypotheses), oublier qui on est et deleguer

-- une fonction plus generale: foldr
-- inferer le type de foldr
-- forme graphique de la liste en peigne
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f k (x:xs) = f x (myFoldr f k xs)
myFoldr f k []     = k

myAnd' :: [Bool] -> Bool
myAnd' bs = foldr (&&) True bs 

-- definir reverse avec foldr
myReverse' :: [a] -> [a]
myReverse' xs = undefined

-- une parenthese sur les lambda anonymes

add' :: Int -> Int -> Int
add' x y = x + y

add'' :: Int -> Int -> Int
add'' = \x -> \y -> x + y

-- avec foldr
myReverse'' :: [a] -> [a]
myReverse'' = undefined

-- eta reduction
myReverse''' :: [a] -> [a]
myReverse''' = undefined

-- un "nouveau type" String

prenom :: String
prenom = "jean francois"


-- un nouveau type tuples

myFst :: (a,b) -> a
myFst (x,y) = x

-- TODO: definir recursivement

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile f []                 = []
myDropWhile f (x:xs) | f x       = myDropWhile f xs
                     | otherwise = x:xs

myElem :: Eq a => a -> [a] -> Bool
myElem y [] = False
myElem y (x:xs) = y==x || myElem y xs

myNotElem :: Eq a => a -> [a] -> Bool
myNotElem x xs= not (myElem x xs)

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f [] = []
myFilter f (x:xs) | f x =  x:(myFilter f xs)
                  | otherwise = myFilter f xs

mySplitAt :: Int -> [a] -> ([a],[a])
mySplitAt 0 xs = ([], xs)
mySplitAt n [] = ([], [])
mySplitAt i (x:xs) =  (x:first, second)
    where (first, second) = mySplitAt (i-1) xs


myZip :: [a] -> [b] -> [(a,b)] 
myZip as [] = []
myZip [] bs = []
myZip (a:as) (b:bs) = (a,b):myZip as bs

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c] 
myZipWith f as [] = []
myZipWith f [] bs = []
myZipWith f (a:as) (b:bs) = (f a b):myZipWith f as bs

myCurry :: ((a,b) -> c) -> a -> b -> c
myCurry f x y =  f (x,y)

myUncurry :: (a -> b -> c) -> (a,b) -> c
myUncurry f (a,b) = f a b

myZipWith' :: (a -> b -> c) -> [a] -> [b] -> [c] 
myZipWith' = undefined

myUnzip :: [(a,b)] -> ([a],[b])
myUnzip [] = ([],[])
myUnzip ((a,b):xs) = (a:first, b:second)
    where (first, second) =myUnzip(xs)

-- TODO: redefinir en utilisant foldr

myConcat' :: [[a]] -> [a]
myConcat' xs = foldr (++) [] xs 

myMap' ::  (a -> b) -> [a] -> [b]
myMap' f xs = foldr (\x -> \fxs ->(f x):fxs) [] xs 

myOr' ::  [Bool] -> Bool
myOr' xs = foldr (||) False xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = foldr (\x -> \fxs -> (f x) || fxs) False xs

myAll :: (a -> Bool) -> [a] -> Bool
myAll f xs= foldr (\x -> \fxs -> (f x) && fxs) True xs

myProduct :: [Int] -> Int
myProduct xs = foldr (\x -> \fxs -> x * fxs) 1 xs

-- TODO: calculuer les 50 plus petits nombres premiers 2, 3, 5, 7, 11...

premiers :: [Int]
premiers = undefined

test2 = take 50 premiers

