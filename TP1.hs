
import Data.List

{-
-- :r


-- commentaires, multilignes
-}

-- constante entiere, identifiant, declaration typee, definition

un :: Int
un = 1

deux :: Int
deux = un + un

-- fonction, declaration typee, definition

mySub :: Int -> Int -> Int
mySub x y = x-y

neg :: Int -> Int
--neg x = mySub 0 x
neg = mySub 0 -- application partielle

-- liste d'entier, nil, cons, liste en comprehension

l0 :: [Int]
l0 = []

l1 :: [Int]
l1 = 1:l0

l2 :: [Int]
l2 = 2:l1
--l2 = 2 mal type

l3 :: [Int]
l3 = [1,2,3]

l4 :: [Int]
l4 = [1..10]

l5 :: [Int]
l5 = [1,3..10]

myHead :: [Int] -> Int
myHead (x:xs) = x -- pattern matching

l6 :: [Int]
l6 = [10,8..0]

myTail :: [Int] -> [Int]
myTail (x:xs) = xs

-- booleen (fonctions booleennes : && || not)

myNull12 :: [Int] -> Bool
myNull12 (x:xs) = False
myNull12 xs     = True
--myNull12 []     = True


-- fonction recursive

-- la fonction append s'ecrit ++ en haskell
-- la fonction ++ est infix puisqu'elle est constituee de symboles (pas des lettres)

myAppend :: [Int] -> [Int] -> [Int]
myAppend (x:xs) ys = x:myAppend xs ys
myAppend []     ys = ys
--myAppend [x]    ys = x:ys

myAppend' :: [Int] -> [Int] -> [Int]
myAppend' xs ys | not (null xs) = head xs : myAppend' (tail xs) ys
                | otherwise     = ys

myAppend'' :: [Int] -> [Int] -> [Int]
myAppend'' xs ys | null xs       = ys
                 | not (null xs) = head xs : myAppend'' (tail xs) ys

myAppend4 :: [Int] -> [Int] -> [Int]
myAppend4 (x:xs) ys = 
    let suite = myAppend4 xs ys 
    in x:suite
myAppend4 []     ys = ys

myAppend5 :: [Int] -> [Int] -> [Int]
myAppend5 (x:xs) ys = x:suite where suite = myAppend5 xs ys
myAppend5 []     ys = ys

myAppend6 :: [Int] -> [Int] -> [Int]
myAppend6 xs ys = myAppend6' xs 
    where myAppend6' (x:xs) = x:myAppend6' xs
          myAppend6' []     = ys

-- a vous...
myInit :: [Int] -> [Int]
myInit (xs:[]) = []
myInit (x1:xs) = x1:myInit(xs)

myLast :: [Int] -> Int
myLast (x:[]) = x
myLast (x:xs) = myLast xs

myNull :: [Int] -> Bool
myNull [] = True
myNull x = False

myNull' :: [Int] -> Bool
myNull' = null

myLength :: [Int] -> Int
myLength (x:xs) = 1 + myLength xs
myLength [] = 0

myReverse :: [Int] -> [Int]
myReverse (x:xs) = myReverse xs ++[x]
myReverse [] = []

-- iteratif, comparer les complexites experimentalement
myReverse' :: [Int] -> [Int]
myReverse' xs = myReverse'' xs []
  where myReverse'' (x:xs) ys = myReverse'' xs (x:ys)
        myReverse'' []  ys = ys

myConcat :: [[Int]] -> [Int]
myConcat (l:ls) = l++myConcat(ls)
myConcat [] = []

myAnd :: [Bool] -> Bool
myAnd (x:xs)= x && myAnd(xs)
myAnd [] = True

myOr ::  [Bool] -> Bool
myOr (x:xs)= x || myOr(xs)
myOr [] = False

myProduct :: [Int] -> Int
myProduct (x:xs)= x * myProduct(xs)
myProduct [] = 1

-- pas d'element neutre pour max et min 

myTake :: Int -> [Int] -> [Int]
myTake 0 ls = []
myTake x [] = []
myTake x (l:ls) = l:(myTake (x-1) ls)


myDrop :: Int -> [Int] -> [Int]
myDrop 0 ls = ls
myDrop x [] = []
myDrop x (l:ls) = myDrop (x-1) ls

-- cette fonction existe sous le nom !!
myBangBang :: [Int] -> Int -> Int
myBangBang (x:xs) 0 = x
myBangBang (x:xs) y = myBangBang xs (y-1)

myInsert :: Int -> [Int] -> [Int]
myInsert x (l:ls) | x <= l = x:(l:ls)
                  | otherwise = l:myInsert x ls
myInsert x [] = [x]

mySort :: [Int] -> [Int]
mySort [] = []
mySort x = mySort'' x []
  where mySort'' (x:xs) ys = mySort'' xs (myInsert x ys)
        mySort'' []  ys = ys