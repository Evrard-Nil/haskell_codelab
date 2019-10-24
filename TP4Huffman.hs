-- NOM : Daillet
-- PRENOM : Evrard-Nil

-- respectez les formes demandees : 

-- [rec def]: une definition recursive, une fonction qui s'appelle elle meme 
-- [non rec def]: une definition non recursive, une fonction qui ne s'appelle pas elle meme 
-- [ZF def]: une definition avec une liste en comprehension

-- fonctions auxiliaires autorisees : (==), (/=), (+), (>), (:), (.), (++), filter, foldr, length, map

-- compression de huffman

exemple1 :: String
exemple1 = "abcbcaa"

-- arbre de Huffman

data Huff a = Leaf a | Node (Huff a) (Huff a) deriving (Show,Eq)

-- chemin dans un arbre

type Bits = [Bit]

data Bit = L | R deriving (Show,Eq)

-- construction de l'arbre de Huffman

-- Q1 [rec def]
-- calcule pour chaque caractere d'une chaine son nombre d'occurences

testQ1 :: Bool
testQ1 = nbOccurrences exemple1 == [('a',3),('b',2),('c',2)]

nbOccurrences :: String -> [(Char,Int)]
nbOccurrences (c:cs) = (c, (1+length (filter (==c) cs))):nbOccurrences (filter (/=c) cs)
nbOccurrences []     = []

{-- 
-- nbOccurrences xs = foldr (\c -> \fcs -> count c fcs) [] xs
--     where count c []                      = [(c,1)]
--           count c ((y, i):ys) | c==y      = (y, i+1):ys
--                               | otherwise = (y,i):(count c ys)


-- nbOccurrences xs =  nbOccurrences' xs []
--     where nbOccurrences'   []       ys                 = ys
--           nbOccurrences' (x:xs)     []                 = nbOccurrences' xs [(x, 1)]
--           nbOccurrences' (x:xs) ((c,y):ys) | x == c    = nbOccurrences' xs ((c, y+1):ys)
--                                            | otherwise = nbOccurrences' xs ((c,y):nbOccurrences' [x] ys)
--}

-- Q2 [rec def]
-- le poids est la somme des occurences contenues dans ses feuilles

testQ2 :: Bool
testQ2 = poids (Node (Leaf ('a',3)) (Node (Leaf ('b',2)) (Leaf ('c',2)))) == 7

poids :: Huff (a,Int) -> Int
poids (Leaf (c, i)) = i
poids (Node a b) = poids a + poids b
        
-- Q3 [rec def]
-- insere un arbre a sa place dans une liste d'arbres tries par poids croissants

testQ3 :: Bool
testQ3 = insere (Leaf ('a',3)) [Leaf ('b',2),Leaf ('c',2)] == [Leaf ('b',2),Leaf ('c',2),Leaf ('a',3)]

insere :: Huff (a,Int) -> [Huff (a,Int)] -> [Huff (a,Int)] 
insere l []                          = [l]
insere l (x:xs) | poids l > poids x  = x:insere l xs
                | otherwise          = l:x:xs
         
-- Q4 [non rec def avec foldr]
-- tri des arbres de Huffman par poids croissants

testQ4 :: Bool
testQ4 = triHuff [Leaf ('a',3),Leaf ('b',2),Leaf ('c',2)] == [Leaf ('b',2),Leaf ('c',2),Leaf ('a',3)]

triHuff :: [Huff (a,Int)] -> [Huff (a,Int)]
-- triHuff = foldr (\h -> \fhs -> insere h fhs) []
triHuff = foldr insere []
          
-- Q5 [non rec def]
-- construit la liste des feuilles pour une chaine donnee

testQ5 :: Bool
testQ5 = feuilles exemple1 == [Leaf ('b',2),Leaf ('c',2),Leaf ('a',3)]

feuilles :: String -> [Huff (Char,Int)]
feuilles s = triHuff (map Leaf (nbOccurrences s))
            
-- Q6 [rec def]
-- agreege les deux arbres les plus legers, repetitivement pour obtenir un arbre unique

testQ6 :: Bool
testQ6 = agrege [Leaf ('b',2),Leaf ('c',2),Leaf ('a',3)] == Node (Leaf ('a',3)) (Node (Leaf ('b',2)) (Leaf ('c',2)))

agrege :: [Huff (a,Int)] -> Huff (a,Int)
agrege (x1:x2:xs) = agrege (insere (Node x1 x2) xs)
agrege (x1:[]) = x1
         
-- Q7 [rec def]
-- efface les nombre d'occurences dans un arbre de Huffman

testQ7 :: Bool
testQ7 = strip (Node (Leaf ('a',3)) (Node (Leaf ('b',2)) (Leaf ('c',2)))) == Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))

strip :: Huff (a,Int) -> Huff a
strip (Leaf (a, i)) = Leaf a
strip (Node a b) = Node (strip a ) (strip b)
        
-- construit l'arbre de Huffman pour une chaine donnee

test' :: Bool
test' = buildHuff exemple1 == Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))

buildHuff :: String -> Huff Char 
buildHuff = strip . agrege . feuilles

-- codage d'une chaine

-- code un caractere 

test'' :: Bool
test'' = codeOne (Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))) 'b' == [R,L]

codeOne :: Huff Char -> Char -> Bits
codeOne root c = head (codeOne' root c)

-- Q8 [def recursive qui utilise aussi map]
-- code un caractere, retourne une liste de longueur 1 qui contient le chemin du caractere
-- commencer par ecrire la fonction qui retourne tous les chemins puis la modifier

testQ8 :: Bool
testQ8  = codeOne' (Node (Leaf 'a')  (Node (Leaf 'b') (Leaf 'c'))) 'b' == [[R, L]]
testQ81 = codeOne' (Node (Node (Leaf 'a') (Leaf 'd')) (Node (Leaf 'b') (Leaf 'c'))) 'a' == [[L, L]]
testQ82 = codeOne' (Node (Node (Leaf 'a') (Leaf 'd')) (Node (Leaf 'b') (Leaf 'c'))) 'd' == [[L, R]]
testQ83 = codeOne' (Node (Node (Leaf 'a') (Leaf 'd')) (Node (Leaf 'b') (Leaf 'c'))) 'c'  == [[R, R]]
testQ84 = codeOne' (Node (Node (Leaf 'a') (Leaf 'd')) (Node (Leaf 'b') (Leaf 'c'))) 'b'  == [[R, L]]
testQ85 = codeOne' (Leaf 'b') 'b' == [[]]

codeOne' :: Huff Char -> Char -> [Bits]
codeOne' (Leaf a) c | c == a    = [[]]
                    | otherwise = []

codeOne' (Node a b) c                    = map ([L]++) (codeOne' a c) ++ map ([R]++) (codeOne' b c)

-- code tous les caracteres d'une chaine

test''' :: Bool
test''' = codeAll (Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))) exemple1 == [L,R,L,R,R,R,L,R,R,L,L]

codeAll :: Huff Char -> String -> Bits
codeAll root s = concat (map (codeOne root) s)


-- Q9 [rec def]
-- decodage d'une liste de bits

testQ9 :: Bool
testQ9 = decode (Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))) (Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))) [L,R,L,R,R,R,L,R,R,L,L] == "abcbcaa"

decode :: Huff Char -> Huff Char -> Bits -> String
decode root (Node a b) (s:ss) | s == L = decode root a ss
                              | s == R = decode root b ss
decode root (Leaf a) ss = a:decode root root ss
decode _ _ [] = []

-- verifie la correction en codange puis decodant et calcule le ratio de compression (hors arbre)

test'''' :: Bool
test'''' = test10 exemple1 == (True,0.19642857)

test10 :: String -> (Bool,Float)
test10 s = 
    let t = buildHuff s
        c = codeAll t s
        s' = decode t t c
    in (s==s',(fromIntegral (length c))/(fromIntegral (8*length s)))

-- Q10 
-- en 10 mots maximum identifier ce qui est recalcule de nombreuses fois et pourrait etre optimise dans ce code

-- reponse : On 
