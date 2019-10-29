
-- NOM :  Daillet
-- PRENOM : Evrard-Nil

-- autorisees : (==), (<=), (++), concat, foldr, fst, map, reverse, snd
-- des fonctions anonymes, et les constructeurs de listes et de paires

-- librement inspire du TP4 de structure algorithmique : a l’arrivee
-- aux urgences un premier diagnostique est realise afin de determiner
-- un niveau de priorite. Les patients dont la priorite est grande
-- sont traites avant ceux de priorite moindre. A niveau de priorite
-- egal, les patients arrives en premiers sont prioritaires.

-- respectez les formes demandees et ecrivez du code simple et beau !
-- si vous ecrivez quelque chose de complique, c'est probablement faux

-- une queue comme une liste par ordre d'arrivee
-- ceux qui font le queue depuis le plus longtemps sont prioritaires

type Queue a = [a]

newQueue :: Queue a
newQueue = []

isEmpty :: Queue a -> Bool
isEmpty = null

enqueue :: a -> Queue a -> Queue a
enqueue a q = q++[a]

dequeue :: Queue a -> (a,Queue a)
dequeue (a:q) = (a,q)

-- une queue de priorites comme une liste de paires
-- les plus severes sont prioritaires,
-- en cas de severites egales, ceux qui font la queue depuis plus longtemps sont prioritaires

type Severity = Int

type QueuePrio a = [(Severity,a)]

newQueuePrio :: QueuePrio a
newQueuePrio = []

isEmptyPrio :: QueuePrio a -> Bool
isEmptyPrio = null

-- Q1 : definir recursivement 

testQ1 = enqueuePrio (4,"Daniel") [(4,"Bernard"),(3,"Albert"),(3,"Cedric")]
  == [(4,"Bernard"),(4,"Daniel"),(3,"Albert"),(3,"Cedric")]

enqueuePrio :: (Severity,a) -> QueuePrio a -> QueuePrio a
enqueuePrio a [] = [a]
enqueuePrio (s1, a1) ((s2, a2):qs) | s1 <= s2 = (s2, a2):(enqueuePrio (s1, a1) qs)
                                   | otherwise = (s1, a1):((s2, a2):qs)

-- Q2 : definir non recursivement 

testQ2 = dequeuePrio [(4,"Bernard"),(4,"Daniel"),(3,"Albert"),(3,"Cedric")]
  == ((4,"Bernard"),[(4,"Daniel"),(3,"Albert"),(3,"Cedric")])

dequeuePrio :: QueuePrio a -> ((Severity,a),QueuePrio a)
dequeuePrio = dequeue

-- une queue de priorites comme une liste de listes chaque sous liste
-- represente une meme severite les priorites sont
-- decroissantes. Comme precedemment : les plus severes sont
-- prioritaires, en cas de severites egales, ceux qui font la queue
-- depuis plus longtemps sont prioritaires

type QueuePrio2 a = [(Severity,[a])]

newQueuePrio2 :: QueuePrio2 a
newQueuePrio2 = []

isEmptyPrio2 :: QueuePrio2 a -> Bool
isEmptyPrio2 = null

-- Q3 : definir recursivement

testQ3 = enqueuePrio2 (4,"Daniel") [(4,["Bernard"]),(3,["Albert","Cedric"])]
  == [(4,["Bernard","Daniel"]),(3,["Albert","Cedric"])]

enqueuePrio2 :: (Severity,a) -> QueuePrio2 a -> QueuePrio2 a
enqueuePrio2 (s1, a1) []                        = [(s1, [a1])]
enqueuePrio2 (s1, a1) ((s2, a2):qs) | s1 == s2  = ((s2, a2++[a1]):qs)
                                    | otherwise = (s2, a2):(enqueuePrio2 (s1, a1) qs)

-- Q4 : definir non recursivement

testQ4 = dequeuePrio2 [(4,["Bernard","Daniel"]),(3,["Albert","Cedric"])]
  == ((4,"Bernard"),[(4,["Daniel"]),(3,["Albert","Cedric"])])
  
dequeuePrio2 :: QueuePrio2 a -> ((Severity,a),QueuePrio2 a)
dequeuePrio2 ((s, [p]):qs)    = ((s,p),qs)
dequeuePrio2 ((s, (p:ps)):qs) = ((s,p),((s,ps):qs))

-- Q5 : definir non recursivement avec une liste en comprehension

testQ5 = convertPrio2ToPrio [(4,["Bernard","Daniel"]),(3,["Albert","Cedric"])]
  == [(4,"Bernard"),(4,"Daniel"),(3,"Albert"),(3,"Cedric")]

convertPrio2ToPrio :: QueuePrio2 a -> [(Severity,a)]
convertPrio2ToPrio a =  [(s, q) | (s, qs) <- a , q<- qs]

-- Q6 non recursivement

testQ6 = convertPrio2ToPrio' [(4,["Bernard","Daniel"]),(3,["Albert","Cedric"])]
  == [(4,"Bernard"),(4,"Daniel"),(3,"Albert"),(3,"Cedric")]

convertPrio2ToPrio' :: QueuePrio2 a -> QueuePrio a
convertPrio2ToPrio' a = foldr (\(s, xs) -> \qs -> (map (\x -> (s,x)) xs) ++ qs) [] a

-- Q7 : definir non recursivement

testQ7 = convertPrioToPrio2 [(4,"Bernard"),(4,"Daniel"),(3,"Albert"),(3,"Cedric")]
  == [(4,["Bernard","Daniel"]),(3,["Albert","Cedric"])]
  
convertPrioToPrio2 :: QueuePrio a -> QueuePrio2 a
convertPrioToPrio2 q = foldr (\x -> \xs -> enqueuePrio2 x xs) [] (reverse q)

-- Q8 : definir recursivement sans pattern matching

testQ8 = convertPrioToPrio2Iter [(4,"Bernard"),(4,"Daniel"),(3,"Albert"),(3,"Cedric")]
  == [(4,["Bernard","Daniel"]),(3,["Albert","Cedric"])]

convertPrioToPrio2Iter :: QueuePrio a -> QueuePrio2 a
convertPrioToPrio2Iter q = aux q newQueuePrio2

aux :: QueuePrio a -> QueuePrio2 a -> QueuePrio2 a
aux [] q2 = q2
aux q1 q2 = aux (snd (dequeuePrio q1)) (enqueuePrio2 (fst (dequeuePrio q1)) q2) 

-- la representation des listes en haskell favorise l'acces par la gauche
-- on introduit un nouveau type de listes qui favorise l'acces a droite

data Tsil a = Snoc (Tsil a) a | Lin deriving (Show,Eq)

-- dans ce cas [1,2,3] est représentée par 

tsil1 :: Tsil Int
tsil1 = Snoc (Snoc (Snoc Lin 1) 2) 3

-- Q9 definir la fonction equivalente a init

tini :: Tsil a -> Tsil a
tini (Snoc xs x) = xs

test9 = tini tsil1 == Snoc (Snoc Lin 1) 2

-- Q10 definir la fonction equivalente a head

daeh :: Tsil a -> a
daeh (Snoc Lin x) = x
daeh (Snoc xs x) = daeh xs

test10 = daeh tsil1 == 1