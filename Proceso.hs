module Proceso (Procesador, AT(Nil,Tern), RoseTree(Rose), Trie(TrieNodo), foldAT, foldRose, foldTrie, procVacio, procId, procCola, procHijosRose, procHijosAT, procRaizTrie, procSubTries, unoxuno, sufijos, inorder, preorder, postorder, preorderRose, hojasRose, ramasRose, caminos, palabras, ifProc,(++!), (.!)) where

import Test.HUnit
import Control.Arrow (ArrowChoice(left, right))


--Definiciones de tipos

type Procesador a b = a -> [b]


-- Árboles ternarios
data AT a = Nil | Tern a (AT a) (AT a) (AT a) deriving Eq
at = Tern 1 (Tern 2 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 4 Nil Nil Nil)
at2 = Tern "p" (Tern "e" (Tern "l" (Nil) (Nil) (Nil)) (Tern "u" Nil Nil Nil) (Tern "c" Nil Nil Nil)) (Tern "h" (Tern "e" Nil Nil Nil) Nil Nil) Nil
--Es es árbol ternario con 1 en la raíz, y con sus tres hijos 2, 3 y 4.

-- RoseTrees
data RoseTree a = Rose a [RoseTree a] deriving Eq
rt = Rose 1 [Rose 2 [], Rose 3 [], Rose 4 [], Rose 5 []] 
rt2 = Rose "p" [Rose "p" [],Rose "e" [],Rose "l" [],Rose "u" [],Rose "c" [],Rose "h" [],Rose "e" []]
--es el RoseTree con 1 en la raíz y 4 hijos (2, 3, 4 y 5)

-- Tries
data Trie a = TrieNodo (Maybe a) [(Char, Trie a)] deriving Eq
t = TrieNodo (Just True) [('a', TrieNodo (Just True) []), ('b', TrieNodo Nothing [('a', TrieNodo (Just True) [('d', TrieNodo Nothing [])])]), ('c', TrieNodo (Just False) [])]
t2 = TrieNodo Nothing [('p', TrieNodo Nothing [('e', TrieNodo Nothing [('l', TrieNodo Nothing [('e',TrieNodo Nothing [('l',TrieNodo Nothing [('u',TrieNodo Nothing [('c',TrieNodo Nothing [('h',TrieNodo Nothing [('e', TrieNodo Nothing [])])])])])])])])])]
-- es el Trie Bool de que tiene True en la raíz, tres hijos (a, b, y c), y, a su vez, b tiene como hijo a d.


-- Definiciones de Show

instance Show a => Show (RoseTree a) where
    show = showRoseTree 0
      where
        showRoseTree :: Show a => Int -> RoseTree a -> String
        showRoseTree indent (Rose value children) =
            replicate indent ' ' ++ show value ++ "\n" ++
            concatMap (showRoseTree (indent + 2)) children

instance Show a => Show (AT a) where
    show = showAT 0
      where
        showAT :: Show a => Int -> AT a -> String
        showAT _ Nil = replicate 2 ' ' ++ "Nil"
        showAT indent (Tern value left middle right) =
            replicate indent ' ' ++ show value ++ "\n" ++
            showSubtree (indent + 2) left ++
            showSubtree (indent + 2) middle ++
            showSubtree (indent + 2) right

        showSubtree :: Show a => Int -> AT a -> String
        showSubtree indent subtree =
            case subtree of
                Nil -> replicate indent ' ' ++ "Nil\n"
                _   -> showAT indent subtree

instance Show a => Show (Trie a) where
    show = showTrie ""
      where
        showTrie :: Show a => String -> Trie a -> String
        showTrie indent (TrieNodo maybeValue children) =
            let valueLine = case maybeValue of
                                Nothing -> indent ++ "<vacío>\n"
                                Just v  -> indent ++ "Valor: " ++ show v ++ "\n"
                childrenLines = concatMap (\(c, t) -> showTrie (indent ++ "  " ++ [c] ++ ": ") t) children
            in valueLine ++ childrenLines


--Ejercicio 1
procVacio :: Procesador a b
procVacio = const []

procId :: Procesador a a
procId x = [x]

procCola :: Procesador [a] a
procCola [] = []
procCola (_:xs) = xs

procHijosRose :: Procesador (RoseTree a) (RoseTree a)
procHijosRose (Rose valor hijos)  = hijos

procHijosAT :: Procesador (AT a) (AT a)
procHijosAT Nil = []
procHijosAT (Tern _ left center right) = [left, center, right]

procRaizTrie :: Procesador (Trie a) (Maybe a)
procRaizTrie  (TrieNodo key _) = [key]

procSubTries :: Procesador (Trie a) (Char, Trie a)
procSubTries (TrieNodo _ value) = value


--Ejercicio 2

foldAT :: b -> (a -> b -> b -> b -> b) -> AT a -> b
foldAT atNil atBin Nil = atNil
foldAT atNil atBin (Tern raiz left right center) = atBin raiz (rec left) (rec center)  (rec right)
  where rec = foldAT atNil atBin

foldRose :: (t -> [b] -> b) -> RoseTree t -> b
foldRose cRose (Rose n hijos)= cRose n (map rec hijos)
  where rec = foldRose cRose


foldTrie :: (Maybe a -> [(Char, b)]  -> b) -> Trie a -> b
foldTrie fTrie (TrieNodo val hijos) = fTrie val (map rec hijos)
  where rec (c,t) = (c, foldTrie fTrie t)



--Ejercicio 3
unoxuno ::  Procesador [a] [a]
unoxuno = map (: [])

sufijos :: Procesador [a] [a]
sufijos x = concatMap (\i -> [drop i x]) [0..length x]

--Ejercicio 4

preorder :: AT a -> [a]
preorder = foldAT [] (\x left middle right -> x : (left ++ right ++ middle) )

inorder :: AT a -> [a]
inorder = foldAT [] (\x left middle right -> left ++ right ++[x] ++ middle )

postorder :: AT a -> [a]
postorder = foldAT [] (\x left middle right -> (left ++ right ++ middle ) ++ [x])

--Ejercicio 5
miRT = Rose 1 [Rose 2 [], Rose 3 [Rose 4 [], Rose 5 [], Rose 6 []]]
preorderRose :: Procesador (RoseTree a) a
preorderRose = foldRose (\x recs -> x : concat recs)

hojasRose :: Procesador (RoseTree a) a
hojasRose = foldRose (\x recs -> if null recs then [x] else concat recs)

ramasRose :: Procesador (RoseTree a) [a]
ramasRose = foldRose (\x recs -> if null recs then [[x]] else map (x :) (concat recs))


--Ejercicio 6



caminos :: Trie a -> [[Char]]
caminos  = foldTrie (\_ subcaminos -> "" : concatMap (\(c, t) -> map (c :) t) subcaminos)




--Ejercicio 7

palabras :: Trie a -> [[Char]]
palabras = foldTrie (\val subcaminos ->
    case val of
      Just _  -> "" : concatMap (\(c, t) -> map (c :) t) subcaminos 
      Nothing -> concatMap (\(c, t) -> map (c :) t) subcaminos 
  )

--Ejercicio 8

-- 8.a)
ifProc :: (a->Bool) -> Procesador a b -> Procesador a b -> Procesador a b
ifProc x a b =  (\y -> if x y then a y else b y)

-- 8.b)
(++!) :: Procesador a b -> Procesador a b -> Procesador a b
(++!) a b = (\x -> (a x) ++ (b x))

-- 8.c)
(.!) :: Procesador b c -> Procesador a b -> Procesador a c
(.!) a b = (\x -> concatMap a (b x)) 

--Ejercicio 9
-- Se recomienda poner la demostración en un documento aparte, por claridad y prolijidad, y, preferentemente, en algún formato de Markup o Latex, de forma de que su lectura no sea complicada.


{-Tests-}

main :: IO Counts
main = do runTestTT allTests

allTests = test [ -- Reemplazar los tests de prueba por tests propios
  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7,
  "ejercicio8a" ~: testsEj8a,
  "ejercicio8b" ~: testsEj8b,
  "ejercicio8c" ~: testsEj8c
  ]

testsEj1 = test [ -- Casos de test para el ejercicio 1
  
  ([] :: [Int]) ~=? (procVacio [1,2,3] ) 
  ,
  ([[1,2,3]]) ~=? (procId [1,2,3])
  ,
  ([2,3]) ~=? (procCola [1,2,3])
  ,
  ([] :: [Int]) ~=? (procCola ([] :: [Int]))
  ,
  ([Rose 2 [], Rose 3 [], Rose 4 [], Rose 5 []]) ~=? (procHijosRose (Rose 1 [Rose 2 [], Rose 3 [], Rose 4 [], Rose 5 []]) )
  ,
  [(Tern 2 Nil Nil Nil),(Tern 3 Nil Nil Nil),(Tern 4 Nil Nil Nil)] ~=? (procHijosAT (Tern 1 (Tern 2 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 4 Nil Nil Nil)))
  ,
  [(Just True)] ~=? (procRaizTrie  (TrieNodo (Just True) [('a', TrieNodo (Just True) [])] ))
  ,
  [('a',TrieNodo (Just True) [])] ~=? (procSubTries  (TrieNodo (Just True) [('a', TrieNodo (Just True) [])] ))


  ]



testsEj2 = test [ 
  (10)      
    ~=? (foldAT 0 (\x left middle right -> x + (left + middle + right)) at), -- sumar nodos arbol ternario
  (15)
    ~=? (foldRose (\x recs -> x + sum recs) rt), -- sumar nodos rose tree
  (3)
    ~=? (foldTrie (\val subcaminos ->
    case val of
      Just True  -> 1 + sum (map snd subcaminos)
      Nothing -> sum (map snd subcaminos)
      Just False -> sum (map snd subcaminos)
  ) t)              -- contar nodos True en trie
  ]

testsEj3 = test [ -- Casos de test para el ejercicio 3
  ([[[1]]]) ~=? (unoxuno [[1]]),
  ([[1],[2],[3],[4]]) ~=? (unoxuno [1,2,3,4]),
  ([""]) ~=? (sufijos ""),
  (["test","est","st","t",""]) ~=? (sufijos "test")
  ]

testsEj4 = test [ -- Casos de test para el ejercicio 4
  [1,2,3,4] ~=? (preorder at),
  ["p","e","l","u","c","h","e"] ~=? (preorder at2),
  [2,3,4,1] ~=? (postorder at),
  ["l","u","c","e","e","h","p"] ~=? (postorder at2),
  [2,3,1,4] ~=? (inorder at),
  ["l","u","e","c","e","h","p"] ~=? (inorder at2)
  ]

testsEj5 = test [ -- Casos de test para el ejercicio 5
  [1,2,3,4,5]  ~=? (preorderRose rt),
  ["p","p","e","l","u","c","h","e"] ~=? (preorderRose rt2),
  [2,3,4,5] ~=? (hojasRose rt),
  ["p","e","l","u","c","h","e"] ~=? (hojasRose rt2),
  [[1,2],[1,3],[1,4],[1,5]] ~=? (ramasRose rt),
  [["p","p"],["p","e"],["p","l"],["p","u"],["p","c"],["p","h"],["p","e"]] ~=? (ramasRose rt2)
  ]

testsEj6 = test [ -- Casos de test para el ejercicio 6
  ["","a","b","ba","bad","c"] ~=? (caminos t),
  ["","p","pe","pel","pele","pelel","pelelu","peleluc","peleluch","peleluche"] ~=? (caminos t2)
  ]

testsEj7 = test [ -- Casos de test para el ejercicio 7
    True ~=? True
                ]

testsEj8a = test [ -- Casos de test para el ejercicio 7
  True         -- Caso de test 1 - expresión a testear
    ~=? True                                          -- Caso de test 1 - resultado esperado
  ]
testsEj8b = test [ -- Casos de test para el ejercicio 7
  True         -- Caso de test 1 - expresión a testear
    ~=? True                                          -- Caso de test 1 - resultado esperado
  ]
testsEj8c = test [ -- Casos de test para el ejercicio 7
  True         -- Caso de test 1 - expresión a testear
    ~=? True                                          -- Caso de test 1 - resultado esperado
  ]

