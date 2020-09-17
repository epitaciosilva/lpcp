module Exercicio2 where

{-1-}
ocorrencias :: Int -> [Int] -> Int
ocorrencias n [] = 0
ocorrencias n (a:x) 
    | n == a = 1 + ocorrencias n x
    | otherwise = ocorrencias n x

{-2-}
removeEncontrados :: Int -> [Int] -> [Int]
removeEncontrados  n [] = []
removeEncontrados  n (a:x)
    | n == a = removeEncontrados  n x
    | otherwise = a : removeEncontrados n x

unicos :: [Int] -> [Int]
unicos [] = []
unicos (a:x) 
    | ocorrencias a (a:x) == 1  = a : unicos x
    | otherwise  = unicos(removeEncontrados a x)

{-3-}
putFinal :: Int -> [Int] -> [Int]
putFinal n [] = [n]
putFinal n (a:x) =  a : putFinal n x

desloque :: [Int] -> [Int]
desloque [] = []
desloque (a:x) = putFinal a x

{-5-}
elimina :: [Int] -> Int -> [Int]
elimina (a:x) n = removeEncontrados n (a:x)

{-6-}
inList :: Int -> [Int] -> (Bool, [Int])  -- retorna Bool se pertence ou a lista e então retorna o restante da lista
inList n [] = (False,[])
inList n (b:y)
    | n == b = (True, y)
    | otherwise = inList n y

emptyList :: [Int] -> Bool
emptyList [] = True
emptyList (a:x) = False

sublista :: [Int] -> [Int] -> Bool
sublista _ [] = False
sublista (a:x) (b:y)
    | emptyList x && (fst (inList a (b:y))) = True
    | otherwise = sublista x (snd (inList a (b:y)))
