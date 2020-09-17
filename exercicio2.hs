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
