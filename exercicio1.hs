module Exercicio1 where
import qualified Vendas

import Data.Char (toUpper, isLower)

{-1-}
semZeroNoPeriodo :: Int -> Bool
semZeroNoPeriodo n
    | n == -1 = False
    | Vendas.vendas n == 0   = True
    | semZeroNoPeriodo (n-1) = True
    | otherwise = False

{-2-}
funct :: Int -> Int -> Int -> Bool
funct x y z
    | y >= x && x <= z  = False
    | otherwise         = True

{-3-}
maiusculas :: Char -> Char
maiusculas ch
    | isLower ch = toUpper(ch)
    | otherwise = ch

-- brincando com as funções
maisculasString :: String -> String
maisculasString str = [maiusculas ch | ch <-str]

{-4-}
infixl 6 &-
(&-) :: Int -> Int -> Int
x &- y = x - 2*y
{-
infixr 6 &-
(&-) :: Int -> Int -> Int
x &- y = x - 2*y
infix 6 &-
(&-) :: Int -> Int -> Int
x &- y = x - 2*y
    -}

{-5-}
first :: [Int] -> Int
first (a:x) = a

maiorNumeroVendas :: Int -> [Int]
maiorNumeroVendas (-1) = [0]
maiorNumeroVendas n 
    | Vendas.vendas n > first(maiorNumeroVendas(n-1)) = Vendas.vendas n : []
    | Vendas.vendas n == first(maiorNumeroVendas(n-1)) = Vendas.vendas n : maiorNumeroVendas(n-1) -- caso seja igual adiciona a lista
    | otherwise = first(maiorNumeroVendas(n-1)) : []

{-6-}
howManyLess :: Int -> Int -> Int
howManyLess valor day
    | day == -1 = 0
    | Vendas.vendas day < valor = 1 + howManyLess valor (day-1)
    | otherwise = howManyLess valor (day - 1)
