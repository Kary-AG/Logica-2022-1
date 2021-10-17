{-
-- | Lógica Computacional 2022-01
-- | Práctica 1: Introducción a Haskell 
-- | Profesor: Dr. Favio E. Miranda Perea
-- | Ayudante: Javier Enríquez Mendoza
-- | Ayudante: Ramón Arenas Ayala
-- | Laboratorio: Daniel Lugo Cano
--
-- | Integrantes:
-- | Shin Ui Chul
-- | Azpeitia García Karyme Ivette
-- | Sánchez Reza Neider
-}

module Practica1 where 

import Data.List

-- | Tipo de dato Binario, es la representacion de todos los numeros binarios
-- que empiezan con uno.
data Binario = U | Cero Binario | Uno Binario

-- |1| Definicion de la clase Show para el tipo de dato Binario.
instance Show Binario where
  show U         = show 1
  show (Cero a ) = show a ++ show 0
  show (Uno a)   = show a ++ show 1
  
-- |2| sucesor. Regresa el sucesor de un Binario.
-- -> Ejemplo sucesor de U = Cero U, sucesor de 1 es 10.
sucesor :: Binario -> Binario
sucesor U        = Cero U
sucesor (Cero a) = Uno a
sucesor (Uno a)  = Cero (sucesor a)

-- |3| suma. Regresa la suma de 2 numeros de un Binario.
-- -> Ejemplo suma de U U = Cero U, suma de 1 y 1 es 10.
suma :: Binario -> Binario -> Binario
suma n n' = natABin((binANat n) + (binANat n'))

{- Otro intento con todos los casos:
suma U U                 = Cero U
suma U (Cero a)          = Uno a
suma (Cero a)  U         = Uno a
suma U (Uno a)           = Cero (suma U a)
suma (Uno a) U           = Cero (suma U a)
suma (Cero a) (Cero a')  = Cero (suma a a')
suma (Uno a) (Uno a')    = Cero (suma (suma U a) a')
suma (Uno a) (Cero a')   = Uno  (suma a a')
suma (Cero a) (Uno a')   = Uno  (suma a a')
-}

-- |4| producto. Regresa el producto de dos numeros Binario.
-- -> Ejemplo producto de (Cero U) (Cero U) = (Cero (Cero U)), producto de 10
-- con 10 es 100.
producto :: Binario -> Binario -> Binario
producto n n' = natABin((binANat n) * (binANat n'))

-- |5| natBinLista. Regresa la representacion en Binario de un numero decimal en forma de Lista.
-- -> Ejemplo natBinLista 8 = [1,0,0,0]
natBinLista :: Int -> [Int]
natBinLista 0 = error "Solo numeros mayores que 0."
natBinLista 1 = [1]
natBinLista n = natBinLista (n `div` 2)++ [n `mod` 2]

-- |6| sumaBinLista. Regresa la suma de dos listas que representan dos numeros binarios.
-- -> Ejemplo sumaBinLista de [1] [1,0] = (Uno U)
sumaBinLista :: [Int] -> [Int] -> Binario
sumaBinLista xs ys = suma (binLBin xs) (binLBin ys)

{-- FUNCIONES AUX --}
-- | binLBin. Regresa la representación en Binario de una lista de 0s y 1s.
binLBin :: [Int] -> Binario
binLBin [1]        = U
binLBin xs
    | last' xs == 0 = Cero (binLBin(init' xs))
    | otherwise    = Uno (binLBin(init' xs))

-- | binLbin. Regresa la representacion en Lista de 0s y 1s, de un numero Binario.
binLbin :: Binario -> [Int]
binLbin U = [1]
binLbin (Cero a) = binLbin a ++ [0]
binLbin (Uno a) = binLbin a ++ [1]

-- | binInt. Regresa el binario (en lista) en su representación decimal.
binInt :: [Int] -> Int
binInt xs = foldll (\x y -> 2*x+ y) 0 xs

-- | foldl. Implementación de la función foldl, predefinida en Haskell.
foldll :: (t -> a -> t) -> t -> [a] -> t
foldll _ z []     = z
foldll f z (x:xs) = let z' = z `f` x
                    in foldll f z' xs

-- | reverse' Implementación de la función reverse
reverse' ::[a]->[a]
reverse' = foldll (\e a -> a:e) []

-- | last' Implementación de la función last
last' ::[a]-> a
last' []     = error "D:2y"
last' (x:[]) = x
last' (_:xs) = last' xs

-- | init' Implementación de la función init
init' ::[a]->[a]
init' []      = error "D:1"
init' [x]     = []
init' (x:xs)  = x: init' xs



{-- PUNTOS EXTRA --}
-- |1| natABin. Recibe un natural (mayor que 1) y devuelve un Binario.
natABin :: Int -> Binario
natABin 0 = error "Solo numeros mayores que 0."
natABin n = binLBin(natBinLista n)

-- |2| binANat. Recibe un Binario y devuelve su reprentacion en numero natural (mayor que 1).
binANat :: Binario -> Int
binANat b = binInt(binLbin b)

-- |3| predecesor. Recibe un Binario y devuelve su Binario anterior.
predecesor :: Binario -> Binario
predecesor U = U
predecesor n = natABin((binANat n) - 1)

{-- EJEMPLOS --}
b   = Uno (Cero U)
-- Devuelve: 101
b'  = Cero (Uno U)
-- Devuelve: 110
b'' = Cero(Uno(Cero U))
-- Devuelve: 1010

s1 = sucesor (Cero U)
-- Devuelve: 11
s2 = sucesor (Uno U)
-- Devuelve: 100
s3 = sucesor (Cero (Uno U))
-- Devuelve: 111

sm1 = suma (Cero U) (Uno U)
-- Devuelve: 101
sm2 = suma (Uno U) (Uno U)
-- Devuelve: 110
sm3 = suma (Cero (Uno U)) (Uno (Cero U))
-- Devuelve: 1011

p1 = producto U U
-- Devuelve: 1
p2 = producto (Uno U) (Uno U)
-- Devuelve: 1001
p3 = producto (Cero U) (Cero U)
-- Devuelve: 100

n1 = natBinLista 16
-- Devuelve: [1,0,0,0,0]
n2 = natBinLista 100
-- Devuelve: [1,1,0,0,1,0,0]
n3 = natBinLista 29
-- Devuelve: [1,1,1,0,1]

sb1 = sumaBinLista [1,0,1] [1,0]
-- Devuelve: 111
sb2 = sumaBinLista [1,0] [1,1,1,0]
-- Devuelve: 10000
sb3 = sumaBinLista [1,1,1] [1,0,1,0]
-- Devuelve: 10001
sb4 = sumaBinLista [1,0,1] [1,1,1,1]
-- Devuelve: 10100

nA1 = natABin 21
-- Devuelve: 10101
nA2 = natABin 0
-- Devuelve: Exception: Solo numeros mayores que 0.

bA1 = binANat (Uno (Uno (Cero U)))
-- Devuelve: 11
bA2 = binANat (Uno (Cero (Uno (Cero U))))
-- Devuelve: 21

pc1 = predecesor U
-- Devuelve: 1
pc2 = predecesor (Cero U)
-- Devuelve: 1
pc3 = predecesor (Uno U)
-- Devuelve: 10
