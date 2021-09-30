{-
-- | Lógica Computacional 2022-01
-- | Práctica 1: Introducción a Haskell 
-- | Profesor: Dr. Favio E. Miranda Perea
-- | Ayudante: Javier Enríquez Mendoza
-- | Ayudante: Ramón Arenas Ayala
-- | Laboratorio: Daniel Lugo Cano
-}

module Practica1 where 

import Data.List
-- | Tipo de dato Binario, es la representacion de todos los numero binarios que empiezan con uno

data Binario = U | Cero Binario | Uno Binario
-- |1| Definicion de la clase Show para el tipo de dato Binario

instance Show Binario where
  show U         = show 1
  show (Cero a ) = show a ++ show 0
  show (Uno a)   = show a ++ show 1
-- |2| sucesor. Regresa el sucesor de un Binario
-- -> Ejemplo sucesor de U (uno)  = Cero U , sucesor de 1 es 10
sucesor  :: Binario -> Binario
sucesor U       = Cero U
sucesor (Cero a) = Uno a
sucesor (Uno a)  = Cero(sucesor a)


-- |3| suma. Regresa la suma de 2 numeros de un Binarios
-- -> ejemplo suma de U U = Cero U , suma de 1 y 1 es 10
suma :: Binario -> Binario -> Binario
suma n n' = natABin((binANat n)+(binANat n'))

  {-suma U U                 = Cero U
suma U (Cero a)          = Uno a
suma (Cero a)  U         = Uno a
suma U (Uno a)           = Cero (suma U a)
suma (Uno a) U           = Cero (suma U a)
suma (Cero a) (Cero a')  = Cero (suma a a')
suma (Uno a) (Uno a')    = Cero (suma (suma U a) a')
suma (Uno a) (Cero a')   = Uno  (suma a a')
suma (Cero a) (Uno a')   = Uno  (suma a a')
-}
-- |4| producto. Regresa el producto de 2 numeros Binarios
-- -> Ejemplo producto de (Cero U) (Cero U) = (Cero (Cero U)) , producto de 10 con 10 es 100
producto :: Binario -> Binario -> Binario
producto n  n' = natABin((binANat n) * (binANat n'))

-- |5| natBinLista. Regresa el la representacion en Binario de un numero Decimal en forma de Lista
-- -> ejemplo natBinLista 8 = [1,0,0,0]
natBinLista :: Int -> [Int]
natBinLista 1 = [1]
natBinLista n = natBinLista (n `div` 2)++ [n `mod` 2]

-- |6| sumaBinLista. Regresa la suma de 2 listas que representan 2 numeros binarios.
-- -> ejemplo sumaBinLista de [1] [1,0] = (Uno U)
sumaBinLista :: [Int] -> [Int] -> Binario
sumaBinLista  xs ys = suma (binLBin xs) (binLBin ys)


{--FUNCIONES AUX--}
-- binLBin. Regresa la representación en Binario
binLBin:: [Int]-> Binario
binLBin [1]        = U
binLBin xs
    | last xs == 0 = Cero (binLBin(init xs))
    | otherwise    = Uno (binLBin(init xs))

--binLbin Regresa la representacion en Lista de 0s y 1s
binLbin:: Binario -> [Int]
binLbin U = [1]
binLbin (Cero a) = binLbin a ++ [0]
binLbin (Uno a) = binLbin a ++ [1]

--binInt:: Regresa el binario en su representación decimal

binInt:: [Int]->Int
binInt xs = foldlk (\x y -> 2*x+ y) 0 xs

-- foldl' Implementación de la función predefinida en Haskell
foldlk f z []     = z
foldlk f z (x:xs) = let z' = z `f` x
                    in foldlk f z' xs
{-- PUNTOS EXTRA --}

-- |1| natABin: Recibe un natural (mayor que 1) y devuelve un binario
natABin :: Int -> Binario
natABin 0 = error "Solo numeros mayores que 0"
natABin n = binLBin(natBinLista n)

-- |2| binANat: Recibe un binario y devuelve su reprentacion en numero natural (mayor que 1).
binANat :: Binario -> Int
binANat  b = binInt(binLbin b)

-- |3| predecesor: Recibe un binario y devuelve su binario anterior
predecesor :: Binario -> Binario
predecesor U = U
predecesor n =  natABin((binANat n) - 1)

{--Ejemplos--}

b   = Uno (Cero U)
b'  = Cero (Uno U)
b'' = Cero(Uno(Cero U))

s1 = sucesor (Cero U)
-- 11
s2 = sucesor (Uno U)
--100
s3 = sucesor (Cero (Uno U))
--111
sm1 =suma (Cero U) (Uno U)
--101
sm2 =suma (Uno U) (Uno U)
--110
sm3 =suma (Cero (Uno U)) (Uno (Cero U))

p1 = producto U U
--1
p2 = producto (Uno U) (Uno U)
--1001
p3 = producto (Cero U) (Cero U)
--100
n1 =natBinLista 16
--[1,0,0,0,0]
n2=natBinLista 100
--[1,1,0,0,1,0,0]
n3 =natBinLista 29
--[1,1,1,0,1]

sb1=sumaBinLista [1,0,1] [1,0]
--111
sb2=sumaBinLista [1,0] [1,1,1,0]
--10000
sb3=sumaBinLista [1,1,1] [1,0,1,0]
--10001
sb4= sumaBinLista [1,0,1] [1,1,1,1]
--10100
nA1=natABin 21
--10101
nA2 =natABin 0
--Exception: Solo numeros mayores que 0
bA1=binANat (Uno (Uno (Cero U)))
--11
bA2 =binANat (Uno (Cero (Uno (Cero U))))
--21
pc1 =predecesor U
--1
pc2 = predecesor (Cero U)
--1
pc3 =predecesor (Uno U)
--10
