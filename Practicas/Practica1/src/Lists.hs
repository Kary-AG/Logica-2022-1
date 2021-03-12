{-
- Lógica Computacional 2021-2
- Profesor: Favio Ezequiel Miranda Perea
- Ayudante: Javier Enríquez Mendoza
- Ayudante: Fernando Abigail Galicia Mendoza
- Laboratorio: Emiliano Galeana Araujo
- Laboratorio: América Montserrat García Coronado
- Practica 1: Recordando Haskell. Listas
- Integrantes:
- Shin Ui Chul
- Azpeitia García Karyme Ivette
- Sánchez Reza Neider
-}

module Lists where
import Data.List

data List a = Void | Cons a (List a) -- deriving (Show, Eq)

instance (Show a) => Show (List a) where
  show Void       = "[]"
  show (Cons a x) = "(" ++ show a ++ ":" ++ show x ++ ")"

instance (Eq a) => Eq (List a) where
  (==) Void Void = True
  (==) (Cons a b) (Cons a' b') = if  a == a' then and [b == b'] else False

--------------------------------------------------------------------------------
--------                            FUNCIONES                           --------
--------------------------------------------------------------------------------

-- | Funcion que regresa tal vez la cabeza de la lista.
myHead :: List a -> Maybe a
myHead Void       = Nothing
myHead (Cons a _) = Just a

-- | Funcion que regresa tal vez la cola de la lista.
myTail :: List a -> Maybe (List a)
myTail Void         = Nothing
myTail (Cons a b)   = Just b

-- | Funcion que concatena recursivamente dos listas.
myConcat :: List a -> List a -> List a
myConcat Void Void    = Void
myConcat Void x       = x
myConcat x Void       = x
myConcat (Cons a x) x'= (Cons a (myConcat x x'))

-- | Funcion que dado un elemento =e= y una lista =l=, regresa la
-- lista =l= sin =e=.
remove :: (Eq a) => a -> List a -> List a
remove a Void       = Void
remove a (Cons a' x)
  | a == a'         = remove a x
  | otherwise       = (Cons a' (remove a x))

-- | Funcion que nos dice si un elemento está contenido en una lista.
myElem :: (Eq a) => a -> List a -> Bool
myElem a Void = False
myElem a (Cons a' x)
  |a == a'    = True
  |otherwise  = myElem a x

-- Funcion que filtra en nuestras listas.
myFilter :: (a -> Bool) -> List a -> List a
myFilter  f Void       = Void
myFilter f (Cons a x)
  | (f a) == True      = (Cons a (myFilter f x))
  |otherwise           = myFilter f x

-- Funcion que mapea una funcion a nuestras listas.
myMap :: (a -> b) -> List a -> List b
myMap  f Void      = Void
myMap f (Cons a x) = (Cons (f a) (myMap f x))

-- Funcion que devuelve la intersección de dos listas.
intersection :: (Eq a) => [a] -> [a] -> [a]
intersection xs ys = [x | x<- xs, elem x ys]  

-- Funcion que calcula la diferencia simétrica de dos listas.
symetric :: (Eq a) => [a] -> [a] -> [a]
symetric xs ys = [x | x <- xs++ys , elem x (intersection xs ys) == False]

-- Función que devuelve el conjunto potencia de una lista.
potencia :: Eq a => [a] -> [[a]]
potencia [] = [[]]
potencia (x:xs) = [(x:z) | z <- cjt]++ cjt
  where cjt     = potencia xs

--------------------------------------------------------------------------------
--------                             PRUEBAS                            --------
--------------------------------------------------------------------------------

-- Lista que contiene a los primeros cinco elementos.
l1 = (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Void)))))

-- Lista que contiene a los elementos del 6-10.
l2 = (Cons 6 (Cons 7 (Cons 8 (Cons 9 (Cons 10 Void)))))

-- instancia Eq

eq1 = l1 == l2
-- Regresa: False

eq2 = l1 == l1
-- Regresa: True

myHead1 = myHead l1
-- Regresa: Just 1

myHead2 = myHead Void
-- Regresa: Nothing

myTail1 = myTail l1
-- Regresa: Just (2:(3:(4:(5:[]))))

myTail2 = myTail  Void
-- Regresa: Nothing

myConcat1 = myConcat l1 l2
-- Regresa: (1:(2:(3:(4:(5:(6:(7:(8:(9:(10:[]))))))))))

myConcat2 = myConcat l2 l1
-- Regresa: (6:(7:(8:(9:(10:(1:(2:(3:(4:(5:[]))))))))))

remove1 = remove 1 l1
-- Regresa: (2:(3:(4:(5:[]))))

remove2 = remove 4 l2
-- Regresa: (6:(7:(8:(9:(10:[])))))

myElem1 = myElem 1 l1
-- Regresa: True

myElem2 = myElem 4 l2
-- Regresa: False

myFilter1 = myFilter odd l1
-- Regresa: (1:(3:(5:[])))

myFilter2 = myFilter even l1
-- Regresa: (2:(4:[]))

myMap1 = myMap (+5) l1
-- Regresa: (6:(7:(8:(9:(10:[])))))

myMap2 = myMap (*0) l2
-- Regresa: (0:(0:(0:(0:(0:[])))))

intersection1 = intersection [1,2,3] [2,3]
-- Regresa: [2,3]

symetric1 = symetric [1,2,3] [2,3]
-- Regresa: [1]

potencia1 = potencia [1,2,3]
-- Regresa: [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]
