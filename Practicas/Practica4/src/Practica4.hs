{-
-- | Lógica Computacional 2022-01
-- | Práctica 4: Unificación
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

module Practica4 where
import Data.List
import LPO
import LPOSintac

-- |1| Funcion simpSus que dada una sustitución elimina de ella los pares con componentes iguales
simpSus :: Subst -> Subst
simpSus []     = []
simpSus (x:xs) = simp [x] ++ simp xs

-- |2| Funcion compSus la cual recibe dos Subst y devuelve su compisición.
compSus :: Subst -> Subst -> Subst
compSus [] []  = []
compSus [] xs  = xs
compSus xs []  = xs
compSus (x:xs) ys  = duplicados ((trueSust [x] ys)++(compSus xs ys)) (x:xs) ys




-- |3| Funcion que dados dos términos devuelva una lista de sustituciones que cumplan las condiciones dadas
unifica :: Term -> Term -> [Subst]
unifica (F x [] ) (F y [])= if x==y then [[(a,F b [])| a<-[x,y],b<-lst2]] else []
unifica (V x) (V y)       = [[(x, V y)]]
unifica (V x) (F y [])    = [[(x, F y [])]]
unifica (F y []) (V x)    = [[(x, F y [])]]
unifica (F x xs) (V y)    = [[(y, F x xs)]]
unifica (V y) (F x xs)    = [[(y, F x xs)]]
unifica (F x xs) (F y ys)
  | x==y && xs == ys = []
  | x ==y            =  ((unifica (head xs) (head ys)) ++ unificador (tail xs) (tail ys))
  | otherwise        = []


--Crea sustituciones

susT::[Term] ->[Term]->[(Term, Term)]
susT [] [] = []
susT xs [] = []
susT [] xs = []
susT xs ys = acomoda (duplas xs ys)

duplas [] [] = []
duplas (x:xs) (y:ys) = (x,y):(duplas xs ys)


acomoda []                = []
acomoda [(V y , F x xs)]  = [(V y, F x xs)]
acomoda [(F x xs, V y)]   = [(V y, F x xs)]
acomoda [(F x xs, F y ys)]= [(F x xs, F y ys)]
acomoda [(V x, V y)]      = [(V x, V y)]
acomoda (x:xs)            = (acomoda [x])++ (acomoda xs)

-- |4| Funcion que unifica dos listas de términos de la misma longitud componente a componente
unificaListas :: [Term] -> [Term] -> [Subst]
unificaListas []   []       = []
unificaListas (x:xs) (y:ys) = (unifica x y) ++ (unificaListas xs ys)

-- |5| Funcion unificaConj que implementa el caso general para unificar un conjunto (lista)
unificaConj :: [Term] -> [Subst]
unificaConj []       = []
unificaConj (x:y:xs) = (unificaListas [x] [y]) ++ unificaConj xs


-- |6| Funcion que unifica dos literales
unificaLit :: Form -> Form -> [Subst]
unificaLit = error "Implementar"
{-
-------------------------------------------------FUNCIONES AUXILIARES---------------------------------------------------------------------
-}
nameF:: Subst -> [String]
nameF []     = []
nameF (x:xs) = name[snd x] ++ nameF xs


name' (_, (V x))   = x
name' (_, (F x xs)) = x

name:: [Term] -> [String]
name []         = []
name [(V x)]    = [x]
name [(F x xs)] = x: (name xs)
name (x:xs)     = name [x]

simp [] = []
simp [(x, V y)]     = if x == y then [] else  [(x, V y)]
simp [(x, F y xs)]  = if x == y then simp (dupla [x] xs) else [(x, F y (lst((simp (dupla [x] xs)))))]
simp (x:xs) = simp [x] ++ simp xs

-- Arreglat con orden superior
lst:: Subst -> [Term]
lst []     =  []
lst (x:xs) =  snd x : lst xs

-- Arreglat con orden superior
--dupla:: [Nombre] -> [Term] -> Subst
dupla _ []  = []
dupla xs ys = [(x,y) | x<-xs, y<-ys]

trueSust:: Subst-> Subst -> Subst
trueSust [] []                 = []
trueSust [] xs                 = []
trueSust xs []                 = xs
trueSust [(x, V t)] ts         = if elem t (map fst ts) then [dobleDupla (x, V t) (duplaLst t ts)] else [(x,V t)]
trueSust [(x, F y (a:as))] ts  =[(x, F y (map snd ((trueSust [(x,a)] ts)++(trueSust (dupla [x] as) ts ))))]
trueSust (x:xs) ts             = (trueSust [x] ts)++ trueSust xs ts

duplicados::Subst ->Subst-> Subst-> Subst
duplicados xs ys  []          = xs
duplicados xs zs (y:ys)
  | elem (fst y) (map fst zs) = duplicados (delete y xs) zs  ys
  | otherwise                 = union (duplicados xs zs ys) [y]

dobleDupla::(Nombre,Term)->(Nombre,Term)->(Nombre,Term)
dobleDupla  t d = (fst t, snd d)

duplaLst :: Nombre-> Subst->(Nombre,Term)
duplaLst x (y:ys) = if x == (fst y) then y else duplaLst x ys


lstSnd [] = []
lstSnd (x:xs) = (fst x) : (lstSnd xs)

lst2 = [[a]|a<-['a'..]]


sus::Subst-> Subst ->Subst
sus [] _                = []
sus [(x,V y)] xs         = if elem y (map fst xs) then [dobleDupla (x,V y) (duplaLst y xs)] else [(x, V y)]
sus [(x, F t (y:ys))] xs = if elem (name' (x,y)) (map fst xs) then [(x, F t ((snd(dobleDupla (x,y)(duplaLst (name'(x,y))xs ))):map snd(sus(dupla[x] ys) xs)))]
                          else [(x,F t (y:map snd (sus(dupla [x] ys) xs)))]
sus (x:xs) ys            = (sus [x] ys) ++ (sus xs ys)

sus2:: [Subst]->[Subst]
sus2  [] = []
sus2 xs = (sus (head xs) (lstSus xs)):sus2 (tail xs)

unificador:: [Term]->[Term] -> [Subst]
unificador xs [] = []
unificador [] xs  = []
unificador (x:xs) (y:ys)  =  (unifica x y)++ unificador xs ys

lstSus::[Subst]->Subst
lstSus [] = []
lstSus (x:xs) = (head x):lstSus xs

lstLsust [] =[]
lstLsust (x:xs)= [x]:lstLsust xs

{-
----------------------------------------------------------EJEMPLOS-----------------------------------------------------------------------
-}

simpSus1 = simpSus [("x", V "y"), ("y", V "z"), ("w", V "w")]
-- R = [("x",V "y"),("y",V "z")]
simpSus2 = simpSus [("x", V "y"), ("y", V "z"), ("w", V "z")]
-- R = [("x",V "y"),("y",V "z"),("w",V "z")]
simpSus3 = simpSus [("z", V "y"), ("w", V "z"), ("x", F "a" [])]
-- R = [("z",V "y"),("w",V "z"),("x",F "a" [])]
phi      = [("x", F "f" [V "y"]), ("y", V "z")]
gamma    = [("x", F "g" [V "w"]), ("z", V "m"), ("z", V "w")]
theta    = [("y", V "m"), ("w", F "f" [V "n"]), ("v", V "w")]
compSus1 = compSus gamma phi
--R= [("x",F "g" [V "w"]),("z",V "m"),("z",V "w"),("y",V "z")]
compSus2 = compSus phi theta
--R [("x",F "f" [V "m"]),("y",V "z"),("w",F "f" [V "n"]),("v",V "w")]
compSus3 = compSus (compSus gamma phi) theta
-- R [("x",F "g" [F "f" [V "n"]]),("z",V "m"),("z",F "f" [V "n"]),("y",V "z"),("w",F"f" [V "n"]),("v",V "w")]
compSus4 = compSus gamma (compSus phi theta)
-- R = [("x",F "g" [F "f" [V "n"]]),("z",V "m"),("z",F "f" [V "n"]),("y",V "z"),("w",F "f" [V "n"]),("v",V "w")]
unifica1 = unifica (F "f" [V "x", V "y", V "x"]) (F "f"[V "x", V "y", V "x"])
-- R []
unifica2 = unifica (F "f"[F "g" [V "x"], F "h"[V "x", V "u"]]) (F "f"[V "z",F "h"[F "f"[V "y", V "y"], V "z"]])
--R [("u", F "g"[F "f"[V "y", V "y"]]), ("x", F "f"[V "y", V "y"]), ("z", F "g"[V"x"])]]
