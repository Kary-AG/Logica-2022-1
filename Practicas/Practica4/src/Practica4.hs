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
unifica (F x xs) (V y)    = if elem y (name xs) then error "no unificable" else [[(y, F x xs)]]
unifica (V y) (F x xs)    = if elem y (name xs ) then error "no unificable" else [[(y, F x xs)]]
unifica (F x xs) (F y ys)
  | x==y && xs == ys = []
  | x == y = [head$change(sust(swap(desc xs ys)))]:unificador(desc2(tail(sust(swap(desc xs ys)))))
  | otherwise = []

desc2::[(Term, Term)]->[[Term]]
desc2 []     = []
desc2 (x:xs) = (duplalst x): desc2 xs

unificador:: [[Term]]-> [Subst]
unificador [] = []
unificador (x:xs) = (unifica (head x) (head(tail x))) ++ unificador xs

duplalst (x,y) =[x, y]

change::[(Term, Term)]->[(Nombre, Term)]
change []     = []
change (x:xs) = (nameD x, snd x):change xs

nameD ::(Term, Term)-> String
nameD (V x, _) =  x
--nameD (n, _) = n

desc::[Term]->[Term]->[(Term, Term)]
desc [] [] = []
desc (x:xs) (y:ys)  = (x,y):(desc xs ys)

sust::[(Term, Term)]->[(Term, Term)]
sust  [] = []
sust ((V x, y):xs)
  | elem x (nameDt xs) = (V x, y):sust(rsust (x, y) (sacaElem x xs) ++ (xs\\sacaElem x xs))
  | otherwise = (V x, y): sust xs
sust (x:xs) = x:sust xs

lstLsust [] =[]
lstLsust (x:xs)= [x]:lstLsust xs

swap []                = []
swap [(V y , F x xs)]  = [(V y, F x xs)]
swap [(F x xs, V y)]   = [(V y, F x xs)]
swap [(F x xs, F y ys)]= [(F x xs, F y ys)]
swap [(V x, V y)]      = [(V x, V y)]
swap (x:xs)            = (swap [x])++ (swap xs)

--Funcion que realmente susituye

rsust::(Nombre, Term)->[(Term,Term)]->[(Term, Term)]
rsust _ []           = []
rsust x [(V y, V z)]
  | (fst x) == y && (fst x) == z = [(snd x, snd x)]
  | (fst x )== y = [(snd x, V z)]
  | (fst x) == z = [(V y, snd x)]
rsust a [(V b, F y xs)]
  | (fst a) == b = [(snd a, F y (susTer a xs))]
  | otherwise = [(V b, F  y (susTer a xs))]
rsust a [(F y xs, V b)]
   | (fst a) == b = [(F y (susTer a xs), snd a)]
   | otherwise = [(F  y (susTer a xs), snd a)]
rsust a [(F y ys, F x xs)] = [(F y (susTer a ys), F x (susTer a xs))]
rsust a (x:xs) = (rsust a [x])++(rsust a xs)


--Función sustituye en [Term]
susTer::(Nombre, Term)->[Term]->[Term]
susTer _ [] = []
susTer x [V y]
  |  fst x == y       = [snd x]
  |otherwise          = [V y]
susTer x [F y []]     = [F y []]
susTer x [F y zs]     = [F y (susTer x zs)]
susTer x (y:ys)       = susTer x [y]++ susTer x ys

--Sacar el elemento a sustituir
sacaElem:: Nombre ->[(Term,Term)]->[(Term, Term)]
sacaElem _ []     = []
sacaElem x (y:ys)
  | elem x (nameDt [y])  = y: sacaElem x ys
  | otherwise = sacaElem x ys

--Nos regresa el nombre de todas las varaibles de nuestra lista de Terminos
nameDt :: [(Term, Term)] ->[String]
nameDt [] = []
nameDt [(V x, V y)]       = [x,y]
nameDt [(V x, F y ys)]    = x:y:name ys
nameDt [(F y ys, V x)]    = y:x:name ys
nameDt [(F y ys, F x xs)] = y:x:name ys ++name xs
nameDt (x:xs)             = nameDt [x] ++ nameDt xs

nameF:: Subst -> [String]
nameF []     = []
nameF (x:xs) = name[snd x] ++ nameF xs


name' (_, (V x))   = x
name' (_, (F x xs)) = x

name:: [Term] -> [String]
name []         = []
name [(V x)]    = [x]
name [(F x xs)] = name xs
name (x:xs)     = name [x]++name xs

-- |4| Funcion que unifica dos listas de términos de la misma longitud componente a componente
unificaListas :: [Term] -> [Term] -> [Subst]
unificaListas []   []       = []
unificaListas xs  []        = []
unificaListas [] xs         = []
unificaListas (x:xs) (y:ys) = (unifica x y) ++ (unificaListas xs ys)

-- |5| Funcion unificaConj que implementa el caso general para unificar un conjunto (lista)
unificaConj :: [Term] -> [Subst]
unificaConj []       = []
unificaConj [x]  = unificaListas [x] []
unificaConj (x:y:z:xs) = unificaListas [x] [y] ++ unificaListas (unificaC [y] (unificaListas [x] [y])) [z] ++ unificaConj xs


-- |6| Funcion que unifica dos literales
unificaLit :: Form -> Form -> [Subst]
unificaLit (Pr x xs) (Pr y ys)= unificaListas xs  ys

-- |Extra| Función recibe 2 formulas y regresa el conjunto discorde
conjuntoDiscorde :: Either Form Term -> Either Form Term -> Either [Form] [Term]
conjuntoDiscorde (Left  (Pr x xs)) (Left(Pr y ys))
  |  x == y   = Right (cjtDTerm xs ys)
  | otherwise =  Left [(Pr x xs), (Pr y ys)]
conjuntoDiscorde (Right(V x)) (Right(V y))
  | x == y    = Right []
  | otherwise = Right [V x, V y]
conjuntoDiscorde (Right(F x xs)) (Right(F y ys))
  |  x == y   = Right (cjtDTerm xs ys)
  | otherwise = Right [(F x xs), (F y ys)]
conjuntoDiscorde (Right (F x xs)) (Right (V y)) = Right [(F x xs),( V y)]
conjuntoDiscorde (Right (V y)) (Right (F x xs)) = Right [(V y), (F x xs)]

cjtDTerm::[Term] ->[Term]->[Term]
cjtDTerm [] n =  n
cjtDTerm [(V x)] [(V y)]
  |  x==y     = []
  | otherwise =  [(V x), (V y)]
cjtDTerm [(F x xs)] [(F y ys)]
  | x == y    = cjtDTerm xs ys
  | otherwise = [ (F x xs), (F y ys)]
cjtDTerm [(F x xs)] [(V y)]  =  [(F x xs), (V y)]
cjtDTerm [(V y)] [(F x xs)]  =  [V y, (F x xs)]
cjtDTerm (x:_) (y:_)       = (cjtDTerm [x] [y])

-- |Extra| Algoritmo de unificación Robinson
robinson:: Either Form Term -> Either Form Term-> Either [[Form]] [[Term]]
robinson (Left p) (Left q)   = if conjuntoDiscorde (Left p)  (Left q) == Left []
                               then Left []
                               else (unificaR (conjuntoDiscorde (Left p)(Left q)) (Left p) (Left q))
robinson (Right p) (Right q) = if conjuntoDiscorde (Right p) (Right q) == Right []
                              then (Right [])
                              else unificaR (conjuntoDiscorde (Right p) (Right q)) (Right p) (Right q)


unificaR :: Either [Form] [Term] -> Either Form Term -> Either Form Term -> Either [[Form]] [[Term]]
unificaR (Left n) (Left  _) (Left _) = robinson(Left (head(sustP n))) (Left (head$tail(sustP n)))
unificaR (Right n) (Left (Pr _ xs)) (Left (Pr _ ys)) =
   robinson (Right(head(sustT (swap [(head n, head$tail n)]) xs ys))) (Right(head$tail(sustT (swap [(head n,head$tail n)]) xs ys)))
unificaR (Right n) (Right xs) (Right ys) =
   robinson (Right(head(sustT (swap [(head n, head$tail n)]) [xs] [ys]))) (Right(head$tail(sustT (swap [(head n,head$tail n)]) [xs] [ys])))


sustT ::[(Term, Term)] -> [Term]-> [Term] -> [Term]
sustT  _ [] [] = []
sustT [(V x, xs)] [V y] [] = if x==y then [xs] else [V y]
sustT n [F x xs] [] = sustT n xs []
sustT [(V x, xs )] [V y] [V z]
  |  x == y && x == z = [xs , xs]
  | x == y = [xs, V z]
  | x == z = [V y, xs]
  | otherwise = [V y, V z]
sustT [(V x, xs)] [F  _ ys] [F _ zs] = (sustT [(V x, xs)] ys zs)
sustT [(V x, xs)] [F _ ys] [V z]
  | x == z = [xs]++ (sustT [(V x, xs)] ys [])
  | otherwise = (V z): sustT [(V x, xs)] ys []
sustT [(V x, xs)] [V z] [F _ ys]
  | x == z = [xs]++ (sustT [ ( V x, xs)] ys  [])
  | otherwise = (V z):sustT[(V x, xs)] ys []
sustT d (x:xs) (y:ys) = (sustT d [x] [y]) ++ (sustT d xs ys)

sustP:: [Form]-> [Form]
sustP [(Pr x xs), (Pr y ys) ] = [(Pr x xs), (Pr x ys)]
{-
-------------------------------------------------FUNCIONES AUXILIARES---------------------------------------------------------------------
-}

simp [] = []
simp [(x, V y)]     = if x == y then [] else  [(x, V y)]
simp [(x, F y xs)]  = if x == y then simp (dupla [x]  xs) else [(x, F y (lst((simp (dupla [x] xs)))))]
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

lstSus::[Subst]->Subst
lstSus [] = []
lstSus (x:xs) = (head x):lstSus xs


unificaC :: [Term]->[Subst]->[Term]
unificaC [] _ = []
unificaC _ [] = []
unificaC ys (x:xs) = (susTer (head x) ys)++ unificaC  ys xs

change2::[(Nombre,Term)]->(Term,Term)
change2 [(x, y)]= (V x, y)

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
t1 = F "h" [F "f"[V "w"], F "f"[V "w"]]
t2 = F "h" [F "f"[V "w"], F "f"[V "x"]]
t3 = F "h" [V "z", V "z"]
t4 = V "x"
unificaConj1 = unificaConj [t1,t3,t4]
--R = [[("z", F "f" [V "w"])], [("x", F "h"[F "f"[V "w"], F "f" [V "w"]])]]
unificaConj2 = unificaConj [t2,t3]
-- *** Exception: no unificables.
alpha = Pr "P" [F "f"[F "h" [V "z"], F "h"[V "z"]]]

beta = Pr "P" [V "x"]

zeta = Pr "P" [F "f"[F "h"[V "z"], F "h" [V "x"]]]

unificaLit1 = unificaLit alpha beta
--R [[("x",F "f" [F "h" [V "z"],F "h" [V "z"]])] ]
unificaLit2 = unificaLit zeta beta
-- *** Exception: no unificables.


--f(a, x, h(g(z))), f(z, h(y), h(y))

f1 = F "f" [V "a", V "x", F "h" [F "g"[V "z"]]]
f2 = F "f" [V "z", F "h" [V "y"], F "h" [V "y"]]
