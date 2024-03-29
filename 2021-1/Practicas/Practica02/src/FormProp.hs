module FormulasProposicionales where

import Operadores
import Data.List

-- Representa variables p, q, r, s...
type VarP = String

-- Representa las variables que se evalúan a True.
type Estado = [VarP]

-- data Prop
data Prop = Var VarP
          | Neg Prop
          | Conj Prop Prop
          | Disy Prop Prop
          | Impl Prop Prop
          | Syss Prop Prop

-- instance Show -- TODO
instance Show Prop where
  show (Var p)      = p
  show (Neg p)      = "(¬)"++ show p
  show (Conj p1 p2) = "("  ++ show p1 ++ "(/\\)" ++ show p2 ++ ")"
  show (Disy p1 p2) = "("  ++ show p1 ++ "(v)" ++ show p2 ++ ")"
  show (Impl p1 p2) = "("  ++ show p1 ++ "(-->)" ++ show p2 ++ ")"
  show (Syss p1 p2) = "("  ++ show p1 ++ "(<-->)"++ show p2 ++ ")"
-- instance Operadores -- TODO
instance Operadores Prop where
  (¬)    = Neg
  (\/)   = Disy
  (/\)   = Conj
  (-->)  = Impl
  (<-->) = Syss


---------------------------------------------------------------------------------
--------                             FUNCIONES                           --------
---------------------------------------------------------------------------------

-- Función que recibe una fórmula y devuelve el conjunto (no hay
-- repeticiones) de variables que hay en una fórmula.
vars :: Prop -> [VarP]
vars (Var p) = [p]
vars (Neg p) = vars p
vars (Disy p1 p2) = nub$vars p1 ++ vars p2
vars (Conj p1 p2) = nub$vars p1 ++ vars p2
vars (Impl p1 p2) = nub$vars p1 ++ vars p2
vars (Syss p1 p2) = nub$vars p1 ++ vars p2

-- Funcion que evalua una proposicion dado un estado.
interp :: Estado -> Prop -> Bool
interp [] (Var x)    = False
interp xs (Var x)    = elem x xs
interp xs (Neg x)    = not (interp xs x)
-- yo aqui habria puesto solo "interp i p && interp i q" :O
interp xs (Conj p q) = interp xs p && interp xs q
interp xs (Disy p q) = interp xs p || interp xs q
interp xs (Impl p q) = not$interp xs p && not (interp xs q)
interp xs (Syss p q) = interp xs p == interp xs q

-- Función que cuenta el número de conectivos.
numConectivos :: Prop -> Int
numConectivos (Var p)    = 0
numConectivos (Neg p)    = 1 + numConectivos p
numConectivos (Conj p q) = 1 + numConectivos p + numConectivos q
numConectivos (Disy p q) = 1 + numConectivos p + numConectivos q
numConectivos (Impl p q) = 1 + numConectivos p + numConectivos q
numConectivos (Syss p q) = 1 + numConectivos p + numConectivos q

-- Funcion que elimina las equivalencias (<->).
elimEquiv :: Prop -> Prop
elimEquiv (Var p)    = Var p
elimEquiv (Syss p q) = Conj (Impl (elimEquiv p) (elimEquiv q)) (Impl (elimEquiv q) (elimEquiv p))
elimEquiv (Neg p)    = Neg$ elimEquiv p
elimEquiv (Conj p q) = Conj (elimEquiv p) (elimEquiv q)
elimEquiv (Disy p q) = Disy (elimEquiv p) (elimEquiv q)
elimEquiv (Impl p q) = Impl (elimEquiv p) (elimEquiv q)

-- Funcion que elimina las implicaciones, puedes suponer que no hay
-- equivalencias.
elimImpl :: Prop -> Prop
elimImpl (Var x)    = Var x
elimImpl (Impl p q) = Disy (Neg (elimImpl p)) (elimImpl q)
elimImpl (Neg p)    = Neg$ elimImpl p
elimImpl (Conj p q) = Conj (elimImpl p)(elimImpl q)
elimImpl (Disy p q) = Disy (elimImpl p)(elimImpl q)
elimImpl (Syss p q) = Conj(elimImpl (Impl p q)) (elimImpl(Impl q p))

-- Función que dada una fórmula φ con n-variables devuelve la lista
-- con 2^{n} estados distintos para φ.
estados :: Prop -> [Estado]
estados phi = subconj$ vars phi

-- Funcion que nos da TODOS los modelos de una proposicion.
modelos :: Prop -> [Estado]
modelos phi = [i | i<-(estados phi), interp i phi]

-- Funcion que nos dice si una proposicion es una tautologia.
tautologia :: Prop -> Bool
tautologia phi = estados phi == modelos phi

-- Función que nos dice si una proposición es satisfacible en una
-- interpretación.
satisfen :: Estado -> Prop -> Bool
satisfen = interp

-- Funcion que nos dice si una proposicion es satifacible.
satisfacible :: Prop -> Bool
satisfacible phi = modelos phi /= []

-- Función que nos dice si una proposición es insatisfacible en una
-- interpretación.
insatisfen :: Estado -> Prop -> Bool
insatisfen i phi = interp i phi == False

-- Funcion que nos dice si una proposicion es instisfacible.
contrad :: Prop -> Bool
contrad phi = modelos phi == []

-- Función que regresa una fórmula equivalente donde las negaciones
-- solo se aplican a fórmulas atómicas.
meteNegacion :: Prop -> Prop
meteNegacion (Var p)         = Var p
meteNegacion (Neg(Var p))    = Neg (Var p)
meteNegacion (Neg(Neg p))    =  meteNegacion p
meteNegacion (Neg(Conj p q)) = Disy (meteNegacion (Neg p)) (meteNegacion (Neg q))
meteNegacion (Neg(Disy p q)) = Conj (meteNegacion (Neg p)) (meteNegacion (Neg q))
meteNegacion (Neg(Impl p q ))= Conj (meteNegacion p) (meteNegacion (Neg q))
meteNegacion (Neg(Syss p q)) = Disy (Conj (meteNegacion p) (meteNegacion (Neg q)))(Conj (meteNegacion q) (meteNegacion (Neg p)))
meteNegacion (Conj p q)  = Conj (meteNegacion p) (meteNegacion q)
meteNegacion (Disy p q)  = Disy (meteNegacion p) (meteNegacion q)
meteNegacion (Impl p q)  = Impl (meteNegacion p) (meteNegacion q)
meteNegacion (Syss p q)  = Syss (meteNegacion p) (meteNegacion q)

-- Función que regresa una fórmula equivalente donde las disyunciones
-- sólo se aplica a disyunciones o literales.
-- Puedes suponer que la fórmula que recibes está en FNN.
interiorizaDisyuncion :: Prop -> Prop
interiorizaDisyuncion (Var p)                = Var p
interiorizaDisyuncion (Neg p)                = Neg (interiorizaDisyuncion p)
interiorizaDisyuncion (Disy p (Conj q r))    = Conj (Disy p q) (Disy p r)
interiorizaDisyuncion (Disy (Conj p q) r )   = Conj (Disy p r) (Disy q r)
interiorizaDisyuncion (Disy (Var p) (Var q)) = Disy (Var p) (Var q)
interiorizaDisyuncion (Disy (Var p) q)       =  interiorizaDisyuncion(Disy (Var p) (interiorizaDisyuncion q))
interiorizaDisyuncion (Disy (Neg p) (Var q)) =  interiorizaDisyuncion (Disy (interiorizaDisyuncion (Neg p)) (Var q))
interiorizaDisyuncion (Disy (Disy p q) (Var r)) =  interiorizaDisyuncion(Disy (interiorizaDisyuncion (Disy p q)) (Var r))
interiorizaDisyuncion (Disy (Impl p q) (Var r)) =  interiorizaDisyuncion(Disy (interiorizaDisyuncion (Impl p q)) (Var r))
interiorizaDisyuncion (Disy (Syss p q) (Var r)) =  interiorizaDisyuncion(Disy (interiorizaDisyuncion (Syss p q)) (Var r))
interiorizaDisyuncion (Disy (Neg p) (Neg q))    =  interiorizaDisyuncion(Disy (interiorizaDisyuncion (Neg p)) (interiorizaDisyuncion (Neg q)))
interiorizaDisyuncion (Disy (Disy p q) (Neg r)) =  interiorizaDisyuncion(Disy (interiorizaDisyuncion (Disy p q)) (interiorizaDisyuncion r))
interiorizaDisyuncion (Disy (Impl p q) (Neg r)) =  interiorizaDisyuncion(Disy (interiorizaDisyuncion (Impl p q)) (interiorizaDisyuncion (Neg r)))
interiorizaDisyuncion (Disy (Syss p q) (Neg r)) =  interiorizaDisyuncion(Disy (interiorizaDisyuncion (Syss p q)) (interiorizaDisyuncion (Neg r)))
interiorizaDisyuncion (Disy (Neg p ) (Disy q r)) =  interiorizaDisyuncion(Disy (interiorizaDisyuncion (Neg p)) (interiorizaDisyuncion (Disy q r)))
interiorizaDisyuncion (Disy (Disy p q) (Disy r s)) =  interiorizaDisyuncion(Disy (interiorizaDisyuncion (Disy p q)) (interiorizaDisyuncion (Disy r s)))
interiorizaDisyuncion (Disy (Impl p q) (Disy r s)) =  interiorizaDisyuncion(Disy (interiorizaDisyuncion (Impl p q)) (interiorizaDisyuncion (Disy r s)))
interiorizaDisyuncion (Disy (Syss p q) (Disy r s)) =  interiorizaDisyuncion(Disy (interiorizaDisyuncion (Syss p q)) (interiorizaDisyuncion (Disy r s)))
interiorizaDisyuncion (Disy (Neg p) (Impl q r)) =  interiorizaDisyuncion(Disy (interiorizaDisyuncion (Neg p)) (interiorizaDisyuncion (Impl p r)))
interiorizaDisyuncion (Disy (Disy p q) (Impl r s)) =  interiorizaDisyuncion(Disy (interiorizaDisyuncion (Disy p q)) (interiorizaDisyuncion (Impl r s)))
interiorizaDisyuncion (Disy (Impl p q) (Impl r s)) =  interiorizaDisyuncion(Disy (interiorizaDisyuncion (Impl p q)) (interiorizaDisyuncion (Impl r s)))

-- Función que regresa una fórmula equivalente donde las conjunciones
-- sólo se aplica a conjunciones o literales.
-- Puedes suponer que la fórmula que recibes está en FNN.
interiorizaConjuncion :: Prop -> Prop
interiorizaConjuncion = error "D:"

---------------------------------------------------------------------------------
--------                           AUXILIARES                            --------
---------------------------------------------------------------------------------

-- Función que calcula el conjunto potencia.
subconj :: [a] -> [[a]]
subconj [] = [[]]
subconj (x:xs) = [(x:z) | z <-(subconj xs) ] ++ subconj xs

---------------------------------------------------------------------------------
--------                             EJEMPLOS                            --------
---------------------------------------------------------------------------------

--variables
p = Var "p"
q = Var "q"
r = Var "r"

-- tautología 1
taut1 = (p \/ (¬) p)
-- tautología 2
taut2 = ((p \/ q) \/ ((¬)p /\ (¬) q))
-- contraducción 1
cont1 = ((p \/ q) /\ ((¬)p /\ (¬) q))
-- contradicción 2
cont2 = (p /\ (¬) p)
-- formula 1
form1 = ((p \/ q) /\ (((¬) q) \/ r))

vars1 = vars form1
-- Regresa: ["p","q","r"]

vars2 = vars taut1
-- Regresa: ["p"]

interp1 = interp ["r", "p"] form1
-- Regresa: True

interp2 = interp ["r"] form1
-- Regresa: False

numConectivos1 = numConectivos taut2
-- Regresa: 5

numConectivos2 = numConectivos form1
-- Regresa: 4

elimEquiv1 = elimEquiv (p <--> q)
-- Regresa: (("p" → "q") ^ ("q" → "p"))

elimImpl1 = elimImpl $ elimEquiv (p <--> q)
-- Regresa: ((¬ "p" ∨ "q") ^ (¬ "q" ∨ "p"))

elimImpl2 = elimImpl (p --> (q \/ r))
-- Regresa: (¬ "p" ∨ ("q" ∨ "r"))

estados1 = estados form1
-- Regresa: [["p","q","r"],["p","q"],["p","r"],["p"],["q","r"],["q"],["r"],[]]

estados2 = estados cont1
-- Regresa: [["p","q"],["p"],["q"],[]]

modelos1 = modelos form1
-- Regresa: [["p","q","r"],["p","r"],["p"],["q","r"]]

modelos2 = modelos cont1
-- Regresa: []

tautologia1 = tautologia taut1
-- Regresa: True

tautologia2 = tautologia cont1
-- Regresa: False

satisfen1 = satisfen ["p"] taut1
-- Regresa: True

satisfen2 = satisfen ["p"] cont1
-- Regresa: False

satisfacible1 = satisfacible form1
-- Regresa: True

satisfacible2 = satisfacible cont1
-- Regresa: False

insatisfen1 = insatisfen ["p"] taut1
-- Regresa: False

insatisfen2 = insatisfen ["p"] cont1
-- Regresa: True

contrad1 = contrad taut2
-- Regresa: False

contrad2 = contrad cont2
-- Regresa: True

meteNegacion1 = meteNegacion (Neg (Neg (Var "P")))
-- Regresa: P

meteNegacion2 = meteNegacion (Neg ((Var "P") /\ (Var "Q")))
meteNegacion21 = meteNegacion (Neg (Conj (Var "P") (Var "Q")))
-- Regresa(n): (¬ "P" ∨ ¬ "Q")

interiorizaDisyuncion1 = interiorizaDisyuncion (Disy (Var "P") (Conj (Var "Q") (Var "R")))
-- Regresa: (("P" ∨ "Q") ^ ("P" ∨ "R"))

interiorizaDisyuncion2 = interiorizaDisyuncion (Disy (Conj (Var "P") (Var "Q")) (Var "R"))
-- Regresa: (("P" ∨ "R") ^ ("Q" ∨ "R"))

--interiorizaConjuncion1 = interiorizaConjuncion (Conj (Var "P") (Disy (Var "Q") (Var "R")))
-- Regresa: (("P" ^ "Q") ∨ ("P" ^ "R"))

--interiorizaConjuncion2 = interiorizaConjuncion (Conj (Disy (Var "P") (Var "Q")) (Var "R"))
-- Regresa: (("P" ^ "R") ∨ ("Q" ^ "R"))

--subconj1 = subconj [1,2,3]
-- Regresa: [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]
