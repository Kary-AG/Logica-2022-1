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
-- instance Show Prop where

-- instance Operadores -- TODO
-- instance Operadores Prop where

---------------------------------------------------------------------------------
--------                             FUNCIONES                           --------
---------------------------------------------------------------------------------

-- Función que recibe una fórmula y devuelve el conjunto (no hay
-- repeticiones) de variables que hay en una fórmula.
vars :: Prop -> [VarP]
vars = error "D:"

-- Funcion que evalua una proposicion dado un estado.
interp :: Estado -> Prop -> Bool
interp = error "D:"

-- Función que cuenta el número de conectivos.
numConectivos :: Prop -> Int
numConectivos = error "D:"

-- Funcion que elimina las equivalencias (<->).
elimEquiv :: Prop -> Prop
elimEquiv = error "D:"

-- Funcion que elimina las implicaciones, puedes suponer que no hay
-- equivalencias.
elimImpl :: Prop -> Prop
elimImpl = error "D:"

-- Función que dada una fórmula φ con n-variables devuelve la lista
-- con 2^{n} estados distintos para φ.
estados :: Prop -> [Estado]
estados = error "D:"

-- Funcion que nos da TODOS los modelos de una proposicion.
modelos :: Prop -> [Estado]
modelos = error "D:"

-- Funcion que nos dice si una proposicion es una tautologia.
tautologia :: Prop -> Bool
tautologia = error "D:"

-- Función que nos dice si una proposición es satisfacible en una
-- interpretación.
satisfen :: Estado -> Prop -> Bool
satisfen = error "D:"

-- Funcion que nos dice si una proposicion es satifacible.
satisfacible :: Prop -> Bool
satisfacible = error "D:"

-- Función que nos dice si una proposición es insatisfacible en una
-- interpretación.
insatisfen :: Estado -> Prop -> Bool
insatisfen = error "D:"

-- Funcion que nos dice si una proposicion es instisfacible.
contrad :: Prop -> Bool
contrad = error "D:"

-- Función que regresa una fórmula equivalente donde las negaciones
-- solo se aplican a fórmulas atómicas.
meteNegacion :: Prop -> Prop
meteNegacion = error "D:"

-- Función que regresa una fórmula equivalente donde las disyunciones
-- sólo se aplica a disyunciones o literales.
-- Puedes suponer que la fórmula que recibes está en FNN.
interiorizaDisyuncion :: Prop -> Prop
interiorizaDisyuncion = error "D:"

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
subconj = error "D:"

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

interiorizaConjuncion1 = interiorizaConjuncion (Conj (Var "P") (Disy (Var "Q") (Var "R")))
-- Regresa: (("P" ^ "Q") ∨ ("P" ^ "R"))

interiorizaConjuncion2 = interiorizaConjuncion (Conj (Disy (Var "P") (Var "Q")) (Var "R"))
-- Regresa: (("P" ^ "R") ∨ ("Q" ^ "R"))

subconj1 = subconj [1,2,3]
-- Regresa: [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]
