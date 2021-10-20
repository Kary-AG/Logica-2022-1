{-
-- | Lógica Computacional 2022-01
-- | Práctica 2: Forma normal negativa y conjuntiva
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
-}

module Practica2 where

import LProp
import Data.List

-- |1| Funcion fnn que recibe una formula φ y devuelve su forma normal negativa
fnn :: Prop -> Prop
fnn  Top       = Top
fnn Bot        = Bot
fnn (P x)      =  (P x)
fnn (Neg (P x))= (Neg (P x))
fnn (Neg x)    = fnn$ neg$elimImp$elimEquiv (Neg x)
fnn (Or x y)   = (Or (fnn$ elimImp$elimEquiv x) (fnn$ elimImp$elimEquiv y))
fnn (And x y)  = (And (fnn$ elimImp$elimEquiv x) (fnn$ elimImp$elimEquiv y))
fnn (Impl x y) = fnn$ elimImp$elimEquiv (Impl x y)
fnn (Syss x y) = fnn$ elimImp$elimEquiv(Syss x y)

-- |2| Funcion distr la cual aplica adecuadamente las leyes distributivas a una formula proposicional
distr :: Prop -> Prop
distr (Or (And x y) z) = distr(And ( Or (distr x)  (distr z)) (Or (distr y) (distr z)))
distr (Or z (And x y)) = distr(And (Or (distr z) (distr x)) (Or (distr z)  (distr y)))
distr (And x y)        = And (distr x)  (distr y)
distr x                = x
-- |3| Funcion fnc que recibe una formula φ y devuelve su forma normal conjuntiva, es decir:
--     Permite expresar cualquier formula proposicional como una conjunción de disyunciones.
-- IMPORTANTE: Se puede suponer que la formula de entrada ya se encuentra en forma normal negativa
fnc :: Prop -> Prop
fnc (P x)       = (P x)
fnc (Neg (P x)) = (Neg (P x))
fnc (And x y)   = (And (fnc x) (fnc y))
fnc (Or x y)    = distr$ Or(fnc x) (fnc y)

--Definimos un tipo de dato Literal como sinónimo de fórmula proposicional
type Literal = Prop

--Definimos un tipo de dato Clausula como una lista de literales
type Clausula = [Literal]

-- |4| Función ctolist recibe una proposicion y devuelve la forma clausular de la proposicion
ctolist :: Prop -> Clausula
ctolist (P x)       = [(P x)]
ctolist (Neg (P x)) = [(Neg(P x))]
ctolist (And x y)   = union (ctolist x)  (ctolist y)
ctolist (Or x y)    = union (ctolist x)  (ctolist y)
ctolist (Impl x y)  = [(Impl x y)]
ctolist (Syss x y)  = [(Syss x y)]

-- |5| Función fncC recibe una fórmula en forma normal conjuntiva y debe devolver su conversión en una lista de cláusulas.
fncC :: Prop -> [Clausula]
fncC (And x y)  = ctolist x:[ctolist y]
fncC (Or x y)   = ctolist x:[ctolist y]
fncC (Impl x y) = [ctolist (Impl x y)]
fncC (Syss x y) = [ctolist (Syss x y)]


-- |6| Funcion fncConj recibe una proposicion y devuelve el conjunto de clausulas equivalentes a esa proposicion
fncConj :: Prop -> [Clausula]
fncConj (Or (P x) (P y))              = [ctolist (Or (P x) (P y))]
fncConj (Or (Neg (P x)) (P y))        = [ctolist (Or (Neg(P x)) (P y))]
fncConj (Or (P x) (Neg(P y)))         = [ctolist (Or (P x) (Neg(P y)))]
fncConj (Or (Neg (P x)) (Neg (P y)))  = [ctolist (Or (Neg (P x)) (Neg (P y)))]
fncConj (Or x y)                      = [ctolist (Or x y)]
fncConj (And x y)                     = fncConj x ++ fncConj y
fncConj x                             = fncConj$fnc$fnn x

{--
PUNTOS EXTRA
--}
-- |1| Función argcorrecto que verifica si un argumento con premisas y una conclusión es lógicamente correcto
argcorrecto :: [Prop] -> Prop -> Bool
argcorrecto xs x = consecuencia xs x

{--
FUNCIONES AUX
-}
-- [1'] Función que regresa una fórmula equivalente donde las negaciones solo se aplican a fórmulas atómicas.
neg :: Prop -> Prop
neg (Neg (P x))    = (Neg (P x))
neg (Neg (Neg x))  = elimImp$elimEquiv x
neg (Neg(And x y)) = (Or (neg (Neg (elimImp$elimEquiv x))) (neg (Neg (elimImp$elimEquiv y))))
neg (Neg(Or x y))  = (And (neg (Neg (elimImp$elimEquiv x))) (neg (Neg (elimImp$elimEquiv y))))

satisfenConj :: Estado -> [Prop]-> Bool
satisfenConj i []     = True
satisfenConj i (x:xs) = and ((satisfen i x):[satisfenConj i xs])

estadosConj :: [Prop] -> [Estado]
estadosConj []     =  []
estadosConj (x:xs) =  estados x ++ estadosConj xs

consecuencia gamma phi = null [i | i <- estadosConj(phi:gamma),
                               satisfenConj i gamma,
                               not (satisfen i phi)]
{-
EJEMPLOS
-}

fnn1 = fnn$ Syss (P 'p') (P 'q')
-- Devuelve: (¬ 'p' v 'q') ∧ (¬ 'q' v 'p'))
fnn2 = fnn$ Impl (Or (P 'p') (P 'q')) (P 'p')
-- Devuelve: ((¬ 'p' ∧ ¬ 'q') v 'p')
distr1 = distr (Or (And (P 'p') (P 'q')) (P 'r'))
-- Devuelve: (('p' v 'r') ∧ ('q' v 'r'))
cto1 = ctolist (Neg (P 'p'))
-- Devuelve: [¬ 'p']
cto2 = ctolist (Or (Or (Neg (P 'p')) (P 'r')) (Or (Neg (P 'p')) (P 'q')))
-- Devuelve: [¬ 'p','r','q']
cto3 = ctolist (Impl (P 'p') (P 'q'))
-- Devuelve: [('p' > 'q')]
fncC1 = fncC (And (P 'p') (Or (Neg (P 'q')) (P 'r')))
-- Devuelve: [['p'],[¬ 'q','r']]
fncC2 = fncC (And (Or (Neg (P 'p')) (P 'q')) (Or (Neg (P 'p')) (Neg (P 'r'))))
-- Devuelve: [[¬ 'p','q'],[¬ 'p',¬ 'r']]
fncC3 = fncC (Syss (P 'p') (P 'r'))
-- Devuelve: [[('p' <> 'r')]]
fncConj1 = fncConj (Neg (Syss (P 'p') (P 'q')))
-- Devuelve: [['p','q'],['p',¬ 'p'],[¬ 'q','q'],[¬ 'q',¬ 'p']]
fncConj2 = fncConj (Or(Or (P 'p') (P 'q')) (Or(Neg (P 'q')) (P 'r')))
--Devuelve: ['p','q',¬ 'q','r']
argcorrecto1 = argcorrecto [(Impl (Neg (P 'r')) (Neg (P 'i'))), (Or (P 'e') (Neg(Neg (P 'i')))), (Neg (P 'e'))] (P 'r')
-- Devuelve: True
