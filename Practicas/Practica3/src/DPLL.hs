module DPLL where
import LProp
import Data.List
-- Importen los módulos necesarios de las prácticas anteriores para tener sus definiciones
-- de lógica proposicional

type Literal = Prop
type Clausula = [Literal]
type Formula = [Clausula]
type Modelo = [Literal]
type Configuracion = (Modelo,Formula)

--------------------------------------  BÚSQUEDA DE MÓDELOS HACIA ATRAS ------------------------------

unit :: Configuracion -> Configuracion
unit (a,[])  =  (a,[])
unit ([], xs)= (head$unitM xs, (delete (head$unitM$ xs) xs))
unit (a, xs) = ((head a):(head$unitM$ xs) , delete(head$unitM xs) xs )

elim :: Configuracion -> Configuracion
elim ([], f) = ([],f)
elim (x, []) = (x,[])
elim (x,f)   = (x, fil(x,f))
  where fil (_,[])                     = []
        fil(x,xs)
          | notElem (head x) (head xs) = (head xs): fil (x,tail xs)
          | otherwise                  = fil (x,tail xs)

red :: Configuracion -> Configuracion
red ([], f) = ([],f)
red (m, []) = (m,[])
red (m,f)   = (m,red2 (m,f))
  where red2 (x, [])                         = []
        red2 (x, xs)
         | notElem  (Neg (head x)) (head xs) = head xs: red2 (x,tail xs)
         | otherwise                         = delete (Neg (head x))(head xs): red2 (x, tail xs)

split :: Configuracion -> [Configuracion]
split (m,[]) = [(m,[])]
split (_,f)  = [([negP$head(head f)], f), ([(head$head(f))],f)]

conflict :: Configuracion -> Bool
conflict (m,f) = elem [] f

success :: Configuracion -> Bool
success (m,f) = f==[]

----------------------------------------------- ARBOLES DPLL -------------------------------------

rcu :: Formula -> Formula
rcu [] = []
rcu f  = rcuU (head(head(unitM f))) (delete (head(unitM f)) f)
  where rcuU x []                                      = []
        rcuU x xs
          | elem x (head xs) || elem (negP x) (head xs) = (delete' x (head xs)): (rcuU x (tail xs))
          | otherwise                                   = (head xs): (rcuU x (tail xs))

rlp :: Formula -> Formula
rlp []  = []
rlp f   = rlpP (head(dD$dN$(allP f))) f
  where rlpP x []           = []
        rlpP x f
          | elem x (head f) = rlpP x (tail f)
          | otherwise       = head f: rlpP x (tail f)

rd :: Formula ->(Formula,Formula)
rd [] = ([],[])
rd f  = ((rama' (head(dL$allP f)) (rama(head(dL$allP f))  f),
          (rama' (negP(head(dL$allPP f)))) (rama(negP(head(dL$allPP f))) f)) )

data ArbolDPLL = 
                 Void
               | Uni Formula ArbolDPLL
               | Bi Formula ArbolDPLL ArbolDPLL
               deriving (Eq, Show)

dplltree :: Formula -> ArbolDPLL
dplltree [] = Void
dplltree f  = Uni (rcu f) (Bi (rlp (rcu f)) (dplltree (fst (rd (rlp (rcu f)))))
                           (dplltree(snd (rd(rlp (rcu f))))))


--------------------------------------FUNCIONES AUX-------------------------------------------------

-- Función que regresa la lista de los elementos que tengan longitud 1
unitM [] = []
unitM xs = filter ((==1). length) xs

-- Función que regresa la negación
negP:: Prop -> Prop
negP (P x) = Neg(P x)
negP (Neg (P x)) = (P x)

-- Función que elimina la literal (x) y la literal complementaria
delete' x [] = []
delete' x xs = delete x(delete (negP x) xs)

--Función regresa todas las literales de las clausulas de la fórmula en una sola clausula
allP:: Formula -> Clausula
allP [] = []
allP xs = (head xs ++ allP (tail xs))

--
rama ::Prop-> Formula ->Formula
rama x []           = []
rama p f
  | elem p (head f) = rama p  (tail f)
  |otherwise        = (head f) : (rama p (tail f))

rama':: Prop-> Formula->Formula
rama' x []                 = []
rama' x f
  | elem (negP x) (head f) = (delete (negP x) (head f)): rama' x (tail f)
  | otherwise              = head f: (rama' x (tail f))

allPP:: Formula -> Clausula
allPP [] = []
allPP xs = (head xs ++ allPP (tail xs))

dD []     = []
dD (x:xs) = (x:  (filter (/=x) xs))

-- Función regresa la clausua con la literal pura
dN []                = []
dN [x]               = [x]
dN (x:xs)
  | elem (negP x) xs = dN (delete' x xs)
  | otherwise        =x:dN (tail xs)
-- Función que regresa la clausula que tiene comp
dL []                 = []
dL [x]                = [x]
dL (x:xs)
  | elem (negP x) xs  = x: dL xs
  |otherwise          = dL (tail xs)

---------------------------------------PRUEBAS------------------------------------------------------

unit1 = unit ([],[[P 'p',P 'q'],[P 'p'],[Neg(P 'r')],[P 'r',P 'q']])
-- R: (['p'],[['p','q'],[Neg 'r'],['r','q']])
elim1 = elim ([P 'p'],[[P 'p', P 'q'],[P 'r'],[P 'r',Neg(P 'p')],[P 'r', P 's',P 'p']])
-- R: (['p'],[['r'],['r',Neg 'p']])
red1 = red ([P 'p'],[[P 'r'],[P 'q',P 'r', Neg (P 'p')], [P 'r', P 's', P 'q']])
-- R: (['p'],[['r'],['q','r'],['r','s','q']])
split1 = split([],[[P 'r'],[P 'q', P 'r', Neg (P 'p')], [P 'r', P 's', P 'q']])
--R: [([¬ 'r'],[['r'],['q','r',¬ 'p'],['r','s','q']]),(['r'],[['r'],['q','r',¬ 'p'],['r','s','q']])]
conflict1 = conflict ([P 'p', P 'r', Neg(P 'q')], [[]])
-- R: True
success1 = success ([P 'p', P 'r', Neg (P 'q')], [])
-- R: True
rcu1 = rcu [[P 'p', P 'q',Neg (P 'r')],[P 'p', Neg(P 'q')], [Neg (P 'p')], [P 'r']]
--R: [['q',¬ 'r'],[¬ 'q'],['r']]
rlp1 = rlp [[P 'p', P 'q'],[P 'q', P 'r'], [Neg(P 'q'), P 'p'], [P 'r', Neg (P 'q')]]
-- R: [['q','r'],['r',¬ 'q']]
rd1 = ([[Neg (P 'q'), P 'r'], [Neg(P 'r'), P 'q'], [Neg (P 'q'), Neg (P 'r')]])
--([[¬ 'r']],[['r'],[¬ 'r']])
