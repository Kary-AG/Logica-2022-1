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
unit (a, xs) = (head(unitM xs) , delete (head(unitM xs)) xs )

elim :: Configuracion -> Configuracion
elim ([], f) = ([],f)
elim (x, []) = (x,[])
elim (x,f)   = (x, fil(x,f))
  where fil (_,[])   = []
        fil(x,xs)    = if notElem (head x) (head xs)
          then (head xs): fil (x,tail xs)
          else fil (x,tail xs)

red :: Configuracion -> Configuracion
red ([], f) = ([],f)
red (m, []) = (m,[])
red (m,f)   = (m,red2 (m,f))

red2 (x, []) = []
red2 (x, xs) = if notElem (Neg (head x)) (head xs)
  then head xs: red2 (x,tail xs)
  else delete (Neg (head x))(head xs): red2 (x, tail xs)

split :: Configuracion -> [Configuracion]
split (m,[]) = [(m,[])]
split (_,f)  = [([negP(head(head f))], f), ([(head(head(f)))],f)]

conflict :: Configuracion -> Bool
conflict (m,f) = elem [] f

success :: Configuracion -> Bool
success (m,f) = notElem [] f

----------------------------------------------- ARBOLES DPLL -------------------------------------

rcu :: Formula -> Formula
rcu [] = []
rcu f  = rcuU (head(head(unitM f))) (delete (head(unitM f)) f)
  where rcuU x [] = []
        rcuU x xs = if elem x (head xs)|| elem (negP x) (head xs)
          then delete' x (head xs): rcuU x (tail xs)
          else head xs: rcuU x (tail xs)

rlp :: Formula -> Formula
rlp [] = []
rlp f  = rlpP (head (dN(allP f))) f
  where rlpP x []  = []
        rlpP x f = if elem x (head f)
          then rlpP x (tail f)
          else head f: rlpP x (tail f)

rd :: Formula -> (Formula,Formula)
rd [] = ([], [])
--rd f  = (rama (dD f) f, rama (dD f) f )
data ArbolDPLL = 
                 Void
               | Uni Formula ArbolDPLL
               | Bi Formula ArbolDPLL ArbolDPLL
               deriving (Eq, Show)

dplltree :: Formula -> ArbolDPLL
dplltree _ = error "Implementar :)"

--------------------------------------FUNCIONES AUX-------------------------------------------------

-- Función que regresa la lista de los elementos que tengan longitud 1
unitM xs = filter ((==1). length) xs

-- Función que regresa la negación
negP:: Prop -> Prop
negP (P x) = Neg(P x)
negP (Neg (P x)) = (P x)

-- Función que elimina la literal (x) y la literal complementaria
delete' x [] = []
delete' x xs = delete x(delete (negP x) xs)

--Función regresa todas las literales de las clausulas en la fórmula
allP:: Formula -> Clausula
allP [] = []
allP xs = dD (head xs ++ allP (tail xs))
  where dD []     = []
        dD (x:xs) = dN (x: dD (filter (/=x) xs))

-- Función regresa la clausua con la literal pura
dN [] = []
dN [x]    = [x]
dN (x:xs) = if elem (negP x) xs
        then dN (delete' x xs)
        else x:dN (tail xs)
-- Función que regresa la clausula que tiene comp
dD []  = []
dD [x] = [x]
dD (x:xs) = if elem (negP x) xs
  then [x]
  else dD (tail xs)

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
rd1 = (allP [[Neg (P 'q'), P 'r'], [Neg(P 'r'), P 'q'], [Neg (P 'q'), Neg (P 'r')]])
