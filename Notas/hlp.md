# Document Title

Lógica Proposicional 

- Símbolos o Variables proposicionales $p, q,r,...$
- Constantes lógicas 
- Conectivos  $\wedge, \vee, \rightarrow$
- Símbolos Auxiliares

Lenguaje **PROP** 

$\phi\ci\VarP |\BOTO |\top|\neg\phi|\phi\estrellita\ci$

$\estrellita ::= \wedge|\vee|\rightarrow|\Rightleftarrow$

$VarP::= P_1|P_2|P_3 ...$

Problema 1

Operadores ambiguas

Conj y Disyuncion asocian izq


Definiciones Recursivas 

La lista vacia es una lista [] -> CASO BASE

Si x:A y xs es una lista de A entonces (x:xs) es una lista -> 

Son todas  -> CERRADURA

PPROP -> GENERA UN PRINCIPIO DE INDUCCIÓN


### Principio de Inducción de PROP

- Caso base  Demostrar P para $\bot\top$ y $VarP$
- Hipótesis de Inducción  Suponer que $P$ se cumple para $\phi\psi$
- Paso Inductivo 

### Sustitución 

Función recursiva (Aplica sustitución para cada uno de los casos de la gramática )

## Semántica

NO es lo mismo tener el mismo vallor que ser iguales 

Tautología $I(\phi) = 1 \forall I$

Contradicción $$

Satisfacible Existe I tal que I(\phi)=1

I es modelo para $\phi$

Insatisfacible 
