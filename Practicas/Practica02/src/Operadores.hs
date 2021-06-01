module Operadores where

-- | Operadores. Clase que representa la abstraccion
-- de operadores aplicados a cualquier tipo de dato.
class Operadores a where

  -- | neg. Metodo que representa al operador NOT
  (¬) :: a -> a
  infixl 8 ¬

  -- | conj. Metodo que representa al operador AND.
  (\/) :: a -> a -> a
  infixr 5 \/

  -- | conj. Metodo que representa al operador OR.
  (/\) :: a -> a -> a
  infixr 4 /\

  -- | impl. Metodo que representa al operador IMPLICACIÓN.
  (-->) :: a -> a -> a
  infixr 3 -->

  -- | syss. Metodo que representa al operador EQUIVALENCIA.
  (<-->) :: a -> a -> a
  infixr 2 <-->

