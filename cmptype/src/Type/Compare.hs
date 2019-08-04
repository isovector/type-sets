{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Type.Compare where

import GHC.TypeLits

------------------------------------------------------------------------------
-- | Compare two types. For 'Nat's and 'Symbol's, this uses the built-in
-- comparisons. For all other types, "Type.Compare.Plugin" will solve it.
type family CmpType (a :: k) (b :: k) :: Ordering where
  CmpType a             a = 'EQ
  CmpType (a :: Nat)    b = CmpNat a b
  CmpType (a :: Symbol) b = CmpSymbol a b

