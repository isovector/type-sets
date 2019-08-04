{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module CmpType where

import GHC.TypeLits

type family CmpType (a :: k) (b :: k) :: Ordering where
  CmpType a a = 'EQ
  -- CmpType (a :: Symbol) b = CmpSymbol a b
  -- CmpType (a :: Nat) b    = CmpNat a b

