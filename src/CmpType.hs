{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module CmpType where

import GHC.TypeLits

type family CmpType (a :: k) (b :: k) :: Ordering

