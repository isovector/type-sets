{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wall              #-}

{-# OPTIONS_GHC -fplugin=Type.Compare.Plugin #-}

module Test where

import Lib
import GHC.Exts (Any)
import Unsafe.Coerce


type family IsEQ (a :: Ordering) :: Bool where
  IsEQ 'EQ = 'True
  IsEQ _   = 'False

-- zop :: Proxy 'False
-- zop = Proxy @(IsEQ (CmpType 3 4))

class FromSides (side :: [Side]) where
  fromSides :: [Side]

instance FromSides '[] where
  fromSides = []

instance FromSides xs => FromSides ('L : xs) where
  fromSides = L : fromSides @xs

instance FromSides xs => FromSides ('R : xs) where
  fromSides = R : fromSides @xs


data Variant (v :: BST *) = Variant
  { vTag :: [Side]
  , vResult :: Any
  }


toVariant :: forall ds bst t. (Find t bst ~ ds, FromSides ds) => t -> Variant bst
toVariant t = Variant (fromSides @ds) $ unsafeCoerce t

fromVariant :: forall ds bst t. (Find t bst ~ ds, FromSides ds) => Variant bst -> Maybe t
fromVariant (Variant tag res) =
  if tag == fromSides @ds
     then Just $ unsafeCoerce res
     else Nothing


foo :: Variant (Insert String (Insert Bool (Insert Int 'Empty)))
foo = toVariant True

bar :: Maybe Bool
bar = fromVariant foo

