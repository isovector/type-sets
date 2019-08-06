{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wall              #-}

{-# OPTIONS_GHC -fplugin=Type.Compare.Plugin #-}

module Type.Set.Variant where
  -- ( Variant (..)
  -- , Has (..)
  -- ) where

import Data.Type.Equality
import Type.Set
import Unsafe.Coerce

data SSide (ss :: [Side]) where
  SNil :: SSide '[]
  SL :: SSide ss -> SSide ('L ': ss)
  SR :: SSide ss -> SSide ('R ': ss)

class FromSides (ss :: [Side]) where
  fromSides :: SSide ss

instance FromSides '[] where
  fromSides = SNil

instance FromSides ss => FromSides ('L ': ss) where
  fromSides = SL fromSides

instance FromSides ss => FromSides ('R ': ss) where
  fromSides = SR fromSides


data Variant (v :: TypeSet *) where
  Variant :: SSide ss -> Follow ss v -> Variant v
type role Variant nominal


class Has t bst where
  toVariant :: t -> Variant bst
  fromVariant :: Variant bst -> Maybe t

instance ( Follow (Locate t bst) bst ~ t
         , FromSides (Locate t bst)
         ) => Has t bst where
  toVariant = Variant (fromSides @(Locate t bst))
  fromVariant (Variant tag res) =
    case testEquality tag (fromSides @(Locate t bst)) of
      Just Refl -> Just res
      Nothing -> Nothing


instance TestEquality SSide where
  testEquality SNil   SNil   = Just Refl
  testEquality (SL a) (SL b) =
    case testEquality a b of
      Just Refl -> Just Refl
      Nothing -> Nothing
  testEquality (SR a) (SR b) =
    case testEquality a b of
      Just Refl -> Just Refl
      Nothing -> Nothing
  testEquality (SL _) SNil   = Nothing
  testEquality SNil   (SL _) = Nothing
  testEquality (SR _) SNil   = Nothing
  testEquality SNil   (SR _) = Nothing
  testEquality (SR _) (SL _) = Nothing
  testEquality (SL _) (SR _) = Nothing


proveFollowInsert :: Follow ss (Insert t bst) :~: Follow ss bst
proveFollowInsert = unsafeCoerce Refl


weaken :: forall t bst. Variant bst -> Variant (Insert t bst)
weaken (Variant (tag :: SSide ss) res) = Variant tag $
  case proveFollowInsert @ss @t @bst of
    Refl -> res


foo :: Variant (Insert String (Insert Bool (Insert Int 'Empty)))
foo = toVariant True

bar :: Maybe Bool
bar = fromVariant foo

