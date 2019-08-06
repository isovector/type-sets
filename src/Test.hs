{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RoleAnnotations       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wall              #-}

{-# OPTIONS_GHC -fplugin=Type.Compare.Plugin #-}

module Test where

import Data.Type.Equality
import Type.Set

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


type Has t bst proof = (Locate t bst ~ proof, Follow proof bst ~ t, FromSides proof)


toVariant :: forall t bst proof. (Has t bst proof) => t -> Variant bst
toVariant t = Variant (fromSides @proof) t

fromVariant :: forall t bst proof. (Has t bst proof) => Variant bst -> Maybe t
fromVariant (Variant tag res) =
  case testEquality tag (fromSides @proof) of
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


foo :: Variant (Insert String (Insert Bool (Insert Int 'Empty)))
foo = toVariant True

bar :: Maybe Bool
bar = fromVariant foo

