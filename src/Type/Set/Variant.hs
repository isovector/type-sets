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

module Type.Set.Variant
  ( -- * Core types
    Variant (..)
  , Has (..)

    -- * Decomposition proofs
  , decompRoot
  , Split (..)

    -- * Weakening
  , weaken

    -- * Internal stuff
  , proveFollowInsert
  , SSide (..)
  , FromSides (..)
  ) where

import Data.Type.Equality
import Type.Set
import Unsafe.Coerce

------------------------------------------------------------------------------
-- | Singletons for 'Side's.
data SSide (ss :: [Side]) where
  SNil :: SSide '[]
  SL :: SSide ss -> SSide ('L ': ss)
  SR :: SSide ss -> SSide ('R ': ss)

------------------------------------------------------------------------------
-- | Get a singleton for a list of 'Side's.
class FromSides (ss :: [Side]) where
  fromSides :: SSide ss

instance FromSides '[] where
  fromSides = SNil

instance FromSides ss => FromSides ('L ': ss) where
  fromSides = SL fromSides

instance FromSides ss => FromSides ('R ': ss) where
  fromSides = SR fromSides


------------------------------------------------------------------------------
-- | A 'Variant' is like an 'Either', except that it can store any of the types
-- contained in the 'TypeSet'. You can use 'toVariant' to construct one, and
-- 'fromVariant' to pattern match it out.
data Variant (v :: TypeSet *) where
  Variant :: SSide ss -> Follow ss v -> Variant v
type role Variant nominal


------------------------------------------------------------------------------
-- | A proof that the set @bst@ contains type @t@.
class Has t bst where
  -- | Inject a @t@ into a 'Variant'.
  toVariant :: t -> Variant bst
  -- | Attempt to project a 'Variant' into @t@. This might fail, because there
  -- is no guarantee that the 'Variant' /actually contains/ @t@.
  --
  -- You can use 'decompRoot' instead of this function if you'd like a proof
  -- that the 'Variant' doesn't contain @t@ in the case of failure.
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


------------------------------------------------------------------------------
-- | A proof that inserting into a @bst@ doesn't affect the position of
-- anything already in the tree.
proveFollowInsert :: Follow ss (Insert t bst) :~: Follow ss bst
proveFollowInsert = unsafeCoerce Refl


------------------------------------------------------------------------------
-- | Weaken a 'Variant' so that it can contain something else.
weaken :: forall t bst. Variant bst -> Variant (Insert t bst)
weaken (Variant (tag :: SSide ss) res) = Variant tag $
  case proveFollowInsert @ss @t @bst of
    Refl -> res


data Split t lbst rbst
  = Root t
  | LSplit (Variant lbst)
  | RSplit (Variant rbst)


------------------------------------------------------------------------------
-- | Like 'fromVariant', but decomposes the 'Variant' into its left and right
-- branches, depending on where @t@ is.
decompRoot :: Variant ('Branch t lbst rbst) -> Split t lbst rbst
decompRoot (Variant SNil t) = Root t
decompRoot (Variant (SL s) t) = LSplit (Variant s t)
decompRoot (Variant (SR s) t) = RSplit (Variant s t)

