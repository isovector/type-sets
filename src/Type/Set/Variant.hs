{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE AutoDeriveTypeable    #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RoleAnnotations       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
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
  , ForAllIn(..)

    -- * Weakening
  , weaken

    -- * Internal stuff
  , proveFollowInsert
  , toSideList
  , SSide (..)
  , FromSides (..)
  ) where

import Data.Type.Equality
import Type.Set
import Data.Kind
import Data.Constraint
import Type.Reflection
import Unsafe.Coerce

------------------------------------------------------------------------------
-- | A proof that `ForAllIn c bst` implies `c (Follow ss bst)`.
--   Given an `s :: SSide ss` use `forMember @ss @c @bst s` to get a
--   `Dict (c (Follow ss bst))`.
class ForAllIn (c :: k -> Constraint) (bst :: TypeSet k) where
  forMember :: (Follow ss bst ~ f) =>
    SSide ss -> Dict (c (Follow ss bst))

instance (c a, ForAllIn c l, ForAllIn c r
         ) => ForAllIn (c :: k -> Constraint) ('Branch (a :: k) (l :: TypeSet k) (r :: TypeSet k)) where

  forMember SNil = (Dict :: Dict (c a))
  forMember (SL ss)
    = case (unsafeCoerce HRefl :: Follow ('L ': ss) ('Branch a l r) :~~: Follow ss l) of
        HRefl -> forMember @_ @c @l ss
  forMember (SR ss)
    = case (unsafeCoerce HRefl :: Follow ('R ': ss) ('Branch a l r) :~~: Follow ss r) of
        HRefl -> forMember @_ @c @r ss

instance () => ForAllIn (c :: k -> Constraint) 'Empty where
  forMember = error "Somehow got invalid path into TypeSet"

------------------------------------------------------------------------------
-- | Singletons for 'Side's.
data SSide (ss :: [Side]) where
  SNil :: SSide '[]
  SL :: SSide ss -> SSide ('L ': ss)
  SR :: SSide ss -> SSide ('R ': ss)

deriving instance Typeable (SSide ss)

------------------------------------------------------------------------------
-- | Get the type level version of a path into a BST.
toSideList :: SSide ss -> [Side]
toSideList SNil = []
toSideList (SL s) = L : toSideList s
toSideList (SR s) = R : toSideList s

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
proveFollowInsert :: Follow ss (Insert t bst) :~~: Follow ss bst
proveFollowInsert = unsafeCoerce HRefl

------------------------------------------------------------------------------
-- | Weaken a 'Variant' so that it can contain something else.
weaken :: forall t bst. Variant bst -> Variant (Insert t bst)
weaken (Variant (tag :: SSide ss) res) = Variant tag $
  case proveFollowInsert @ss @t @bst of
    HRefl -> res

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

instance (ForAllIn Eq bst, ForAllIn Typeable bst) => Eq (Variant bst) where
  (Variant s r) == (Variant s' r')
    = case forMember @_ @Typeable @bst s of
      Dict -> case forMember @_ @Typeable @bst s' of
        Dict -> case eqTypeRep (typeOf r) (typeOf r') of
          Nothing -> False
          Just HRefl -> case forMember @_ @Eq @bst s of
            Dict -> r == r'

instance (ForAllIn Eq bst
         , ForAllIn Ord bst
         , ForAllIn Typeable bst
         ) => Ord (Variant bst) where
  compare (Variant s r) (Variant s' r')
    = case forMember @_ @Typeable @bst s of
      Dict -> case forMember @_ @Typeable @bst s' of
        Dict -> case eqTypeRep (typeOf r) (typeOf r') of
          Nothing -> compare (toSideList s) (toSideList s')
          Just HRefl -> case forMember @_ @Ord @bst s of
            Dict -> compare r r'

instance (ForAllIn Show bst
         ) => Show (Variant bst) where
  showsPrec i (Variant s r)
    = case forMember @_ @Show @bst s of
      Dict -> showParen (i > 5) $
        showString "toVariant $ " . showsPrec 1 r
