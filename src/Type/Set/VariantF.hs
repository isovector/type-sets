{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE AutoDeriveTypeable     #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE RoleAnnotations        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wall               #-}

module Type.Set.VariantF
  ( -- * Core types
    VariantF (..)
  , HasF (..)

    -- * Decomposition proofs
  , decompRootF
  , SplitF (..)

    -- * Weakening
  , weakenF
  ) where

import Data.Type.Equality
import Type.Set.Variant
import Data.Functor.Classes
import Data.Constraint
import Type.Reflection
import Type.Set

------------------------------------------------------------------------------
-- | A 'VariantF' is a higher-order version of 'Variant' which can contain
--   any of the 'Functor's within its 'TypeSet'. You can use 'toVariantF' to
--   construct one, and 'fromVariantF' to pattern match it out.
data VariantF (bst :: TypeSet (k -> *)) (a :: k) where
  VariantF :: SSide ss -> Follow ss bst a -> VariantF bst a
type role VariantF nominal nominal

------------------------------------------------------------------------------
-- | A proof that the set @bst@ contains @f :: @.
class HasF (f :: k -> *) (bst :: TypeSet (k -> *)) where

  -- | Inject a @t@ into a 'VariantF'.
  toVariantF :: f a -> VariantF bst a

  -- | Attempt to project a 'VariantF' into @f a@. This might fail, because there
  -- is no guarantee that the 'VariantF' /actually contains/ @f a@.
  fromVariantF :: VariantF bst a -> Maybe (f a)

instance ( Follow (Locate f bst) bst ~ f
         , FromSides (Locate f bst)
         , Typeable (Locate f bst)
         ) => HasF f bst where
  toVariantF = VariantF (fromSides @(Locate f bst))
  fromVariantF (VariantF tag res) =
    case testEquality tag (fromSides @(Locate f bst)) of
      Just Refl -> Just res
      Nothing -> Nothing

------------------------------------------------------------------------------
-- | TODO
data SplitF f lbst rbst a
  = RootF (f a)
  | LSplitF (VariantF lbst a)
  | RSplitF (VariantF rbst a)

------------------------------------------------------------------------------
-- | TODO
decompRootF :: VariantF ('Branch f lbst rbst) a -> SplitF f lbst rbst a
decompRootF (VariantF SNil   t) = RootF t
decompRootF (VariantF (SL s) t) = LSplitF (VariantF s t)
decompRootF (VariantF (SR s) t) = RSplitF (VariantF s t)

------------------------------------------------------------------------------
-- | Weaken a 'VariantF' so that it can contain something else.
weakenF :: forall f bst a. VariantF bst a -> VariantF (Insert f bst) a
weakenF (VariantF (tag :: SSide ss) res)
  = VariantF (tag :: SSide ss) $
    case proveFollowInsert @ss @f @bst of
      HRefl -> res :: Follow ss bst a

instance (ForAllIn Functor bst) => Functor (VariantF bst) where
  fmap f (VariantF s r)
    = case forMember @_ @Functor @bst s of
      Dict -> VariantF s $ fmap f r

instance ( ForAllIn Functor bst
         , ForAllIn Foldable bst
         ) => Foldable (VariantF bst) where
  foldMap f (VariantF s r)
    = case forMember @_ @Foldable @bst s of
        Dict -> foldMap f r

instance ( ForAllIn Functor bst
         , ForAllIn Foldable bst
         , ForAllIn Traversable bst
         ) => Traversable (VariantF bst) where
  traverse f (VariantF s r)
    = case forMember @_ @Traversable @bst s of
        Dict -> VariantF s <$> traverse f r

instance ( ForAllIn Eq1 bst
         , ForAllIn Typeable bst
         ) => Eq1 (VariantF bst) where
  liftEq eq (VariantF (s :: SSide ss) r) (VariantF (s':: SSide ss') r')
    = case forMember @_ @Typeable @bst s of
      Dict -> case forMember @_ @Typeable @bst s' of
        Dict -> case eqTypeRep (typeRep @(Follow ss bst)) (typeRep @(Follow ss' bst)) of
          Nothing -> False
          Just HRefl -> case forMember @_ @Eq1 @bst s of
            Dict -> liftEq eq r r'

instance ( ForAllIn Eq1 bst
         , ForAllIn Typeable bst
         , Eq a
         ) => Eq (VariantF bst a) where
  (==) = eq1

instance ( ForAllIn Eq1 bst
         , ForAllIn Ord1 bst
         , ForAllIn Typeable bst
         ) => Ord1 (VariantF bst) where
  liftCompare cmp (VariantF (s :: SSide ss) r) (VariantF (s':: SSide ss') r')
    = case forMember @_ @Typeable @bst s of
      Dict -> case forMember @_ @Typeable @bst s' of
        Dict -> case eqTypeRep (typeRep @(Follow ss bst)) (typeRep @(Follow ss' bst)) of
          Nothing -> compare (toSideList s) (toSideList s')
          Just HRefl -> case forMember @_ @Ord1 @bst s of
            Dict -> liftCompare cmp r r'


instance ( ForAllIn Eq1 bst
         , ForAllIn Ord1 bst
         , ForAllIn Typeable bst
         , Ord a
         ) => Ord (VariantF bst a) where
  compare = compare1

instance ( ForAllIn Show1 bst
         ) => Show1 (VariantF bst) where

  liftShowsPrec :: forall a. (Int -> a -> ShowS)
    -> ([a] -> ShowS) -> Int -> VariantF bst a -> ShowS
  liftShowsPrec prec lPrec d (VariantF s r)
    = case forMember @_  @Show1 @bst s of
      Dict -> showParen (d > 5) $
        (showString "toVariantF " :: ShowS) .
        showString " $ " .
        liftShowsPrec prec lPrec (d+1) r

instance ( ForAllIn Show1 bst
         , Show a) => Show (VariantF bst a) where
  showsPrec = showsPrec1
