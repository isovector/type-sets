{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RoleAnnotations       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wall              #-}

module Type.Set.VariantF
  ( -- * Core types
    VariantF (..)
  , HasF (..)

    -- * Decomposition proofs
  , decompRootF
  , SplitF (..)

    -- * Membership Proofs
  , ForAllIn(..)

    -- * Weakening
  , weakenF

    -- * Internal stuff
  , proveFollowInsertF
  ) where

import Data.Type.Equality
import Type.Set.Variant
import Data.Kind
import Data.Functor.Classes
import Data.Typeable
import Data.Constraint
import Type.Set
import Unsafe.Coerce


-- proveFollowStop :: forall ss a l r .  Follow '[] (Branch a l r) :~~: a
-- proveFollowStop = unsafeCoerce HRefl

proveFollowLeft :: forall ss a l r ss'. (ss ~ ('L ': ss'))
  => Follow ss ('Branch a l r) :~~: Follow ss' l
proveFollowLeft = unsafeCoerce HRefl

proveFollowRight :: forall ss a l r ss'. (ss ~ ('R ': ss'))
  => Follow ss ('Branch a l r) :~~: Follow ss' r
proveFollowRight = unsafeCoerce HRefl

-- | A proof that `ForAllIn c bst` implies `c (Follow ss bst)`.
--   Given an `s :: SSide ss` use `forMember @ss @c @bst s` to get a
--   `Dict (c (Follow ss bst))`.
class ForAllIn (c :: k -> Constraint) (bst :: TypeSet k) where
  forMember :: (Follow ss bst ~ f) =>
    SSide ss -> Dict (c (Follow ss bst))

instance (c a, ForAllIn c l, ForAllIn c r
         ) => ForAllIn (c :: k -> Constraint) ('Branch (a :: k) (l :: TypeSet k) (r :: TypeSet k)) where

  -- forMember :: forall (ss :: [Side]). SSide ss -> Dict (c (Follow ss ('Branch a l r)))
  forMember SNil = (Dict :: Dict (c a))
  forMember (SL ss)
    = case proveFollowLeft of
        HRefl -> forMember @_ @c @l ss-- :: Dict (c (Follow ss l)))
  forMember (SR ss)
    = case proveFollowRight of
        HRefl -> forMember @_ @c @r ss-- :: Dict (c (Follow ss r)))

instance () => ForAllIn (c :: k -> Constraint) 'Empty where
  forMember = error "Somehow got invalid path into TypeSet"


type family ForAll (c :: k -> Constraint) (bst :: TypeSet k) :: Constraint

------------------------------------------------------------------------------
-- | A 'VariantF' is a higher-order version of 'Variant' which can contain
--   any of the 'Functor's within its 'TypeSet'. You can use 'toVariantF' to
--   construct one, and 'fromVariantF' to pattern match it out.
data VariantF (bst :: TypeSet (* -> *)) (a :: *) where
  VariantF :: ()
    => SSide ss -> Follow ss bst a -> VariantF bst a
type role VariantF nominal nominal


------------------------------------------------------------------------------
-- | A proof that the set @bst@ contains functor @f@.
class HasF f bst where

  -- | Inject a @t@ into a 'VariantF'.
  toVariantF :: f a -> VariantF bst a

  -- | Attempt to project a 'VariantF' into @f a@. This might fail, because there
  -- is no guarantee that the 'VariantF' /actually contains/ @f a@.
  fromVariantF :: VariantF bst a -> Maybe (f a)

instance ( Follow (Locate f bst) bst ~ f
         , FromSides (Locate f bst)
         ) => HasF f bst where
  toVariantF = VariantF (fromSides @(Locate f bst))
  fromVariantF (VariantF tag res) =
    case testEquality tag (fromSides @(Locate f bst)) of
      Just Refl -> Just res
      Nothing -> Nothing

-- forall c f bst. (ForAll c bst, HasF f bst) => c f

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
-- | A proof that inserting into a @bst@ doesn't affect the position of
-- anything already in the tree.
proveFollowInsertF :: forall (ss :: [Side]) (f :: * -> *) (bst :: TypeSet (* -> *)).
  Follow ss (Insert f bst) :~~: Follow ss bst
proveFollowInsertF = unsafeCoerce HRefl



------------------------------------------------------------------------------
-- | Weaken a 'VariantF' so that it can contain something else.
weakenF :: forall f bst a. VariantF bst a -> VariantF (Insert f bst) a
weakenF (VariantF (tag :: SSide ss) res)
  = VariantF (tag :: SSide ss) $
    case proveFollowInsertF @ss @f @bst of
      HRefl -> res :: Follow ss bst a


instance (ForAllIn Functor bst) => Functor (VariantF bst) where
  fmap f (VariantF s r) = case forMember @_ @Functor @bst s of
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

data SomeType = SomeType

-- | I have no good way to ensure that a is typeable so the output replaces
--   the parameter type with '\'?\''
instance ( ForAllIn Show1 bst
         , ForAllIn Typeable bst
         , ForAllIn Functor bst
         ) => Show1 (VariantF bst) where

  liftShowsPrec :: forall a. (Int -> a -> ShowS)
    -> ([a] -> ShowS) -> Int -> VariantF bst a -> ShowS
  liftShowsPrec prec lPrec d (VariantF s r)
    = case forMember @_  @Show1 @bst s of
        Dict -> case forMember @_ @Typeable @bst s of
          Dict -> case forMember @_ @Functor @bst s of
            Dict -> showParen (d > 5) $
              (showString "toVariantF @(" :: ShowS) .
              showsTypeRep (typeOf (const SomeType <$> r)) .
              showString ") $ " .
              liftShowsPrec prec lPrec (d+1) r

instance ( ForAllIn Show1 bst
         , ForAllIn Typeable bst
         , ForAllIn Functor bst
         , Show a) => Show (VariantF bst a) where
  showsPrec = showsPrec1
