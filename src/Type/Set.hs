{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Type.Set
  ( TypeSet (..)
  , Side (..)
  , Insert
  , Member
  , Remove
  , Merge
  , Locate
  ) where

import Type.Compare
import GHC.TypeLits


------------------------------------------------------------------------------
-- |
data TypeSet a
  = Empty
  | Branch a (TypeSet a) (TypeSet a)


------------------------------------------------------------------------------
-- |
data Side = L | R
  deriving (Eq, Ord, Show)

------------------------------------------------------------------------------
-- |
type family Insert (t :: k) (bst :: TypeSet k)  :: TypeSet k where
  Insert t 'Empty = 'Branch t 'Empty 'Empty
  Insert t ('Branch a lbst rbst) =
    InsertImpl (CmpType t a) t a lbst rbst

type family InsertImpl (ord :: Ordering)
                       (t :: k)
                       (a :: k)
                       (lbst :: TypeSet k)
                       (rbst :: TypeSet k) :: TypeSet k where
  InsertImpl 'EQ t a lbst rbst = 'Branch a lbst rbst
  InsertImpl 'LT t a lbst rbst = 'Branch a (Insert t lbst) rbst
  InsertImpl 'GT t a lbst rbst = 'Branch a lbst (Insert t rbst)


------------------------------------------------------------------------------
-- |
type family Member (t :: k) (bst :: TypeSet k)  :: Bool where
  Member t 'Empty = 'False
  Member t ('Branch a lbst rbst) = MemberImpl (CmpType t a) t lbst rbst

type family MemberImpl (ord :: Ordering)
                       (t :: k)
                       (lbst :: TypeSet k)
                       (rbst :: TypeSet k) :: Bool where
  MemberImpl 'EQ t lbst rbst = 'True
  MemberImpl 'LT t lbst rbst = Member t lbst
  MemberImpl 'GT t lbst rbst = Member t rbst


------------------------------------------------------------------------------
-- |
type family Merge (small :: TypeSet k) (big :: TypeSet k) :: TypeSet k where
  Merge Empty big   = big
  Merge small Empty = small
  Merge ('Branch a lbst rbst) big = Merge rbst (Merge lbst (Insert a big))


------------------------------------------------------------------------------
-- |
type family Remove (t :: k) (bst :: TypeSet k) :: TypeSet k where
  Remove t Empty = Empty
  Remove t ('Branch a lbst rbst) = RemoveImpl (CmpType t a) t a lbst rbst

type family RemoveImpl (ord :: Ordering)
                       (t :: k)
                       (a :: k)
                       (lbst :: TypeSet k)
                       (rbst :: TypeSet k) :: TypeSet k where
  RemoveImpl 'LT t a lbst rbst = 'Branch a (Remove t lbst) rbst
  RemoveImpl 'EQ t a Empty rbst = rbst
  RemoveImpl 'EQ t a lbst Empty = lbst
  RemoveImpl 'EQ t a lbst rbst =
    'Branch (RightMost lbst) (Remove (RightMost lbst) lbst) rbst
  RemoveImpl 'GT t a lbst rbst = 'Branch a lbst (Remove t rbst)


------------------------------------------------------------------------------
-- |
type family RightMost (bst :: TypeSet k) :: k where
  RightMost ('Branch a lbst 'Empty) = a
  RightMost ('Branch a lbst rbst) = RightMost rbst


------------------------------------------------------------------------------
-- |
type family Locate (t :: k) (bst :: TypeSet k) :: [Side] where
  Member t ('Branch a lbst rbst) = LocateImpl (CmpType t a) t lbst rbst
  Member t 'Empty = TypeError ('Text "Unable to find" ':<>: 'ShowType t)

type family LocateImpl (ord :: Ordering)
                       (t :: k)
                       (lbst :: TypeSet k)
                       (rbst :: TypeSet k) :: [Side] where
  LocateImpl 'EQ t lbst rbst = '[]
  LocateImpl 'LT t lbst rbst = 'L ': Locate t lbst
  LocateImpl 'GT t lbst rbst = 'R ': Locate t rbst

