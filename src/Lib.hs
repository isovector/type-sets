{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib where

import Type.Compare
import GHC.TypeLits

data BST a
  = Empty
  | Branch a (BST a) (BST a)

data Side = L | R
  deriving (Eq, Ord, Show)

type family Insert (t :: k) (bst :: BST k)  :: BST k where
  Insert t 'Empty = 'Branch t 'Empty 'Empty
  Insert t ('Branch a lbst rbst) =
    InsertChild (CmpType t a) t a lbst rbst

type family InsertChild (ord :: Ordering) (t :: k) (a :: k) (lbst :: BST k) (rbst :: BST k) :: BST k where
  InsertChild 'EQ t a lbst rbst = 'Branch a lbst rbst
  InsertChild 'LT t a lbst rbst = 'Branch a (Insert t lbst) rbst
  InsertChild 'GT t a lbst rbst = 'Branch a lbst (Insert t rbst)


type family Member (t :: k) (bst :: BST k)  :: Bool where
  Member t 'Empty = 'False
  Member t ('Branch a lbst rbst) = MemberImpl (CmpType t a) t lbst rbst

type family MemberImpl (ord :: Ordering) (t :: k) (lbst :: BST k) (rbst :: BST k) :: Bool where
  MemberImpl 'EQ t lbst rbst = 'True
  MemberImpl 'LT t lbst rbst = Member t lbst
  MemberImpl 'GT t lbst rbst = Member t rbst


type family Merge (small :: BST k) (big :: BST k) :: BST k where
  Merge Empty big = big
  Merge ('Branch a lbst rbst) big = Merge rbst (Merge lbst (Insert a big))


type family Delete (t :: k) (bst :: BST k) :: BST k where
  Delete t Empty = Empty
  Delete t ('Branch a lbst rbst) = DeleteImpl (CmpType t a) t a lbst rbst

type family DeleteImpl (ord :: Ordering) (t :: k) (a :: k) (lbst :: BST k) (rbst :: BST k) :: BST k where
  DeleteImpl 'LT t a lbst rbst = 'Branch a (Delete t lbst) rbst
  DeleteImpl 'EQ t a Empty rbst = rbst
  DeleteImpl 'EQ t a lbst Empty = lbst
  DeleteImpl 'EQ t a lbst rbst = 'Branch (RightMost lbst) (Delete (RightMost lbst) lbst) rbst
  DeleteImpl 'GT t a lbst rbst = 'Branch a lbst (Delete t rbst)

type family RightMost (bst :: BST k) :: k where
  RightMost ('Branch a lbst 'Empty) = a
  RightMost ('Branch a lbst rbst) = RightMost rbst


type family Find (t :: k) (bst :: BST k) :: [Side] where
  Member t ('Branch a lbst rbst) = FindImpl (CmpType t a) t lbst rbst
  Member t 'Empty = TypeError ('Text "Unable to find" ':<>: 'ShowType t)

type family FindImpl (ord :: Ordering) (t :: k) (lbst :: BST k) (rbst :: BST k) :: [Side] where
  FindImpl 'EQ t lbst rbst = '[]
  FindImpl 'LT t lbst rbst = 'L ': Find t lbst
  FindImpl 'GT t lbst rbst = 'R ': Find t rbst

