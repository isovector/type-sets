{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib where

import CmpType
import GHC.TypeLits

data BST a
  = Empty
  | Branch a (BST a) (BST a)

type family Insert (bst :: BST k) (t :: k) :: BST k where
  Insert 'Empty t = 'Branch t 'Empty 'Empty
  Insert ('Branch a lbst rbst) t =
    InsertChild (CmpType t a) t a lbst rbst

type family InsertChild (ord :: Ordering) (t :: k) (a :: k) (lbst :: BST k) (rbst :: BST k) :: BST k where
  InsertChild 'EQ t a lbst rbst = 'Branch a lbst rbst
  InsertChild 'LT t a lbst rbst = 'Branch a (Insert lbst t) rbst
  InsertChild 'GT t a lbst rbst = 'Branch a lbst (Insert rbst t)


type family Member (bst :: BST k) (t :: k) :: Bool where
  Member 'Empty t = 'False
  Member ('Branch a lbst rbst) t = MemberImpl (CmpType t a) t lbst rbst

type family MemberImpl (ord :: Ordering) (t :: k) (lbst :: BST k) (rbst :: BST k) :: Bool where
  MemberImpl 'EQ t lbst rbst = 'True
  MemberImpl 'LT t lbst rbst = Member lbst t
  MemberImpl 'GT t lbst rbst = Member rbst t


type family Merge (small :: BST k) (big :: BST k) :: BST k where
  Merge Empty big = big
  Merge ('Branch a lbst rbst) big = Merge rbst (Merge lbst (Insert big a))


type family Delete (bst :: BST k) (t :: k) :: BST k where
  Delete Empty t = Empty
  Delete ('Branch a lbst rbst) t = DeleteImpl (CmpType t a) t a lbst rbst

type family DeleteImpl (ord :: Ordering) (t :: k) (a :: k) (lbst :: BST k) (rbst :: BST k) :: BST k where
  DeleteImpl 'LT t a lbst rbst = 'Branch a (Delete lbst t) rbst
  DeleteImpl 'EQ t a Empty rbst = rbst
  DeleteImpl 'EQ t a lbst Empty = lbst
  DeleteImpl 'EQ t a lbst rbst = 'Branch (RightMost lbst) (Delete lbst (RightMost lbst)) rbst
  DeleteImpl 'GT t a lbst rbst = 'Branch a lbst (Delete rbst t)

type family RightMost (bst :: BST k) :: k where
  RightMost ('Branch a lbst 'Empty) = a
  RightMost ('Branch a lbst rbst) = RightMost rbst

