{-# LANGUAGE AutoDeriveTypeable   #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Type.Set
  ( -- * Core type
    TypeSet (..)

    -- * Set operations
  , Member
  , Insert
  , Remove
  , Merge

    -- * Tree operations
  , Locate
  , Follow
  , Side (..)
  ) where

import Type.Compare
import Type.Reflection
import GHC.TypeLits


------------------------------------------------------------------------------
-- | A binary search tree. When @-XDataKinds@ is turned on, this becomes the
-- backbone of the type-level set.
--
-- >>> type MySet = Insert Bool (Insert String (Insert (Maybe Int) 'Empty))
data TypeSet a
  = Empty
  | Branch a (TypeSet a) (TypeSet a)


------------------------------------------------------------------------------
-- | Either left or right down a path.
data Side = L | R
  deriving (Eq, Ord, Show, Typeable)

deriving instance Typeable 'L
deriving instance Typeable 'R


------------------------------------------------------------------------------
-- | /O(log n)/. Insert an element into the 'TypeSet'.
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
-- | /O(log n)/. Determine membership in the 'TypeSet.'
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
-- | /O(m log n)/ for @Merge m n@; put your smaller set on the left side. Merge
-- two 'TypeSet's together.
type family Merge (small :: TypeSet k) (big :: TypeSet k) :: TypeSet k where
  Merge Empty big   = big
  Merge small Empty = small
  Merge ('Branch a lbst rbst) big = Merge rbst (Merge lbst (Insert a big))


------------------------------------------------------------------------------
-- | /O(log n)/. Remove an element from the 'TypeSet'.
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
-- | /O(log n)/. Get the right-most element in a tree. This function is stuck
-- if the tree is empty.
type family RightMost (bst :: TypeSet k) :: k where
  RightMost ('Branch a lbst 'Empty) = a
  RightMost ('Branch a lbst rbst) = RightMost rbst


------------------------------------------------------------------------------
-- | /O(log n)/. Compute a @['Side']@ which finds the desired element in the
-- tree. The result of this can be passed to 'Follow' in order to look up the
-- same element again later.
type family Locate (t :: k) (bst :: TypeSet k) :: [Side] where
  Member t ('Branch a lbst rbst) = LocateImpl (CmpType t a) t lbst rbst
  Member t 'Empty = TypeError ('Text "Unable to locate: " ':<>: 'ShowType t)

type family LocateImpl (ord :: Ordering)
                       (t :: k)
                       (lbst :: TypeSet k)
                       (rbst :: TypeSet k) :: [Side] where
  LocateImpl 'EQ t lbst rbst = '[]
  LocateImpl 'LT t lbst rbst = 'L ': Locate t lbst
  LocateImpl 'GT t lbst rbst = 'R ': Locate t rbst


------------------------------------------------------------------------------
-- | /O(log n)/. Follow the result of a 'Locate' to get a particular element in
-- the tree.
type family Follow (ss :: [Side]) (bst :: TypeSet k) :: k where
  Follow '[] ('Branch t _ _) = t
  Follow ('L ': ss) ('Branch _ l _) = Follow ss l
  Follow ('R ': ss) ('Branch _ _ r) = Follow ss r
  Follow ss 'Empty = TypeError ('Text "Unable to follow: " ':<>: 'ShowType ss)
