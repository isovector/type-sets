{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

-- | See <https://www.cs.kent.ac.uk/people/staff/smk/redblack/rb.html here> for
-- the original term-level code by Stefan Kahrs.
--
-- @since 0.1.1.0
module Type.RBSet
  ( -- * Core type
    TypeSet (..)
  , Empty
    -- * Set operations
  , Member
  , Insertable(Insert)
  , InsertAll
  , FromList
  , Removable(Remove)
  , Merge
  ) where

import Type.Compare
import GHC.TypeLits

-- | The color of a node.
data Color = R
           | B
    deriving (Show,Eq)

-- | A Red-Black tree.
--
-- @since 0.1.1.0
data TypeSet a = E
               | N Color (TypeSet a) a (TypeSet a)
    deriving (Show,Eq)

-- | A map without entries.
type Empty = E

--
--
-- Insertion

{- | Insert a list of type level key / value pairs into a type-level map.
-}
type family InsertAll (es :: [k]) (t :: TypeSet k) :: TypeSet k where
    InsertAll '[] t = t
    InsertAll ( v ': es ) t = Insert v (InsertAll es t)

{- | Build a type-level map out of a list of type level key / value pairs.
-}
type FromList (es :: [k]) = InsertAll es Empty

{- | The associated type family 'Insert' produces the resulting map.

 -}
class Insertable (k :: ki) (t :: TypeSet ki) where
    type Insert k t :: TypeSet ki

-- insert x s =
--  T B a z b
--  where
--  T _ a z b = ins s
instance (InsertableHelper1 k t, Insert1 k t ~ inserted, CanMakeBlack inserted) => Insertable k t where
    type Insert k t = MakeBlack (Insert1 k t)

class CanMakeBlack (t :: TypeSet ki) where
    type MakeBlack t :: TypeSet ki

instance CanMakeBlack (N color left k right) where
    type MakeBlack (N color left k right) = N B left k right

instance CanMakeBlack E where
    type MakeBlack E = E

class InsertableHelper1 (k :: ki)
                        (t :: TypeSet ki) where
    type Insert1 k t :: TypeSet ki

instance InsertableHelper1 k E where
    type Insert1 k E = N R E k E

instance (CmpType k k' ~ ordering,
          InsertableHelper2 ordering k color left k' right
         )
         => InsertableHelper1 k (N color left k' right) where
    -- FIXME possible duplicate work with CmpType: both in constraint and in associated type family.
    -- Is that bad? How to avoid it?
    type Insert1 k (N color left k' right) = Insert2 (CmpType k k') k color left k' right

class InsertableHelper2 (ordering :: Ordering)
                        (k :: ki)
                        (color :: Color)
                        (left :: TypeSet ki)
                        (k' :: ki)
                        (right :: TypeSet ki) where
    type Insert2 ordering k color left k' right :: TypeSet ki

--  ins s@(T B a y b)
--      | x<y = balance (ins a) y b
instance (InsertableHelper1 k left, Insert1 k left ~ inserted,
          Balanceable inserted k' right
         )
         => InsertableHelper2 LT k B left k' right where
    type Insert2              LT k B left k' right = Balance (Insert1 k left) k' right

--  ins s@(T B a y b)
--      | x<y = balance (ins a) y b
instance (InsertableHelper1 k left, Insert1 k left ~ inserted,
          Balanceable inserted k' right
         )
         => InsertableHelper2 LT k R left k' right where
    type Insert2              LT k R left k' right = N R (Insert1 k left) k' right


instance InsertableHelper2 EQ k color left k right where
    type Insert2           EQ k color left k right = N color left k right

--  ins s@(T B a y b)
--      | ...
--      | x>y = balance a y (ins b)
instance (InsertableHelper1 k right, Insert1 k right ~ inserted,
          Balanceable left  k' inserted
         )
         => InsertableHelper2 GT k B left k' right where
    type Insert2              GT k B left k' right = Balance left  k' (Insert1 k right)

--  ins s@(T R a y b)
--      | ...
--      | x>y = T R a y (ins b)
instance (InsertableHelper1 k right, Insert1 k right ~ inserted,
          Balanceable left  k' inserted
         )
         => InsertableHelper2 GT k R left k' right where
    type Insert2              GT k R left k' right = N R left k' (Insert1 k right)

data BalanceAction = BalanceSpecial
                   | BalanceLL
                   | BalanceLR
                   | BalanceRL
                   | BalanceRR
                   | DoNotBalance
                   deriving Show

type family ShouldBalance (left :: TypeSet ki) (right :: TypeSet ki) :: BalanceAction where
    ShouldBalance (N R _ _ _) (N R _ _ _) = BalanceSpecial
    ShouldBalance (N R (N R _ _ _) _ _) _ = BalanceLL
    ShouldBalance (N R _ _ (N R _ _ _)) _ = BalanceLR
    ShouldBalance _ (N R (N R _ _ _) _ _) = BalanceRL
    ShouldBalance _ (N R _ _ (N R _ _ _)) = BalanceRR
    ShouldBalance _ _                     = DoNotBalance

class Balanceable (left :: TypeSet ki) (k :: ki) (right :: TypeSet ki) where
    type Balance left k right :: TypeSet ki

instance (ShouldBalance left right ~ action,
          BalanceableHelper action left k right
         )
         => Balanceable left k right where
    -- FIXME possible duplicate work with ShouldBalance: both in constraint and in associated type family.
    -- Is that bad? How to avoid it?
    type Balance left k right = Balance' (ShouldBalance left right) left k right

class BalanceableHelper (action :: BalanceAction)
                        (left :: TypeSet ki)
                        (k :: ki)
                        (right :: TypeSet ki) where
    type Balance' action left k right :: TypeSet ki

-- balance (T R a x b) y (T R c z d) = T R (T B a x b) y (T B c z d)
instance BalanceableHelper BalanceSpecial (N R left1 k1 right1) kx (N R left2 k2 right2) where
    type Balance'          BalanceSpecial (N R left1 k1 right1) kx (N R left2 k2 right2) =
                                      N R (N B left1 k1 right1) kx (N B left2 k2 right2)

-- balance (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
instance BalanceableHelper BalanceLL (N R (N R a k1 b) k2 c) k3 d where
    type Balance'          BalanceLL (N R (N R a k1 b) k2 c) k3 d =
                                 N R (N B a k1 b) k2 (N B c k3 d)

-- balance (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
instance BalanceableHelper BalanceLR (N R a k1 (N R b k2 c)) k3 d where
    type Balance'          BalanceLR (N R a k1 (N R b k2 c)) k3 d =
                                 N R (N B a k1 b) k2 (N B c k3 d)

-- balance a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
instance BalanceableHelper BalanceRL a k1 (N R (N R b k2 c) k3 d) where
    type Balance'          BalanceRL a k1 (N R (N R b k2 c) k3 d) =
                                 N R (N B a k1 b) k2 (N B c k3 d)


-- balance a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
instance BalanceableHelper BalanceRR a k1(N R b k2 (N R c k3 d)) where
    type Balance'          BalanceRR a k1(N R b k2 (N R c k3 d)) =
                                 N R (N B a k1 b) k2 (N B c k3 d)

-- balance a x b = T B a x b
instance BalanceableHelper DoNotBalance a k b where
    type Balance'          DoNotBalance a k b = N B a k b


--- Member
---
---
------------------------------------------------------------------------------
-- | /O(log n)/. Determine membership in the 'TypeSet.'
type family Member (t :: k) (bst :: TypeSet k)  :: Bool where
  Member t 'E = 'False
  Member t ('N _ lbst a rbst) = MemberImpl (CmpType t a) t lbst rbst

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
  Merge ('N _ lbst a rbst) big = Merge rbst (Merge lbst (Insert a big))


--
--
--
-- deletion
--
--
--

type family DiscriminateBalL (l :: TypeSet ki) (r :: TypeSet ki) :: Bool where
    DiscriminateBalL (N R _ _ _) _ = False
    DiscriminateBalL _           _ = True

class BalanceableL (l :: TypeSet ki) (k :: ki) (r :: TypeSet ki) where
    type BalL l k r :: TypeSet ki

class BalanceableHelperL (b :: Bool) (l :: TypeSet ki) (k :: ki) (r :: TypeSet ki) where
    type BalL' b l k r :: TypeSet ki

instance (DiscriminateBalL l r ~ b, BalanceableHelperL b l k r) => BalanceableL l k r where
    type BalL l k r = BalL' (DiscriminateBalL l r) l k r

-- balleft :: RB a -> a -> RB a -> RB a
-- balleft (T R a x b) y c = T R (T B a x b) y c
instance BalanceableHelperL False (N R left1 k1 right1) k2 right2 where
    type BalL'              False (N R left1 k1 right1) k2 right2 =
                             (N R (N B left1 k1 right1) k2 right2)

-- balleft bl x (T B a y b) = balance bl x (T R a y b)
-- the @(N B in the call to balance tree is misleading, as it is ingored...
instance (N R t2 z t3 ~ g, BalanceableHelper (ShouldBalance t1 g) t1 y g) =>
    BalanceableHelperL True t1 y (N B t2 z t3) where
    type BalL'         True t1 y (N B t2 z t3)
                 =  Balance t1 y (N R t2 z t3)

-- balleft bl x (T R (T B a y b) z c) = T R (T B bl x a) y (balance b z (sub1 c))
instance (N R l k r ~ g, BalanceableHelper    (ShouldBalance t3 g) t3 z g) =>
    BalanceableHelperL True t1 y (N R (N B t2 u t3) z (N B l k r)) where
    type BalL'         True t1 y (N R (N B t2 u t3) z (N B l k r)) =
                             N R (N B t1 y t2) u (Balance t3 z (N R l k r))


-- balright :: RB a -> a -> RB a -> RB a
-- balright a x (T R b y c) = T R a x (T B b y c)
-- balright (T B a x b) y bl = balance (T R a x b) y bl
-- balright (T R a x (T B b y c)) z bl = T R (balance (sub1 a) x b) y (T B c z bl)
type family DiscriminateBalR (l :: TypeSet ki) (r :: TypeSet ki) :: Bool where
    DiscriminateBalR _ (N R _ _ _) = False
    DiscriminateBalR _ _           = True

class BalanceableR (l :: TypeSet ki) (k :: ki) (r :: TypeSet ki) where
    type BalR l k r :: TypeSet ki

class BalanceableHelperR (b :: Bool) (l :: TypeSet ki) (k :: ki) (r :: TypeSet ki) where
    type BalR' b l k r :: TypeSet ki

instance (DiscriminateBalR l r ~ b, BalanceableHelperR b l k r) => BalanceableR l k r where
    type BalR l k r = BalR' (DiscriminateBalR l r) l k r

-- balright :: RB a -> a -> RB a -> RB a
-- balright a x (T R b y c) = T R a x (T B b y c)
instance BalanceableHelperR False right2 k2 (N R left1 k1 right1) where
    type BalR'              False right2 k2 (N R left1 k1 right1) =
                                  (N R right2 k2 (N B left1 k1 right1))

-- balright (T B a x b) y bl = balance (T R a x b) y bl
instance (N R t2 z t3 ~ g, ShouldBalance g t1 ~ shouldbalance, BalanceableHelper shouldbalance g y t1) =>
    BalanceableHelperR True (N B t2 z t3) y t1 where
    type BalR'         True (N B t2 z t3) y t1
             =  Balance (N R t2 z t3) y t1

-- balright (T R a x (T B b y c)) z bl = T R (balance (sub1 a) x b) y (T B c z bl)
instance (N R t2 u t3 ~ g, ShouldBalance g l ~ shouldbalance, BalanceableHelper shouldbalance g z l) =>
    BalanceableHelperR True (N R (N B t2 u t3) z (N B l k r)) y t1 where
    type BalR'         True (N R (N B t2 u t3) z (N B l k r)) y t1 =
                             N R (Balance (N R t2 u t3) z l) k (N B r y t1)
-- app :: RB a -> RB a -> RB a
-- app E x = x
-- app x E = x
-- app (T R a x b) (T R c y d) =
--  case app b c of
--      T R b' z c' -> T R(T R a x b') z (T R c' y d)
--      bc -> T R a x (T R bc y d)
-- app (T B a x b) (T B c y d) =
--  case app b c of
--      T R b' z c' -> T R(T B a x b') z (T B c' y d)
--      bc -> balleft a x (T B bc y d)
-- app a (T R b x c) = T R (app a b) x c
-- app (T R a x b) c = T R a x (app b c)


class Fuseable (l :: TypeSet ki) (r :: TypeSet ki) where
    type Fuse l r :: TypeSet ki

instance Fuseable E E where
    type Fuse E E = E

-- app E x = x
instance Fuseable E (N color left k right) where
    type Fuse E (N color left k right) = N color left k right

-- app x E = x
instance Fuseable (N color left k right) E where
    type Fuse (N color left k right) E = N color left k right

-- app a (T R b x c) = T R (app a b) x c
instance Fuseable (N B left1 k1 right1) left2
    => Fuseable (N B left1 k1 right1) (N R left2 k2 right2) where
    type Fuse   (N B left1 k1 right1) (N R left2 k2 right2) = N R (Fuse (N B left1 k1 right1) left2) k2 right2


-- app (T R a x b) c = T R a x (app b c)
instance Fuseable right1 (N B left2 k2 right2)
    => Fuseable (N R left1 k1 right1) (N B left2 k2 right2) where
    type Fuse   (N R left1 k1 right1) (N B left2 k2 right2) = N R left1 k1 (Fuse right1 (N B left2 k2 right2))


-- app (T R a x b) (T R c y d) =
instance (Fuseable right1 left2, Fuse right1 left2 ~ fused, FuseableHelper1 fused (N R left1 k1 right1) (N R left2 k2 right2))
    => Fuseable (N R left1 k1 right1) (N R left2 k2 right2) where
    type Fuse   (N R left1 k1 right1) (N R left2 k2 right2) = Fuse1 (Fuse right1 left2) (N R left1 k1 right1) (N R left2 k2 right2)

class FuseableHelper1 (fused :: TypeSet ki) (l :: TypeSet ki) (r :: TypeSet ki) where
    type Fuse1 fused l r :: TypeSet ki

-- app (T R a x b) (T R c y d) =
--  case app b c of
--      T R b' z c' -> T R (T R a x b') z (T R c' y d)
-- FIXME: The Fuseable constraint is repeated from avobe :(
instance (Fuseable right1 left2, Fuse right1 left2 ~ N R s1 z s2)
    => FuseableHelper1 (N R s1 z s2) (N R left1 k1 right1) (N R left2 k2 right2) where
    type Fuse1         (N R s1 z s2) (N R left1 k1 right1) (N R left2 k2 right2) = N R (N R left1 k1 s1) z (N R s2 k2 right2)


-- app (T R a x b) (T R c y d) =
--  case app b c of
--      ...
--      bc -> T R a x (T R bc y d)
-- FIXME: The Fuseable constraint is repeated from above :(
instance (Fuseable right1 left2, Fuse right1 left2 ~ N B s1 z s2)
    => FuseableHelper1 (N B s1 z s2) (N R left1 k1 right1) (N R left2 k2 right2) where
    type Fuse1         (N B s1 z s2) (N R left1 k1 right1) (N R left2 k2 right2) = N R left1 k1 (N R (N B s1 z s2) k2 right2)

-- app (T R a x b) (T R c y d) =
--  case app b c of
--      ...
--      bc -> T R a x (T R bc y d)
instance FuseableHelper1 E (N R left1 k1 E) (N R E k2 right2) where
    type Fuse1           E (N R left1 k1 E) (N R E k2 right2) = N R left1 k1 (N R E k2 right2)

-- app (T B a x b) (T B c y d) =
instance (Fuseable right1 left2, Fuse right1 left2 ~ fused, FuseableHelper2 fused (N B left1 k1 right1) (N B left2 k2 right2))
    => Fuseable (N B left1 k1 right1) (N B left2 k2 right2) where
    type Fuse   (N B left1 k1 right1) (N B left2 k2 right2) = Fuse2 (Fuse right1 left2) (N B left1 k1 right1) (N B left2 k2 right2)

-- could FuseableHelper1 and FuseableHelper2 be, well... fused?
class FuseableHelper2 (fused :: TypeSet ki) (l :: TypeSet ki) (r :: TypeSet ki) where
    type Fuse2 fused l r :: TypeSet ki

-- app (T B a x b) (T B c y d) =
--  case app b c of
--      T R b' z c' -> T R (T B a x b') z (T B c' y d)
instance (Fuseable right1 left2, Fuse right1 left2 ~ N R s1 z s2)
    => FuseableHelper2 (N R s1 z s2) (N B left1 k1 right1) (N B left2 k2 right2) where
    type Fuse2         (N R s1 z s2) (N B left1 k1 right1) (N B left2 k2 right2) = N R (N B left1 k1 s1) z (N B s2 k2 right2)

-- app (T B a x b) (T B c y d) =
--  case app b c of
--      ...
--      bc -> balleft a x (T B bc y d)
instance (Fuseable right1 left2, Fuse right1 left2 ~ N B s1 z s2, BalanceableL left1 k1 (N B (N B s1 z s2) k2 right2))
    => FuseableHelper2 (N B s1 z s2) (N B left1 k1 right1) (N B left2 k2 right2) where
    type Fuse2         (N B s1 z s2) (N B left1 k1 right1) (N B left2 k2 right2) = BalL left1 k1 (N B (N B s1 z s2) k2 right2)

-- app (T B a x b) (T B c y d) =
--  case app b c of
--      ...
--      bc -> balleft a x (T B bc y d)
instance (BalanceableL left1 k1 (N B E k2 right2))
    => FuseableHelper2 E (N B left1 k1 E) (N B E k2 right2) where
    type Fuse2         E (N B left1 k1 E) (N B E k2 right2) = BalL left1 k1 (N B E k2 right2)


--  del E = E
--  del (T _ a y b)
--      | x<y = delformLeft a y b
--      | x>y = delformRight a y b
--      | otherwise = app a b
class Delable (k :: ki) (t :: TypeSet ki) where
    type Del k t :: TypeSet ki

--  delformLeft a@(T B _ _ _) y b = balleft (del a) y b
--  delformLeft a y b = T R (del a) y b
--  In the term-level code, the k to delete is already on the environment.
class DelableL (k :: ki) (l :: TypeSet ki) (kx :: ki)  (r :: TypeSet ki) where
    type DelL k l kx r :: TypeSet ki

--  delformLeft a@(T B _ _ _) y b = balleft (del a) y b
instance (N B leftz kz rightz ~ g, Delable k g, Del k g ~ deleted, BalanceableL deleted kx right)
    => DelableL k (N B leftz kz rightz) kx right where
    type DelL   k (N B leftz kz rightz) kx right = BalL (Del k (N B leftz kz rightz)) kx right

--  delformLeft a y b = T R (del a) y b
instance (Delable k (N R leftz kz rightz))
    => DelableL k (N R leftz kz rightz) kx right where
    type DelL   k (N R leftz kz rightz) kx right = N R (Del k (N R leftz kz rightz)) kx right

--  delformLeft a y b = T R (del a) y b
instance DelableL k E kx right where
    type DelL     k E kx right = N R E kx right

--  delformRight a y b@(T B _ _ _) = balright a y (del b)
--  delformRight a y b = T R a y (del b)
class DelableR (k :: ki) (l :: TypeSet ki) (kx :: ki) (r :: TypeSet ki) where
    type DelR k l kx r :: TypeSet ki

--  delformRight a y b@(T B _ _ _) = balright a y (del b)
instance (N B leftz kz rightz ~ g, Delable k g, Del k g ~ deleted, BalanceableR left kx deleted)
    => DelableR k left kx (N B leftz kz rightz) where
    type DelR   k left kx (N B leftz kz rightz) = BalR left kx (Del k (N B leftz kz rightz))


--  delformRight a y b = T R a y (del b)
instance (Delable k (N R leftz kz rightz))
    => DelableR k left kx (N R leftz kz rightz) where
    type   DelR k left kx (N R leftz kz rightz) = N R left kx (Del k (N R leftz kz rightz))

--  delformRight a y b = T R a y (del b)
instance DelableR k left kx E where
    type DelR     k left kx E = N R left kx E

--  del E = E
instance Delable k E where
    type Del     k E = E

-- the color is discarded
--  del (T _ a y b)
--      | x<y = delformLeft a y b
--      | x>y = delformRight a y b
--      | otherwise = app a b
instance (CmpType kx k ~ ordering, DelableHelper ordering k left kx right) => Delable k (N color left kx right) where
    type Del k (N color left kx right) = Del' (CmpType kx k) k left kx right

class DelableHelper (ordering :: Ordering) (k :: ki) (l :: TypeSet ki) (kx :: ki) (r :: TypeSet ki) where
    type Del' ordering k l kx r :: TypeSet ki

--      | x<y = delformLeft a y b
instance DelableL k left kx right => DelableHelper GT k left kx right where
    type Del'                                      GT k left kx right = DelL k left kx right

--      | otherwise = app a b
instance Fuseable left right => DelableHelper EQ k left k right where
    type Del'                                 EQ k left k right = Fuse left right

--      | x>y = delformRight a y b
instance DelableR k left kx right => DelableHelper LT k left kx right where
    type Del'                                      LT k left kx right = DelR k left kx right

{- | The associated type family 'Remove' produces the resulting map.

 -}
class Removable (k :: ki) (t :: TypeSet ki) where
    type Remove k t :: TypeSet ki

instance (Delable k t, Del k t ~ deleted, CanMakeBlack deleted) => Removable k t where
    type Remove k t = MakeBlack (Del k t)



-- The original term-level code, taken from:
-- https://www.cs.kent.ac.uk/people/staff/smk/redblack/rb.html
--
-- {- Version 1, 'untyped' -}
-- data Color = R | B deriving Show
-- data RB a = E | T Color (RB a) a (RB a) deriving Show
--
-- {- Insertion and membership test as by Okasaki -}
-- insert :: Ord a => a -> RB a -> RB a
-- insert x s =
--  T B a z b
--  where
--  T _ a z b = ins s
--  ins E = T R E x E
--  ins s@(T B a y b)
--      | x<y = balance (ins a) y b
--      | x>y = balance a y (ins b)
--      | otherwise = s
--  ins s@(T R a y b)
--      | x<y = T R (ins a) y b
--      | x>y = T R a y (ins b)
--      | otherwise = s
--
--
-- {- balance: first equation is new,
--    to make it work with a weaker invariant -}
-- balance :: RB a -> a -> RB a -> RB a
-- balance (T R a x b) y (T R c z d) = T R (T B a x b) y (T B c z d)
-- balance (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
-- balance (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
-- balance a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
-- balance a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
-- balance a x b = T B a x b
--
-- member :: Ord a => a -> RB a -> Bool
-- member x E = False
-- member x (T _ a y b)
--  | x<y = member x a
--  | x>y = member x b
--  | otherwise = True
--
-- {- deletion a la SMK -}
-- delete :: Ord a => a -> RB a -> RB a
-- delete x t =
--  case del t of {T _ a y b -> T B a y b; _ -> E}
--  where
--  del E = E
--  del (T _ a y b)
--      | x<y = delformLeft a y b
--      | x>y = delformRight a y b
--             | otherwise = app a b
--  delformLeft a@(T B _ _ _) y b = balleft (del a) y b
--  delformLeft a y b = T R (del a) y b
--
--  delformRight a y b@(T B _ _ _) = balright a y (del b)
--  delformRight a y b = T R a y (del b)
--
-- balleft :: RB a -> a -> RB a -> RB a
-- balleft (T R a x b) y c = T R (T B a x b) y c
-- balleft bl x (T B a y b) = balance bl x (T R a y b)
-- balleft bl x (T R (T B a y b) z c) = T R (T B bl x a) y (balance b z (sub1 c))
--
-- balright :: RB a -> a -> RB a -> RB a
-- balright a x (T R b y c) = T R a x (T B b y c)
-- balright (T B a x b) y bl = balance (T R a x b) y bl
-- balright (T R a x (T B b y c)) z bl = T R (balance (sub1 a) x b) y (T B c z bl)
--
-- sub1 :: RB a -> RB a
-- sub1 (T B a x b) = T R a x b
-- sub1 _ = error "invariance violation"
--
-- app :: RB a -> RB a -> RB a
-- app E x = x
-- app x E = x
-- app (T R a x b) (T R c y d) =
--  case app b c of
--      T R b' z c' -> T R (T R a x b') z (T R c' y d)
--      bc -> T R a x (T R bc y d)
-- app (T B a x b) (T B c y d) =
--  case app b c of
--      T R b' z c' -> T R(T B a x b') z (T B c' y d)
--      bc -> balleft a x (T B bc y d)
-- app a (T R b x c) = T R (app a b) x c
-- app (T R a x b) c = T R a x (app b c)

