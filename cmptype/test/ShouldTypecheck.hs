{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}

{-# OPTIONS_GHC -fplugin=Type.Compare.Plugin #-}

module ShouldTypecheck where

import           Data.Proxy
import           GHC.TypeLits
import qualified Prelude as P
import           Prelude hiding (Int)
import           Type.Compare


testSym1 :: Proxy (CmpType "a" "apple") -> Proxy 'LT
testSym1 = id

testSym2 :: Proxy (CmpType "banana" "banana") -> Proxy 'EQ
testSym2 = id

testSym3 :: Proxy (CmpType "dominos" "cabana") -> Proxy 'GT
testSym3 = id


testNat1 :: Proxy (CmpType 0 1) -> Proxy 'LT
testNat1 = id

testNat2 :: Proxy (CmpType 2 2) -> Proxy 'EQ
testNat2 = id

testNat3 :: Proxy (CmpType 4 3) -> Proxy 'GT
testNat3 = id


data Apricot
data Blueberry
data Coconut
data Durian

testType1 :: Proxy (CmpType Apricot Blueberry) -> Proxy 'LT
testType1 = id

testType2 :: Proxy (CmpType Coconut Coconut) -> Proxy 'EQ
testType2 = id

testType3 :: Proxy (CmpType Durian Coconut) -> Proxy 'GT
testType3 = id


type family IsEQ (ord :: Ordering) :: Bool where
  IsEQ 'EQ = 'True
  IsEQ _   = 'False

testType4 :: Proxy (IsEQ (CmpType Durian Coconut)) -> Proxy 'False
testType4 = id

testType5 :: Proxy (IsEQ (CmpType Blueberry Blueberry)) -> Proxy 'True
testType5 = id

testType6 :: Proxy (CmpType [Apricot] [Blueberry]) -> Proxy 'LT
testType6 = id

testType7 :: Proxy (CmpType [Durian] [Coconut]) -> Proxy 'GT
testType7 = id

testType8 :: Proxy '[CmpType [Durian] [Coconut]] -> Proxy '[ 'GT ]
testType8 = id

testType9 :: Proxy '[IsEQ (CmpType [Durian] [Coconut])] -> Proxy '[ 'False ]
testType9 = id

testType10 :: Proxy '[IsEQ (CmpType [[Apricot]] [[Apricot]])] -> Proxy '[ 'True ]
testType10 = id

testType11 :: Proxy '[IsEQ (CmpType [Apricot] [[Apricot]])] -> Proxy '[ 'False ]
testType11 = id


data Int

testType12 :: Proxy (IsEQ (CmpType Int P.Int)) -> Proxy 'False
testType12 = id

testType13 :: Proxy (IsEQ (CmpType '[Int] '[P.Int])) -> Proxy 'False
testType13 = id

testType14 :: Proxy (CmpType '(Int, Int) '(Int, Bool)) -> Proxy 'GT
testType14 = id

testType15 :: Proxy (CmpType '[Int, Bool] '[Int, Bool]) -> Proxy 'EQ
testType15 = id

testType16 :: Proxy (CmpType '[Int, Int] '[Int, Bool]) -> Proxy 'GT
testType16 = id

-- NOTE(sandy): I don't really understand why this one is so...
testType17 :: Proxy (CmpType '[Int, Bool] '[]) -> Proxy 'LT
testType17 = id

testType18 :: Proxy (CmpType '(String, Bool, '(Int, Bool, String), Int)
                             '(String, Bool, '(Int, Char, String), Int)) -> Proxy 'LT
testType18 = id

testType19 :: Proxy (CmpType '(String, Bool, '(Int, Char, String), Int)
                             '(String, Bool, '(Int, Bool, String), Int)) -> Proxy 'GT
testType19 = id

testType20 :: Proxy (CmpType '(String, Bool, '(Int, Char, String), Int)
                             '(String, Bool, '(Int, Char, String), Int)) -> Proxy 'EQ
testType20 = id

testType21 :: Proxy (CmpType '(String, Bool, '(Int, "a", String), Int)
                             '(String, Bool, '(Int, "b", String), Int)) -> Proxy 'LT
testType21 = id

testType22 :: Proxy (CmpType '(String, Bool, '(Int, "b", String), Int)
                             '(String, Bool, '(Int, "b", String), Int)) -> Proxy 'EQ
testType22 = id

