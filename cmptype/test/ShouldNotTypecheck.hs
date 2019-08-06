{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeOperators  #-}

{-# OPTIONS_GHC -fdefer-type-errors
                -fno-warn-deferred-type-errors
                #-}

-- TODO(sandy): It would be nice to have a better way of observing stuckness.
module ShouldNotTypecheck where

import Data.Proxy
import GHC.TypeLits
import Test.ShouldNotTypecheck
import Type.Compare


type family Stuck :: k

testWrongType1 :: Proxy (CmpType a b) -> Proxy 'LT
testWrongType1 = id

testWrongType2 :: Proxy (CmpType Stuck Int) -> Proxy 'GT
testWrongType2 = id

testWrongType3 :: Proxy (CmpType (0 - 1) 2) -> Proxy 'GT
testWrongType3 = id


testStuckInstances :: IO ()
testStuckInstances = do
  shouldNotTypecheck testWrongType1
  shouldNotTypecheck testWrongType2
  shouldNotTypecheck testWrongType3


