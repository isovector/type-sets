{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE TypeFamilies   #-}

{-# OPTIONS_GHC -fdefer-type-errors
                -fno-warn-deferred-type-errors
                #-}

module ShouldNotTypecheck where

import Data.Proxy
import Test.ShouldNotTypecheck
import Type.Compare


type family Stuck :: k

testWrongType1 :: Proxy (CmpType a b) -> Proxy 'LT
testWrongType1 = id

testWrongType2 :: Proxy (CmpType Stuck Int) -> Proxy 'GT
testWrongType2 = id


testStuckInstances :: IO ()
testStuckInstances = do
  shouldNotTypecheck testWrongType1
  shouldNotTypecheck testWrongType2


