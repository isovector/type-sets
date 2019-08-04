{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -fplugin=Plugin #-}

module Test where

import CmpType
import Data.Proxy


type family IsEQ (a :: Ordering) :: Bool where
  IsEQ 'EQ = 'True
  IsEQ _   = 'False

zop :: Proxy 'EQ
zop = Proxy @(CmpType 2 2)

