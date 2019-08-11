{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -fplugin=Type.Compare.Plugin      #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=0  #-} -- with iterations=10, bit sets fail in the RB tests :(

import Data.Proxy
import Data.Functor.Identity
import Data.Functor.Const
import Type.Set
import qualified Type.RBSet as RB

type MySet = Insert Bool (Insert String (Insert (Maybe Int) 'Empty))

test1 :: Proxy (Member Bool MySet) -> Proxy 'True
test1 = id  -- Bool is a member :)

test2 :: Proxy (Member Char MySet) -> Proxy 'False
test2 = id  -- False is not a member :(

-- RB tests
--
--
type MyRBSet = RB.FromList '[
                              Bool
                            , String
                            , String
                            , String
                            , Maybe Int
                            , Maybe Char
                            , Char
                            , Either Int Int
                            , Either Int Int
                            , Either Int Int
                            , Either Char Int
                            , Either Bool Int
                            , Maybe Bool
                            , Identity Int
                            , Identity Char
                            , Identity Bool
                            , Either Int (Identity Int)
                            , Either Int (Identity Char)
                            , Either Int (Identity Bool)
                            , Either Char (Identity Int)
                            , Either Char (Identity Char)
                            , Either Char (Identity Bool)
                            , Either Bool (Identity Int)
                            , Either Bool (Identity Char)
                            , Either Bool (Identity Bool)
                            ]

type MyReducedRBSet = RB.Remove Bool
                    ( RB.Remove String
                    ( RB.Remove (Either Int Int)
                    ( RB.Remove (Either Char Int)
                    ( RB.Remove (Either Bool (Identity Int))
                    ( RB.Remove (Either Bool (Identity Char))
                    ( RB.Remove (Either Bool (Identity Bool))
                      MyRBSet))))))

type MyReducedToEmptyRBSet =
                      RB.Remove (Maybe Int)
                    ( RB.Remove (Maybe Char)
                    ( RB.Remove Char
                    ( RB.Remove (Either Bool Int)
                    ( RB.Remove (Maybe Bool)
                    ( RB.Remove (Identity Int)
                    ( RB.Remove (Identity Char)
                    ( RB.Remove (Identity Bool)
                    ( RB.Remove (Either Int (Identity Int))
                    ( RB.Remove (Either Int (Identity Char))
                    ( RB.Remove (Either Int (Identity Bool))
                    ( RB.Remove (Either Char (Identity Int))
                    ( RB.Remove (Either Char (Identity Char))
                    ( RB.Remove (Either Char (Identity Bool))
                      MyReducedRBSet)))))))))))))

type MyMergedRBSet = RB.Merge (RB.FromList '[Const Int Bool,
                                             Const Int Char,
                                             Const Int String])
                              MyReducedRBSet

testRB1 :: Proxy (RB.Member (Bool) MyRBSet) -> Proxy 'True
testRB1 = id
testRB2 :: Proxy (RB.Member (String) MyRBSet) -> Proxy 'True
testRB2 = id
testRB3 :: Proxy (RB.Member (Maybe Int) MyRBSet) -> Proxy 'True
testRB3 = id
testRB4 :: Proxy (RB.Member (Maybe Char) MyRBSet) -> Proxy 'True
testRB4 = id
testRB5 :: Proxy (RB.Member (Char) MyRBSet) -> Proxy 'True
testRB5 = id
testRB6 :: Proxy (RB.Member (Either Int Int) MyRBSet) -> Proxy 'True
testRB6 = id
testRB7 :: Proxy (RB.Member (Either Char Int) MyRBSet) -> Proxy 'True
testRB7 = id
testRB8 :: Proxy (RB.Member (Either Bool Int) MyRBSet) -> Proxy 'True
testRB8 = id
testRB9 :: Proxy (RB.Member (Maybe Bool) MyRBSet) -> Proxy 'True
testRB9 = id
testRB10 :: Proxy (RB.Member (Identity Int) MyRBSet) -> Proxy 'True
testRB10 = id
testRB11 :: Proxy (RB.Member (Identity Char) MyRBSet) -> Proxy 'True
testRB11 = id
testRB12 :: Proxy (RB.Member (Identity Bool) MyRBSet) -> Proxy 'True
testRB12 = id
testRB13 :: Proxy (RB.Member (Either Int (Identity Int)) MyRBSet) -> Proxy 'True
testRB13 = id
testRB14 :: Proxy (RB.Member (Either Int (Identity Char)) MyRBSet) -> Proxy 'True
testRB14 = id
testRB15 :: Proxy (RB.Member (Either Int (Identity Bool)) MyRBSet) -> Proxy 'True
testRB15 = id
testRB16 :: Proxy (RB.Member (Either Char (Identity Int)) MyRBSet) -> Proxy 'True
testRB16 = id
testRB17 :: Proxy (RB.Member (Either Char (Identity Char)) MyRBSet) -> Proxy 'True
testRB17 = id
testRB18 :: Proxy (RB.Member (Either Char (Identity Bool)) MyRBSet) -> Proxy 'True
testRB18 = id
testRB19 :: Proxy (RB.Member (Either Bool (Identity Int)) MyRBSet) -> Proxy 'True
testRB19 = id
testRB20 :: Proxy (RB.Member (Either Bool (Identity Char)) MyRBSet) -> Proxy 'True
testRB20 = id
testRB21 :: Proxy (RB.Member (Either Bool (Identity Bool)) MyRBSet) -> Proxy 'True
testRB21 = id

testRB102 :: Proxy (RB.Member Float MyRBSet) -> Proxy 'False
testRB102 = id  -- False is not a member :(

--
-- Test MyReducedRBSet
testRB201 :: Proxy (RB.Member (Bool) MyReducedRBSet) -> Proxy 'False
testRB201 = id
testRB202 :: Proxy (RB.Member (String) MyReducedRBSet) -> Proxy 'False
testRB202 = id
testRB203 :: Proxy (RB.Member (Either Int Int) MyReducedRBSet) -> Proxy 'False
testRB203 = id
testRB204 :: Proxy (RB.Member (Either Char Int) MyReducedRBSet) -> Proxy 'False
testRB204 = id
testRB205 :: Proxy (RB.Member (Either Bool (Identity Int)) MyReducedRBSet) -> Proxy 'False
testRB205 = id
testRB206 :: Proxy (RB.Member (Either Bool (Identity Char)) MyReducedRBSet) -> Proxy 'False
testRB206 = id
testRB207 :: Proxy (RB.Member (Either Bool (Identity Bool)) MyReducedRBSet) -> Proxy 'False
testRB207 = id

-- the rest remain
testRB303 :: Proxy (RB.Member (Maybe Int) MyReducedRBSet) -> Proxy 'True
testRB303 = id
testRB304 :: Proxy (RB.Member (Maybe Char) MyReducedRBSet) -> Proxy 'True
testRB304 = id
testRB305 :: Proxy (RB.Member (Char) MyReducedRBSet) -> Proxy 'True
testRB305 = id
testRB308 :: Proxy (RB.Member (Either Bool Int) MyReducedRBSet) -> Proxy 'True
testRB308 = id
testRB309 :: Proxy (RB.Member (Maybe Bool) MyReducedRBSet) -> Proxy 'True
testRB309 = id
testRB310 :: Proxy (RB.Member (Identity Int) MyReducedRBSet) -> Proxy 'True
testRB310 = id
testRB311 :: Proxy (RB.Member (Identity Char) MyReducedRBSet) -> Proxy 'True
testRB311 = id
testRB312 :: Proxy (RB.Member (Identity Bool) MyReducedRBSet) -> Proxy 'True
testRB312 = id
testRB313 :: Proxy (RB.Member (Either Int (Identity Int)) MyReducedRBSet) -> Proxy 'True
testRB313 = id
testRB314 :: Proxy (RB.Member (Either Int (Identity Char)) MyReducedRBSet) -> Proxy 'True
testRB314 = id
testRB315 :: Proxy (RB.Member (Either Int (Identity Bool)) MyReducedRBSet) -> Proxy 'True
testRB315 = id
testRB316 :: Proxy (RB.Member (Either Char (Identity Int)) MyReducedRBSet) -> Proxy 'True
testRB316 = id
testRB317 :: Proxy (RB.Member (Either Char (Identity Char)) MyReducedRBSet) -> Proxy 'True
testRB317 = id
testRB318 :: Proxy (RB.Member (Either Char (Identity Bool)) MyReducedRBSet) -> Proxy 'True
testRB318 = id

-- Tests for MyMergedRBSet
testRB400 :: Proxy (RB.Member (Const Int Bool) MyMergedRBSet) -> Proxy 'True
testRB400 = id
testRB401 :: Proxy (RB.Member (Const Int Char) MyMergedRBSet) -> Proxy 'True
testRB401 = id
testRB402 :: Proxy (RB.Member (Const Int String) MyMergedRBSet) -> Proxy 'True
testRB402 = id

testRB403 :: Proxy (RB.Member (Maybe Int) MyMergedRBSet) -> Proxy 'True
testRB403 = id
testRB404 :: Proxy (RB.Member (Maybe Char) MyMergedRBSet) -> Proxy 'True
testRB404 = id
testRB405 :: Proxy (RB.Member (Char) MyMergedRBSet) -> Proxy 'True
testRB405 = id
testRB408 :: Proxy (RB.Member (Either Bool Int) MyMergedRBSet) -> Proxy 'True
testRB408 = id
testRB409 :: Proxy (RB.Member (Maybe Bool) MyMergedRBSet) -> Proxy 'True
testRB409 = id
testRB410 :: Proxy (RB.Member (Identity Int) MyMergedRBSet) -> Proxy 'True
testRB410 = id
testRB411 :: Proxy (RB.Member (Identity Char) MyMergedRBSet) -> Proxy 'True
testRB411 = id
testRB412 :: Proxy (RB.Member (Identity Bool) MyMergedRBSet) -> Proxy 'True
testRB412 = id
testRB413 :: Proxy (RB.Member (Either Int (Identity Int)) MyMergedRBSet) -> Proxy 'True
testRB413 = id
testRB414 :: Proxy (RB.Member (Either Int (Identity Char)) MyMergedRBSet) -> Proxy 'True
testRB414 = id
testRB415 :: Proxy (RB.Member (Either Int (Identity Bool)) MyMergedRBSet) -> Proxy 'True
testRB415 = id
testRB416 :: Proxy (RB.Member (Either Char (Identity Int)) MyMergedRBSet) -> Proxy 'True
testRB416 = id
testRB417 :: Proxy (RB.Member (Either Char (Identity Char)) MyMergedRBSet) -> Proxy 'True
testRB417 = id
testRB418 :: Proxy (RB.Member (Either Char (Identity Bool)) MyMergedRBSet) -> Proxy 'True
testRB418 = id

main :: IO ()
main = putStrLn "It compiled!"

