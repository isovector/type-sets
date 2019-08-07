{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -fplugin=Type.Compare.Plugin      #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

import Data.Proxy
import Type.Set

type MySet = Insert Bool (Insert String (Insert (Maybe Int) 'Empty))

test1 :: Proxy (Member Bool MySet) -> Proxy 'True
test1 = id  -- Bool is a member :)

test2 :: Proxy (Member Char MySet) -> Proxy 'False
test2 = id  -- False is not a member :(

main :: IO ()
main = putStrLn "It compiled!"

