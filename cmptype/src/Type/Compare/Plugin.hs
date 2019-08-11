{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall      #-}

module Type.Compare.Plugin (plugin) where

import Plugin.MagicTyFam (magicTyFamPlugin, isTyFamFree)
import Control.Applicative (liftA2)
import Control.Monad (guard)
import Data.List (intercalate)
import GHC.NameViolation (violateName, showName)
import GhcPlugins


------------------------------------------------------------------------------
-- | This plugin automagically solves 'Type.Compare.CmpType'. Enable the GHC
-- flag @-fplugin=Type.Compare.Plugin@ in order to use it.
plugin :: Plugin
plugin = magicTyFamPlugin "cmptype" "Type.Compare" "CmpTypeImpl" $ \[_, a, b] ->
  fmap promoteOrdering $ liftA2 compare (hash a) (hash b)


promoteOrdering :: Ordering -> Type
promoteOrdering = flip mkTyConApp [] . \case
   LT -> promotedLTDataCon
   EQ -> promotedEQDataCon
   GT -> promotedGTDataCon


hash :: Type -> Maybe String
hash t = do
  guard $ isTyFamFree t
  (c, as) <- splitTyConApp_maybe t
  hs <- traverse hash as
  pure $ intercalate " " $ showName (violateName $ getName c) : hs

