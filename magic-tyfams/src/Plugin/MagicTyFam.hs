{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall      #-}

module Plugin.MagicTyFam
  ( magicTyFamPlugin

    -- * Semantics
  , withStuckSemantics
  , isTyFamFree

    -- * Low level stuff
  , solveWanteds
  , loadTyCon

    -- * Re-exports
  , Plugin
  , TyCon
  , Type
  ) where

import Control.Monad (guard)
import Data.Generics (everything, mkQ)
import Data.Traversable (for)
import GHC.TcPluginM.Extra (lookupModule, lookupName, evByFiat)
import GhcPlugins
import TcEvidence (EvTerm (..))
import TcPluginM (TcPluginM, tcLookupTyCon, newGiven)
import TcRnTypes
import TcType (isTyFamFree)


------------------------------------------------------------------------------
-- | Get your hands on the type constructor for your type family.
loadTyCon
    :: String  -- ^ package
    -> String  -- ^ module
    -> String  -- ^ identifier
    -> TcPluginM TyCon
loadTyCon p m i = do
  md <- lookupModule (mkModuleName m) $ fsLit p
  nm <- lookupName md $ mkTcOcc i
  tcLookupTyCon nm


------------------------------------------------------------------------------
-- | Generate a plugin that solves instances of the type family pointed at by
-- the three strings, via the function.
magicTyFamPlugin
    :: String  -- ^ package
    -> String  -- ^ module
    -> String  -- ^ identifier
    -> ([Type] -> Maybe Type)
    -> Plugin
magicTyFamPlugin p m i f = defaultPlugin
  { tcPlugin = const $ Just $ TcPlugin
      { tcPluginInit = loadTyCon p m i
      , tcPluginSolve = \cmp_type _ _ w ->
          TcPluginOk [] <$> solveWanteds f cmp_type w
      , tcPluginStop = const $ pure ()
      }
  , pluginRecompile = const $ pure NoForceRecompile
  }


------------------------------------------------------------------------------
-- | Ensure that your resulting type family preserves the stuckness property,
-- namely that it is stuck if any of its children are stuck.
--
-- You can use 'isTyFamFree' instead of 'withStuckSemantics' if you want tigher
-- control over when your magic type family should be stuck.
withStuckSemantics :: ([Type] -> Maybe Type) -> [Type] -> Maybe Type
withStuckSemantics f ts = do
  guard $ all isTyFamFree ts
  f ts


------------------------------------------------------------------------------
-- | @solveWanteds f tyfam cts@ finds all instaces of @tyfam@ inside the wanted
-- constraints @cts@, and evaluates them via @f@. The result is a set of
-- 'CNonCanonical' constraints, which should be emitted as the second parameter
-- of 'TcPluginOk'.
solveWanteds :: ([Type] -> Maybe Type) -> TyCon -> [Ct] -> TcPluginM [Ct]
solveWanteds _ _ [] = pure []
solveWanteds f cmp_type wanted = do
  let rel = fmap (findRelevant f cmp_type . ctLoc <*> ctev_pred . cc_ev) wanted

  gs <- for (concat rel) $ \(MagicTyFamResult loc t res) -> do
    let EvExpr ev = evByFiat "magic-tyfams" t res
    newGiven loc (mkPrimEqPred t res) ev

  pure $ fmap CNonCanonical gs


------------------------------------------------------------------------------
-- | Locate and expand the use of any type families.
findRelevant :: ([Type] -> Maybe Type) -> TyCon -> CtLoc -> Type -> [MagicTyFamResult]
findRelevant f cmp_type loc = everything (++) $ mkQ [] findCmpType
  where
    findCmpType t =
      case splitTyConApp_maybe t of
        Just (tc, ts) | tc == cmp_type ->
           maybe [] (pure . MagicTyFamResult loc t) $ f ts
        _ -> []


data MagicTyFamResult = MagicTyFamResult
  { _mtfrLoc      :: CtLoc
  , _mtfrOriginal :: Type
  , _mtfrSolved   :: Type
  }

