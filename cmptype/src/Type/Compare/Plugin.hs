{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall      #-}

module Type.Compare.Plugin (plugin) where

import Control.Applicative (liftA2)
import Control.Monad (guard)
import Data.Generics (everything, mkQ)
import Data.List (intercalate)
import Data.Traversable (for)
import GHC.NameViolation (violateName, showName)
import GHC.TcPluginM.Extra (lookupModule, lookupName, evByFiat)
import GhcPlugins
import TcEvidence (EvTerm (..))
import TcPluginM (TcPluginM, tcLookupTyCon, newGiven)
import TcRnTypes
import TcType (isTyFamFree)


plugin :: Plugin
plugin = defaultPlugin
  { tcPlugin = const $ Just $ TcPlugin
      { tcPluginInit = getCmpType
      , tcPluginSolve = solve
      , tcPluginStop = const $ pure ()
      }
  }


getCmpType :: TcPluginM TyCon
getCmpType = do
  md <- lookupModule (mkModuleName "Type.Compare") $ fsLit "cmptype"
  nm <- lookupName md $ mkTcOcc "CmpType"
  tcLookupTyCon nm


promoteOrdering :: Ordering -> Type
promoteOrdering = flip mkTyConApp [] . \case
   LT -> promotedLTDataCon
   EQ -> promotedEQDataCon
   GT -> promotedGTDataCon


doCompare :: Type -> Type -> Maybe Type
doCompare a b = fmap promoteOrdering $ liftA2 compare (hash a) (hash b)


hash :: Type -> Maybe String
hash t = do
  guard $ isTyFamFree t
  (c, as) <- splitTyConApp_maybe t
  hs <- traverse hash as
  pure $ intercalate " " $ showName (violateName $ getName c) : hs


solve
    :: TyCon
    -> [Ct]  -- given
    -> [Ct]  -- derived
    -> [Ct]  -- wanted
    -> TcPluginM TcPluginResult
solve _ _ _ [] = pure $ TcPluginOk [] []
solve cmp_type _ _ wanted = do
  let rel = fmap (findRelevant cmp_type . ctLoc <*> ctev_pred . cc_ev) wanted

  gs <- for (concat rel) $ \(CompareType t res loc) -> do
    let EvExpr ev = evByFiat "cmptype-plugin" t res
    newGiven loc (mkPrimEqPred t res) ev

  pure $ TcPluginOk [] $ fmap CNonCanonical gs


data CompareType = CompareType
  { _cmpTypeType :: Type
  , _cmpTypeNew :: Type
  , _cmpTypeLoc :: CtLoc
  }


findRelevant :: TyCon -> CtLoc ->  Type -> [CompareType]
findRelevant cmp_type loc = everything (++) $ mkQ [] findCmpType
  where
    findCmpType t =
      case splitTyConApp_maybe t of
        Just (tc, [_, a, b]) | tc == cmp_type ->
           maybe [] (\res -> [CompareType t res loc]) $ doCompare a b
        _ -> []

