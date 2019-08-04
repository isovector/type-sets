{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall      #-}

module Plugin (plugin) where

import Data.Function (on)
import Data.Functor ((<&>))
import Data.Generics (everything, mkQ)
import Data.List (intercalate)
import Data.Traversable (for)
import GHC.NameViolation (violateName, showName)
import GHC.TcPluginM.Extra (lookupModule, lookupName, evByFiat)
import GhcPlugins
import TcEvidence (EvTerm (..))
import TcPluginM (TcPluginM, tcLookupTyCon, newGiven)
import TcRnTypes


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
  md <- lookupModule (mkModuleName "CmpType") $ fsLit "type-sets"
  nm <- lookupName md $ mkTcOcc "CmpType"
  tcLookupTyCon nm


promoteOrdering :: Ordering -> Type
promoteOrdering = flip mkTyConApp [] . \case
   LT -> promotedLTDataCon
   EQ -> promotedEQDataCon
   GT -> promotedGTDataCon


doCompare :: Type -> Type -> Type
doCompare a = promoteOrdering . on compare hash a


hash :: Type -> String
hash t =
  case splitTyConApp_maybe t of
    Just (c, as) ->
      let cName = getName c
          aNames = as <&> \a -> maybe (hash a)
                                      (showName . violateName . getName)
                              $ getTyVar_maybe a
       in intercalate " " $ showName (violateName cName) : aNames
    Nothing ->
      case isNumLitTy t of
        Just i -> show i
        Nothing ->
          case isStrLitTy t of
            Just str -> unpackFS str
            Nothing -> error "unknown sort of thing"


solve
    :: TyCon
    -> [Ct]  -- given
    -> [Ct]  -- derived
    -> [Ct]  -- wanted
    -> TcPluginM TcPluginResult
solve _ _ _ [] = pure $ TcPluginOk [] []
solve cmp_type _ _ wanted = do
  let rel = fmap (findRelevant cmp_type . ctLoc <*> ctev_pred . cc_ev) wanted

  gs <- for (concat rel) $ \z -> do
    let t = cmpTypeType z
        res = doCompare (cmpTypeA z) (cmpTypeB z)
        EvExpr ev = evByFiat "type-sets" t res
    newGiven (cmpTypeLoc z) (mkPrimEqPred t res) ev

  pure $ TcPluginOk [] $ fmap CNonCanonical gs


data CompareType = CompareType
  { cmpTypeA :: Type
  , cmpTypeB :: Type
  , cmpTypeType :: Type
  , cmpTypeLoc :: CtLoc
  }


findRelevant :: TyCon -> CtLoc ->  Type -> [CompareType]
findRelevant cmp_type loc = everything (++) $ mkQ [] findCmpType
  where
    findCmpType t =
      case splitTyConApp_maybe t of
        Just (tc, [_, a, b]) | tc == cmp_type -> [CompareType a b t loc]
        _ -> []

