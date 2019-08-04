{-# LANGUAGE BangPatterns #-}

module Plugin where

import TcPluginM
import TcType
import TcEvidence
import TcSMonad
import Data.Functor
import Data.List
import GHC.NameViolation
import GhcPlugins
import Plugins
import TcRnTypes
import GHC.TcPluginM.Extra (lookupModule, lookupName)
import TcPluginM (TcPluginM, tcLookupClass, tcLookupTyCon)
import Data.Generics (mkQ, everything)


-- TODO(sandy): this thing doesn't find cmptypes inside cmptypes :(
findCmpType :: TyCon -> CtLoc -> Type -> [CompareType]
findCmpType cmpType loc t =
  case splitTyConApp_maybe t of
    Just (tc, [k, a, b]) | tc == cmpType -> [CompareType k a b t loc]
    _ -> []


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


data CompareType = CompareType
  { cmpTypeKind :: Kind
  , cmpTypeA :: Type
  , cmpTypeB :: Type
  , cmpTypeType :: Type
  , cmpTypeLoc :: CtLoc
  }

instance Outputable CompareType

doCompare :: CompareType -> Type
doCompare (CompareType _ a b _ _) = flip mkTyConApp [] $
  case hash a `compare` hash b of
    LT -> promotedLTDataCon
    EQ -> promotedEQDataCon
    GT -> promotedGTDataCon


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
        Just int -> "$$intLit:" ++ show int
        Nothing ->
          case isStrLitTy t of
            Just str -> "$$strLit:" ++ unpackFS str
            Nothing -> error "unknown sort of thing"


findRelevants :: TyCon -> CtLoc ->  Type -> [CompareType]
findRelevants tyCon loc t =
  everything (++) (mkQ [] (findCmpType tyCon loc)) t


mkWanted :: TyCon -> CompareType -> TcPluginM (Ct, Coercion)
mkWanted cmpType cmp = do
  (ev, c) <- unsafeTcPluginTcM
             . runTcSDeriveds
             $ newWantedEq
                 (cmpTypeLoc cmp)
                 Nominal
                 (cmpTypeType cmp)
                 (doCompare cmp)
  -- uniq <- tcPluginIO $ fmap uniqFromSupply $ mkSplitUniqSupply 'z'
  -- let var = mkTcTyVar (mkSystemName uniq $ mkVarOcc "hello") (cmpTypeKind cmp) vanillaSkolemTv
  pure ( -- CFunEqCan ev cmpType [cmpTypeKind cmp, cmpTypeA cmp, cmpTypeB cmp] var
         CNonCanonical ev
       , c
       )


solve
    :: TyCon
    -> [Ct]  -- given
    -> [Ct]  -- derived
    -> [Ct]  -- wanted
    -> TcPluginM TcPluginResult
solve cmpType given derivs wanted = do
  zs <- pure $ concat $ fmap (\ct -> findRelevants cmpType (ctLoc ct) $ ctev_pred $ cc_ev ct) wanted
  unifications <- traverse (mkWanted cmpType) zs

  pprTraceM "emitting" $ vcat $ fmap (ppr . fst) unifications


  pure $ TcPluginOk (fmap (\(ct, c) -> (evCoercion c, ct)) unifications) []

--   pprPanic "uh oh" $ ppr $ given ++ derivs ++ wanted

  -- pure $ TcPluginOk
  --         []
  --         [ CFunEqCan
  --             (error "ev")
  --             cmpType
  --             [mkNumLitTy 5, mkNumLitTy 5]
  --             undefined
  --         ]



-- isCmpType :: TyCon -> Ct -> Bool


