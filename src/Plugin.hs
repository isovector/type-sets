{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TupleSections #-}

module Plugin where

import qualified Data.Set as S
import Data.IORef
import Data.Function
import Data.Maybe
import TcPluginM
import TcType
import TcEvidence
-- import TcSMonad
import Data.Functor
import Data.List
import GHC.NameViolation
import GhcPlugins
import Plugins
import TcRnTypes
import GHC.TcPluginM.Extra (lookupModule, lookupName, evByFiat)
import TcPluginM (TcPluginM, tcLookupClass, tcLookupTyCon)
import Data.Generics (mkQ, everything, everywhere, mkT)

replaceCmpType :: TyCon -> Type -> Type
replaceCmpType cmpType t =
  case splitTyConApp_maybe t of
    Just (tc, [k, a, b]) | tc == cmpType -> doCompare $ CompareType k a b t undefined
    _ -> t


-- TODO(sandy): this thing doesn't find cmptypes inside cmptypes :(
findCmpType :: TyCon -> CtLoc -> Type -> [CompareType]
findCmpType cmpType loc t =
  case splitTyConApp_maybe t of
    Just (tc, [k, a, b]) | tc == cmpType -> [CompareType k a b t loc]
    _ -> []


plugin :: Plugin
plugin = defaultPlugin
  { tcPlugin = const $ Just $ TcPlugin
      { tcPluginInit = (,) <$> getCmpType <*> tcPluginIO (newIORef S.empty)
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


mkWanted' :: TyCon -> Ct -> TcPluginM (Maybe (OrdType, (Ct, Ct)))
mkWanted' cmpType ct =
  case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred NomEq t1 t2 -> fmap Just $ do
      let t = everywhere (mkT $ replaceCmpType cmpType) t1
      ct' <- CNonCanonical <$> newWanted (ctLoc ct) (mkPrimEqPredRole Nominal t t2)
      pure (OrdType t, (ct, ct'))

    _                  -> pure Nothing


--   (ev, _) <- unsafeTcPluginTcM
--              . runTcSDeriveds
--              $ newWantedEq
--                  (cmpTypeLoc cmp)
--                  Nominal
--                  (cmpTypeType cmp)
--                  (doCompare cmp)
--   -- uniq <- tcPluginIO $ fmap uniqFromSupply $ mkSplitUniqSupply 'z'
--   -- let var = mkTcTyVar (mkSystemName uniq $ mkVarOcc "hello") (cmpTypeKind cmp) vanillaSkolemTv
--   pure ( -- CFunEqCan ev cmpType [cmpTypeKind cmp, cmpTypeA cmp, cmpTypeB cmp] var
--          CNonCanonical ev
--        )

solve
    :: (TyCon, IORef (S.Set OrdType))
    -> [Ct]  -- given
    -> [Ct]  -- derived
    -> [Ct]  -- wanted
    -> TcPluginM TcPluginResult
solve (cmpType, ref) given derivs wanted = do
  pprTraceM "wanted" $ vcat $ fmap ppr $ wanted
  unifications <- catMaybes <$> traverse (mkWanted' cmpType) wanted

  already_emitted <- tcPluginIO $ readIORef ref
  let unifications' = filter (not . flip S.member already_emitted . fst) unifications
  tcPluginIO $ modifyIORef ref $ S.union $ S.fromList $ fmap fst unifications'

  -- let unifications' =

  pprTraceM "emitting" $ vcat $ fmap (ppr . snd . snd) unifications



  pure $ TcPluginOk (mapMaybe ((\c -> (, c) <$> evMagic c) . fst . snd) unifications') $ fmap (snd . snd) unifications'


newtype OrdType = OrdType
  { getOrdType :: Type
  }

instance Eq OrdType where
  (==) = eqType `on` getOrdType

instance Ord OrdType where
  compare = nonDetCmpType `on` getOrdType



--   zs <- pure $ concat $ fmap (\ct -> findRelevants cmpType (ctLoc ct) $ ctev_pred $ cc_ev ct) wanted

--   pprTraceM "emitting" $ vcat $ fmap ppr $ mapMaybe (\ct -> (, ct) <$> evMagic ct) unifications


--   pure $ TcPluginOk (mapMaybe (\ct -> (, ct) <$> evMagic ct) unifications) []

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


evMagic :: Ct -> Maybe EvTerm
evMagic ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred NomEq t1 t2 -> Just (evByFiat "type-sets" t1 t2)
    _                  -> Nothing

