{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

module Plugin (plugin) where

import           Class
import           Data.Function
import           Data.Functor
import           Data.Generics (everywhere, mkT)
import           Data.IORef
import           Data.List
import           Data.Maybe
import qualified Data.Set as S
import           GHC.NameViolation
import           GHC.TcPluginM.Extra (lookupModule, lookupName, evByFiat)
import           GhcPlugins
import           TcEvidence
import           TcPluginM
import           TcPluginM (TcPluginM, tcLookupTyCon)
import           TcRnTypes


replaceCmpType :: TyCon -> Type -> Type
replaceCmpType cmpType t =
  case splitTyConApp_maybe t of
    Just (tc, [_, a, b]) | tc == cmpType -> doCompare a b
    _ -> t



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

doCompare :: Type -> Type -> Type
doCompare a b = flip mkTyConApp [] $
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
        Just i -> show i
        Nothing ->
          case isStrLitTy t of
            Just str -> unpackFS str
            Nothing -> error "unknown sort of thing"



mkWanted' :: TyCon -> Ct -> TcPluginM (Maybe (OrdType, ((EvTerm, Ct), Ct)))
mkWanted' cmpType ct =
  case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred NomEq t1 t2 -> do
      let t = everywhere (mkT $ replaceCmpType cmpType) t1
      case t1 `eqType` t of
        True -> pure Nothing
        False -> do
          let Just ev = evMagic ct
          ct' <- CNonCanonical <$> newWanted (ctLoc ct) (mkPrimEqPredRole Nominal t t2)
          pure $ Just (OrdType t, ((ev, ct), ct'))

    ClassPred cls ts -> do
      let ts' = everywhere (mkT $ replaceCmpType cmpType) ts
      case and $ zipWith eqType ts ts' of
        True -> pure Nothing
        False -> do
          let t = mkTyConApp (classTyCon cls) ts'
          ct' <- newWanted (ctLoc ct) t
          pure $ Just (OrdType t, ((ctEvTerm $ ct', ct), CDictCan ct' cls ts' False))

    _                  -> pure Nothing


solve
    :: (TyCon, IORef (S.Set OrdType))
    -> [Ct]  -- given
    -> [Ct]  -- derived
    -> [Ct]  -- wanted
    -> TcPluginM TcPluginResult
solve _ _ _ [] = pure $ TcPluginOk [] []
solve (cmpType, ref) _ _ wanted = do
  -- pprTraceM "wanted" $ vcat $ fmap ppr $ wanted
  unifications <- catMaybes <$> traverse (mkWanted' cmpType) wanted

  already_emitted <- tcPluginIO $ readIORef ref
  let unifications' = filter (not . flip S.member already_emitted . fst) unifications
  tcPluginIO $ modifyIORef ref $ S.union $ S.fromList $ fmap fst unifications'

  -- pprTraceM "emitting" $ vcat $ fmap (ppr . snd . snd) unifications
  pure $ TcPluginOk (fmap (fst . snd) unifications') $ fmap (snd . snd) unifications'


newtype OrdType = OrdType
  { getOrdType :: Type
  }

instance Eq OrdType where
  (==) = eqType `on` getOrdType

instance Ord OrdType where
  compare = nonDetCmpType `on` getOrdType


evMagic :: Ct -> Maybe EvTerm
evMagic ct =
  case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred NomEq t1 t2 -> Just (evByFiat "type-sets" t1 t2)
    _                  -> Nothing

