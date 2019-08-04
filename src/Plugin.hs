{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall      #-}

module Plugin (plugin) where

import           Class
import           Data.Data (Data)
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


plugin :: Plugin
plugin = defaultPlugin
  { tcPlugin = const $ Just $ TcPlugin
      { tcPluginInit = (,) <$> getCmpType <*> tcPluginIO (newIORef S.empty)
      , tcPluginSolve = solve
      , tcPluginStop = const $ pure ()
      }
  }


replaceCmpType :: Data a => TyCon -> a -> a
replaceCmpType cmp_type = everywhere $ mkT $ \t ->
  case splitTyConApp_maybe t of
    Just (tc, [_, a, b]) | tc == cmp_type -> doCompare a b
    _ -> t


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


data RewrittenWanted = RewrittenWanted
  { rwOrig :: OrdType
  , rwSolved :: (EvTerm, Ct)
  , rwNew :: Ct
  }


mkWanted' :: TyCon -> Ct -> TcPluginM (Maybe RewrittenWanted)
mkWanted' cmp_type ct =
  case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred NomEq t1 t2 -> do
      let t = replaceCmpType cmp_type t1
      case t1 `eqType` t of
        True -> pure Nothing
        False -> do
          let ev = evByFiat "type-sets" t1 t2
          ct' <- newWanted (ctLoc ct) (mkPrimEqPredRole Nominal t t2)
          pure $ Just $ RewrittenWanted (OrdType t) (ev, ct) $ CNonCanonical ct'

    ClassPred cls ts -> do
      let ts' = replaceCmpType cmp_type ts
      case and $ zipWith eqType ts ts' of
        True -> pure Nothing
        False -> do
          let t = mkTyConApp (classTyCon cls) ts'
          ct' <- newWanted (ctLoc ct) t
          pure $ Just $ RewrittenWanted (OrdType t) (ctEvTerm $ ct', ct) $ CDictCan ct' cls ts' False

    _                  -> pure Nothing


solve
    :: (TyCon, IORef (S.Set OrdType))
    -> [Ct]  -- given
    -> [Ct]  -- derived
    -> [Ct]  -- wanted
    -> TcPluginM TcPluginResult
solve _ _ _ [] = pure $ TcPluginOk [] []
solve (cmp_type, ref) _ _ wanted = do
  already_emitted <- tcPluginIO $ readIORef ref

  rewritten <- catMaybes <$> traverse (mkWanted' cmp_type) wanted
  let filtered = filter (not . flip S.member already_emitted . rwOrig) rewritten
      solved = fmap rwSolved filtered
      to_emit = fmap rwNew filtered

  -- pprTraceM "wanted" $ vcat $ fmap ppr $ wanted
  -- pprTraceM "emitting" $ vcat $ fmap ppr to_emit

  tcPluginIO $ modifyIORef ref $ S.union $ S.fromList $ fmap rwOrig filtered
  pure $ TcPluginOk solved to_emit


newtype OrdType = OrdType
  { getOrdType :: Type
  }

instance Eq OrdType where
  (==) = eqType `on` getOrdType

instance Ord OrdType where
  compare = nonDetCmpType `on` getOrdType


