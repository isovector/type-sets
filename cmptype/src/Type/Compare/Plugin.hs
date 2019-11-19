{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall   #-}

module Type.Compare.Plugin (plugin) where

import Control.Monad (guard)
import Data.Maybe (fromMaybe)
import FastString (unpackFS)
import GHC.NameViolation (violateName, showName)
import GhcPlugins hiding ((<>))
import Plugin.MagicTyFam (magicTyFamPlugin, isTyFamFree)
import TyCoRep

------------------------------------------------------------------------------
-- | This plugin automagically solves 'Type.Compare.CmpType'. Enable the GHC
-- flag @-fplugin=Type.Compare.Plugin@ in order to use it.
plugin :: Plugin
plugin = magicTyFamPlugin "cmptype" "Type.Compare" "CmpTypeImpl" $ \[_, a, b] -> do
  guard $ isTyFamFree a
  guard $ isTyFamFree b
  pure $ promoteOrdering $ compareTypes a b

promoteOrdering :: Ordering -> Type
promoteOrdering = flip mkTyConApp [] . \case
   LT -> promotedLTDataCon
   EQ -> promotedEQDataCon
   GT -> promotedGTDataCon

-- Note: pprTraceIt function is quite useful for debugging this if it goes awry.

compareTypes :: Type -> Type -> Ordering
compareTypes lty rty =
 case (applyTcView lty, applyTcView rty) of
  (TyVarTy{}, _) -> tyVarError
  (_, TyVarTy{}) -> tyVarError

  (AppTy f x, AppTy g y) ->
    compareTypes f g <> compareTypes x y

  (TyConApp lc la, TyConApp rc ra) ->
    compareTyCons lc rc <> compareArgs la ra

  (ForAllTy{}, _) -> forallError
  (_, ForAllTy{}) -> forallError

  (FunTy li lo, FunTy ri ro) ->
    compareTypes li ri <> compareTypes lo ro

  (LitTy l, LitTy r) ->
    compareTyLits l r

  (CastTy{}, _) -> castError
  (_, CastTy{}) -> castError

  (CoercionTy{}, _) -> coercionError
  (_, CoercionTy{}) -> coercionError

  (AppTy{}, _) -> LT
  (_, AppTy{}) -> GT

  (TyConApp{}, _) -> LT
  (_, TyConApp{}) -> GT

  (FunTy{}, _) -> LT
  (_, FunTy{}) -> GT

compareTyCons :: TyCon -> TyCon -> Ordering
compareTyCons l r = compare (tyConToString l) (tyConToString r)

tyConToString :: TyCon -> String
tyConToString = showName . violateName . getName

compareArgs :: [Type] -> [Type] -> Ordering
compareArgs [] [] = EQ
compareArgs [] _ = GT
compareArgs _ [] = LT
compareArgs (l : ls) (r : rs) =
  compareTypes l r <> compareArgs ls rs

compareTyLits :: TyLit -> TyLit -> Ordering
compareTyLits (NumTyLit l) (NumTyLit r) = compare l r
compareTyLits (StrTyLit l) (StrTyLit r) = compare (unpackFS l) (unpackFS r)

compareTyLits NumTyLit{} _ = LT
compareTyLits _ NumTyLit{} = GT

applyTcView :: Type -> Type
applyTcView ty = fromMaybe ty $ tcView ty

forallError, tyVarError, castError, coercionError :: a
forallError = error "CmpType does not currently support impredicative types"

-- TODO: Can any of these happen in practice?
tyVarError = error "Unexpected: CmpType encountered ty var"
castError = error "Unexpected: CmpType encountered kind cast"
coercionError = error "Unexpected: CmpType encountered coercion"
