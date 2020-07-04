{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.Infer
  ( startInference,
    doInference,
  )
where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State (State, get, put, runState)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import Language.Mimsa.Library
import Language.Mimsa.Types

type App = ExceptT TypeError (State Int)

startInference :: Expr -> Either TypeError MonoType
startInference expr = snd <$> doInference M.empty expr

doInference :: Environment -> Expr -> Either TypeError (Substitutions, MonoType)
doInference env expr =
  fst either'
  where
    either' = runState (runExceptT (infer env expr)) 1

inferLiteral :: Literal -> App (Substitutions, MonoType)
inferLiteral (MyInt _) = pure (mempty, MTInt)
inferLiteral (MyBool _) = pure (mempty, MTBool)
inferLiteral (MyString _) = pure (mempty, MTString)
inferLiteral (MyUnit) = pure (mempty, MTUnit)

inferBuiltIn :: Name -> App (Substitutions, MonoType)
inferBuiltIn name = case getLibraryFunction name of
  Just ff -> pure (mempty, getFFType ff)
  _ -> throwError $ MissingBuiltIn name

instantiate :: Scheme -> App MonoType
instantiate (Scheme vars ty) = do
  newVars <- traverse (const getUnknown) vars
  let subst = M.fromList (zip vars newVars)
  pure (applySubst subst ty)

applySubstScheme :: Substitutions -> Scheme -> Scheme
applySubstScheme subst (Scheme vars t) =
  -- The fold takes care of name shadowing
  Scheme vars (applySubst (foldr M.delete subst vars) t)

applySubstCtx :: Substitutions -> Environment -> Environment
applySubstCtx subst ctx = M.map (applySubstScheme subst) ctx

applySubst :: Substitutions -> MonoType -> MonoType
applySubst subst ty = case ty of
  MTVar i ->
    fromMaybe (MTVar i) (M.lookup i subst)
  MTFunction arg res ->
    MTFunction (applySubst subst arg) (applySubst subst res)
  MTPair a b ->
    MTPair
      (applySubst subst a)
      (applySubst subst b)
  MTList a -> MTList (applySubst subst a)
  MTRecord a -> MTRecord (applySubst subst <$> a)
  MTSum a b -> MTSum (applySubst subst a) (applySubst subst b)
  MTInt -> MTInt
  MTString -> MTString
  MTBool -> MTBool
  MTUnit -> MTUnit

composeSubst :: Substitutions -> Substitutions -> Substitutions
composeSubst s1 s2 = M.union (M.map (applySubst s1) s2) s1

inferVarFromScope :: Environment -> Name -> App (Substitutions, MonoType)
inferVarFromScope env name =
  case M.lookup name env of
    Just scheme -> do
      ty <- instantiate scheme
      pure (mempty, ty)
    _ -> throwError $ VariableNotInEnv name env

inferFuncReturn :: Environment -> Name -> Expr -> MonoType -> App (Substitutions, MonoType)
inferFuncReturn env binder function tyArg = do
  let scheme = Scheme [] tyArg
      newEnv = M.insert binder scheme env
  tyRes <- getUnknown
  (s1, tyFun) <- infer newEnv function
  s2 <- unify (MTFunction tyArg tyFun) (applySubst s1 tyRes)
  let s3 = mempty
      subs = s3 `composeSubst` s2 `composeSubst` s1
  pure (subs, applySubst subs tyFun)

--traceAnd :: (Show a) => Text -> a -> a
--traceAnd msg a = traceShow (msg, a) a

inferList :: Environment -> NonEmpty Expr -> App (Substitutions, MonoType)
inferList env (a :| as) = do
  (s1, tyA) <- infer env a
  let foldFn = \as' a' -> do
        (s', ty') <- as'
        (sA, tyB) <- infer env a'
        sB <- unify ty' tyB
        pure (sB `composeSubst` sA `composeSubst` s', applySubst sB tyB)
  foldl
    foldFn
    (pure (s1, tyA))
    as

splitRecordTypes ::
  Map Name (Substitutions, MonoType) ->
  (Substitutions, MonoType)
splitRecordTypes map' = (subs, MTRecord types)
  where
    subs =
      foldr
        composeSubst
        mempty
        ((fst . snd) <$> M.toList map')
    types = snd <$> map'

infer :: Environment -> Expr -> App (Substitutions, MonoType)
infer _ (MyLiteral a) = inferLiteral a
infer env (MyVar name) =
  (inferVarFromScope env name)
    <|> (inferBuiltIn name)
infer env (MyList as) = do
  (s1, tyItems) <- inferList env as
  pure (s1, MTList tyItems)
infer env (MyRecord map') = do
  tyRecord <- getUnknown
  (s1, tyResult) <- splitRecordTypes <$> traverse (infer env) map'
  s2 <- unify tyResult tyRecord
  pure
    ( s2 `composeSubst` s1,
      applySubst (s2 `composeSubst` s1) tyRecord
    )
infer env (MyLet binder expr body) = do
  (s1, tyExpr) <- infer env expr
  let scheme = Scheme [] (applySubst s1 tyExpr)
  let newEnv = M.insert binder scheme env
  (s2, tyBody) <- infer (applySubstCtx s1 newEnv) body
  pure (s2 `composeSubst` s1, tyBody)
infer env (MyRecordAccess (MyRecord items') name) = do
  case M.lookup name items' of
    Just item -> do
      infer env item
    Nothing ->
      throwError $ MissingRecordMember name items'
infer env (MyRecordAccess a name) = do
  (s1, tyItems) <- infer env a
  tyResult <- case tyItems of
    (MTRecord bits) -> do
      case M.lookup name bits of
        Just mt -> pure mt
        _ -> throwError $ MissingRecordTypeMember name bits
    (MTVar _) -> getUnknown
    _ -> throwError $ CannotMatchRecord env tyItems
  s2 <-
    unify
      (MTRecord $ M.singleton name tyResult)
      tyItems
  let subs = s2 `composeSubst` s1
  pure (subs, applySubst subs tyResult)
infer env (MyCase sumExpr (MyLambda binderL exprL) (MyLambda binderR exprR)) = do
  (s1, tySum) <- infer env sumExpr
  (tyL, tyR) <- case tySum of
    (MTSum tyL tyR) -> pure (tyL, tyR)
    (MTVar _a) -> do
      tyL <- getUnknown
      tyR <- getUnknown
      pure (tyL, tyR)
    otherType -> throwError $ CaseMatchExpectedSum otherType
  s2 <- unify (MTSum tyL tyR) tySum
  (s3, tyLeftRes) <- inferFuncReturn env binderL exprL (applySubst (s2 `composeSubst` s1) tyL)
  (s4, tyRightRes) <- inferFuncReturn env binderR exprR (applySubst (s2 `composeSubst` s1) tyR)
  let subs =
        s4 `composeSubst` s3
          `composeSubst` s2
          `composeSubst` s1
  s5 <- unify tyLeftRes tyRightRes
  pure (s5 `composeSubst` subs, applySubst (s5 `composeSubst` subs) tyLeftRes)
infer _env (MyCase _ l r) = throwError $ CaseMatchExpectedLambda l r
infer env (MyLetPair binder1 binder2 expr body) = do
  (s1, tyExpr) <- infer env expr
  (tyA, tyB) <- case tyExpr of
    (MTVar _a) -> do
      tyA <- getUnknown
      tyB <- getUnknown
      pure (tyA, tyB)
    (MTPair a b) -> pure (a, b)
    a -> throwError $ CaseMatchExpectedPair a
  let schemeA = Scheme [] (applySubst s1 tyA)
      schemeB = Scheme [] (applySubst s1 tyB)
      newEnv = M.insert binder1 schemeA (M.insert binder2 schemeB env)
  s2 <- unify tyExpr (MTPair tyA tyB)
  (s3, tyBody) <- infer (applySubstCtx (s2 `composeSubst` s1) newEnv) body
  pure (s3 `composeSubst` s2 `composeSubst` s1, tyBody)
infer env (MyLetList binder1 binder2 expr body) = do
  (s1, tyExpr) <- infer env expr
  (tyHead, tyRest) <- case tyExpr of
    (MTVar _a) -> do
      tyHead <- getUnknown
      tyRest <- getUnknown
      pure (tyHead, tyRest)
    (MTList tyHead) -> do
      tyRest <- getUnknown
      pure (tyHead, tyRest)
    a -> throwError $ CaseMatchExpectedList a
  let schemeHead = Scheme [] (applySubst s1 tyHead)
      schemeRest = Scheme [] (applySubst s1 tyRest)
      newEnv = M.insert binder1 schemeHead (M.insert binder2 schemeRest env)
  s2 <- unify tyExpr (MTList tyHead)
  s3 <- unify tyRest (MTSum MTUnit (MTList tyHead))
  (s4, tyBody) <- infer (applySubstCtx (s3 `composeSubst` s2 `composeSubst` s1) newEnv) body
  pure (s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` s1, tyBody)
infer env (MyLambda binder body) = do
  tyBinder <- getUnknown
  let tmpCtx = M.insert binder (Scheme [] tyBinder) env
  (s1, tyBody) <- infer tmpCtx body
  pure (s1, MTFunction (applySubst s1 tyBinder) tyBody)
infer env (MyApp function argument) = do
  tyRes <- getUnknown
  (s1, tyFun) <- infer env function
  (s2, tyArg) <- infer (applySubstCtx s1 env) argument
  s3 <- unify (applySubst s2 tyFun) (MTFunction tyArg tyRes)
  pure (s3 `composeSubst` s2 `composeSubst` s1, applySubst s3 tyRes)
infer env (MyIf condition thenCase elseCase) = do
  (s1, tyCond) <- infer env condition
  (s2, tyThen) <- infer (applySubstCtx s1 env) thenCase
  (s3, tyElse) <- infer (applySubstCtx (s2 `composeSubst` s1) env) elseCase
  s4 <- unify tyThen tyElse
  s5 <- unify tyCond MTBool
  let subs =
        s5 `composeSubst` s4 `composeSubst` s3
          `composeSubst` s2
          `composeSubst` s1
  pure
    ( subs,
      applySubst subs tyElse
    )
infer env (MyPair a b) = do
  (s1, tyA) <- infer env a
  (s2, tyB) <- infer (applySubstCtx s1 env) b
  pure (s2 `composeSubst` s1, MTPair tyA tyB)
infer env (MySum MyLeft left') = do
  tyRight <- getUnknown
  (s1, tyLeft) <- infer env left'
  pure (s1, MTSum tyLeft (applySubst s1 tyRight))
infer env (MySum MyRight right') = do
  tyLeft <- getUnknown
  (s1, tyRight) <- infer env right'
  pure (s1, MTSum (applySubst s1 tyLeft) tyRight)

freeTypeVars :: MonoType -> S.Set Name
freeTypeVars ty = case ty of
  MTVar var ->
    S.singleton var
  MTFunction t1 t2 ->
    S.union (freeTypeVars t1) (freeTypeVars t2)
  _ ->
    S.empty

-- | Creates a fresh unification variable and binds it to the given type
varBind :: Name -> MonoType -> App Substitutions
varBind var ty
  | ty == MTVar var = pure mempty
  | S.member var (freeTypeVars ty) =
    throwError $
      FailsOccursCheck var
  | otherwise = pure (M.singleton var ty)

unify :: MonoType -> MonoType -> App Substitutions
unify a b | a == b = pure mempty
unify (MTFunction l r) (MTFunction l' r') = do
  s1 <- unify l l'
  s2 <- unify (applySubst s1 r) (applySubst s1 r')
  pure (s2 `composeSubst` s1)
unify (MTPair a b) (MTPair a' b') = do
  s1 <- unify a a'
  s2 <- unify (applySubst s1 b) (applySubst s1 b')
  pure (s2 `composeSubst` s1)
unify (MTSum a b) (MTSum a' b') = do
  s1 <- unify a a'
  s2 <- unify (applySubst s1 b) (applySubst s1 b')
  pure (s2 `composeSubst` s1)
unify (MTVar u) t = varBind u t
unify (MTList a) (MTList a') = unify a a'
unify (MTRecord as) (MTRecord bs) = do
  let allKeys = S.toList $ M.keysSet as <> M.keysSet bs
  let getRecordTypes = \k -> do
        tyLeft <- getTypeOrFresh k as
        tyRight <- getTypeOrFresh k bs
        unify tyLeft tyRight
  s <- traverse getRecordTypes allKeys
  pure (foldl composeSubst mempty s)
unify t (MTVar u) = varBind u t
unify a b =
  throwError $ UnificationError a b

getTypeOrFresh :: Name -> Map Name MonoType -> App MonoType
getTypeOrFresh name map' = do
  case M.lookup name map' of
    Just found -> pure found
    _ -> getUnknown

getUnknown :: App MonoType
getUnknown = do
  nextUniVar <- get
  put (nextUniVar + 1)
  pure (MTVar (mkName $ "U" <> T.pack (show nextUniVar)))
