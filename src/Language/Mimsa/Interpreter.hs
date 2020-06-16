{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Interpreter
  ( interpret,
  )
where

-- let's run our code, at least for the repl
-- run == simplify, essentially
import Control.Monad.Except
import Control.Monad.Trans.State.Lazy
import qualified Data.Map as M
import Data.Text (Text)
import Language.Mimsa.Syntax
import Language.Mimsa.Types

interpret :: Scope -> Expr -> Either Text Expr
interpret scope' expr =
  (fst <$> either')
  where
    either' = runStateT (interpretWithScope expr) scope'

type App = StateT Scope (Either Text)

interpretWithScope :: Expr -> App Expr
interpretWithScope (MyLiteral a) = pure $ MyLiteral a
interpretWithScope (MyLet binder expr body) = do
  modify ((<>) (Scope $ M.singleton binder expr))
  interpretWithScope body
interpretWithScope (MyVar a) = do
  found <- gets (M.lookup a . getScope)
  case found of
    Just expr -> interpretWithScope expr
    Nothing -> throwError $ "Could not find " <> prettyPrint a
interpretWithScope (MyApp (MyVar f) value) = do
  expr <- interpretWithScope (MyVar f)
  interpretWithScope (MyApp expr value)
interpretWithScope (MyApp (MyLambda binder expr) value) =
  interpretWithScope (MyLet binder value expr)
interpretWithScope (MyApp (MyApp a b) c) = do
  expr <- interpretWithScope (MyApp a b)
  interpretWithScope (MyApp expr c)
interpretWithScope (MyApp (MyLet a b c) d) = do
  expr <- interpretWithScope (MyLet a b c)
  interpretWithScope (MyApp expr d)
interpretWithScope (MyApp (MyLiteral _) _) = throwError "Cannot apply a value to a literal value"
interpretWithScope (MyApp (MyIf _ _ _) _) = throwError "Cannot apply a value to an if"
interpretWithScope (MyLambda a b) = pure (MyLambda a b)
interpretWithScope (MyIf (MyLiteral (MyBool pred')) true false) =
  if pred'
    then interpretWithScope true
    else interpretWithScope false
interpretWithScope (MyIf (MyLiteral _) _ _) = throwError "Predicate for If must be a Boolean"
interpretWithScope (MyIf (MyLambda _ _) _ _) = throwError "Predicate for If must be a Boolean"
interpretWithScope (MyIf pred' true false) = do
  predExpr <- interpretWithScope pred'
  interpretWithScope (MyIf predExpr true false)
