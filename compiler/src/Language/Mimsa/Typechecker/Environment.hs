{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.Environment (lookupConstructor) where

import Control.Monad.Except
import qualified Data.Map as M
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker

-- given a constructor name, return the type it lives in
lookupConstructor ::
  (MonadError TypeError m) =>
  Environment ->
  Annotation ->
  TyCon ->
  m (DataType Annotation)
lookupConstructor env ann name = do
  case M.toList $ M.filter (containsConstructor name) (getDataTypes env) of
    [(_, a)] -> pure a -- we only want a single match
    (_ : _) -> throwError (ConflictingConstructors ann name)
    _ -> throwError (TypeConstructorNotInScope env ann name)

-- does this data type contain the given constructor?
containsConstructor :: TyCon -> DataType ann -> Bool
containsConstructor name (DataType _tyName _tyVars constructors) =
  M.member name constructors
