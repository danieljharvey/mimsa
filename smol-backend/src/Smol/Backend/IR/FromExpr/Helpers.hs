{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Smol.Backend.IR.FromExpr.Helpers
  ( flattenConstructorType,
    primFromConstructor,
    lookupTypeName,
    isStringType,
    resolveConstructor,
    resolveIdentifier,
  )
where

import Control.Monad.State
import qualified Data.Map.Strict as M
import Data.String (fromString)
import GHC.Records (HasField (..))
import Smol.Backend.IR.FromExpr.Types
import Smol.Core.Helpers
import qualified Smol.Core.Typecheck.Shared as TC
import qualified Smol.Core.Types as Smol
import Smol.Core.Types.ResolvedDep

resolveConstructor :: Smol.ResolvedDep Smol.Constructor -> Smol.Constructor
resolveConstructor (LocalDefinition c) = c
resolveConstructor (UniqueDefinition c idx) = c <> fromString ("_" <> show idx)
resolveConstructor (TypeclassCall c idx) = fromString "tc_" <> c <> fromString ("_" <> show idx)

resolveIdentifier :: Smol.ResolvedDep Smol.Identifier -> Smol.Identifier
resolveIdentifier (LocalDefinition c) = c
resolveIdentifier (UniqueDefinition i idx) = i <> fromString ("_" <> show idx)
resolveIdentifier (TypeclassCall i idx) = fromString "tc_" <> i <> fromString ("_" <> show idx)

isStringType :: Smol.Type dep ann -> Bool
isStringType (Smol.TPrim _ Smol.TPString) = True
isStringType (Smol.TLiteral _ (Smol.TLString _)) = True
isStringType _ = False

flattenConstructorType ::
  ( Monad m,
    Show ann,
    Show (dep Smol.Identifier),
    Show (dep Smol.TypeName)
  ) =>
  Smol.Type dep ann ->
  m (dep Smol.TypeName, [Smol.Type dep ann])
flattenConstructorType ty = do
  let result = TC.flattenConstructorType ty
  pure (fromRight result)

-- | lookup constructor, get number for it and expected number of args
-- we'll use this to create datatype etc
primFromConstructor ::
  ( MonadState (FromExprState ann) m
  ) =>
  Smol.Constructor ->
  m Smol.Prim
primFromConstructor constructor = do
  dt <- lookupConstructor constructor
  let i = getConstructorNumber dt constructor
  pure (Smol.PInt i)

-- | lookup constructor, get number for it and expected number of args
-- we'll use this to create datatype etc
lookupConstructor ::
  ( MonadState (FromExprState ann) m
  ) =>
  Smol.Constructor ->
  m (Smol.DataType ResolvedDep ann)
lookupConstructor constructor = do
  maybeDt <-
    gets
      ( mapFind
          ( \dt@(Smol.DataType _ _ constructors) ->
              (,) dt <$> M.lookup constructor constructors
          )
          . getField @"fesDataTypes"
      )
  case maybeDt of
    Just (dt, _) -> pure dt
    Nothing -> error "cant find, what the hell man"

lookupTypeName ::
  ( MonadState (FromExprState ann) m
  ) =>
  ResolvedDep Smol.TypeName ->
  m (Smol.DataType ResolvedDep ann)
lookupTypeName tn = do
  maybeDt <- gets (M.lookup tn . getField @"fesDataTypes")
  case maybeDt of
    Just dt -> pure dt
    Nothing -> error $ "couldn't find datatype for " <> show tn

getConstructorNumber :: Smol.DataType ResolvedDep ann -> Smol.Constructor -> Integer
getConstructorNumber (Smol.DataType _ _ constructors) constructor =
  case M.lookup constructor (mapToNumbered constructors) of
    Just i -> i
    Nothing -> error "blah"
