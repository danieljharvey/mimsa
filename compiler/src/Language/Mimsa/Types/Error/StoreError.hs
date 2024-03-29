{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Error.StoreError (StoreError (..), FileType (..)) where

import qualified Data.Text as T
import Language.Mimsa.Core
import Language.Mimsa.Types.Store

data FileType = ProjectFile | StoreExprFile | ModuleFile
  deriving stock (Eq, Ord, Show)

instance Printer FileType where
  prettyPrint ProjectFile = "project"
  prettyPrint StoreExprFile = "store expression"
  prettyPrint ModuleFile = "module"

data StoreError
  = ExpressionDoesNotMatchHash ExprHash ExprHash
  | CouldNotReadFilePath FileType FilePath
  | CouldNotWriteFilePath FileType FilePath
  | CouldNotDecodeJson ExprHash
  | CouldNotDecodeFile FilePath
  | CouldNotDecodeByteString
  | CouldNotFindExprHashForBindings [(Maybe ModuleName, Name)]
  | CouldNotFindExprHashForInfixes [InfixOp]
  | CouldNotFindExprHashForTypeBindings [TyCon]
  | CouldNotFindExprHashForTypeNameBindings [TypeName]
  | CouldNotFindBinding Name
  | CouldNotFindStoreExpression ExprHash
  | CouldNotFindModule ModuleHash
  | UnknownStoreError
  deriving stock (Eq, Ord, Show)

instance Printer StoreError where
  prettyPrint (ExpressionDoesNotMatchHash a b) =
    "Expression hashes does not match expected: "
      <> prettyPrint a
      <> " !== "
      <> prettyPrint b
  prettyPrint (CouldNotReadFilePath fileType path) =
    "Could not read " <> prettyPrint fileType <> " file at path " <> T.pack path
  prettyPrint (CouldNotWriteFilePath fileType path) =
    "Could not write " <> prettyPrint fileType <> " file at path " <> T.pack path
  prettyPrint (CouldNotDecodeJson hash') =
    "Could not decode JSON for hash " <> prettyPrint hash'
  prettyPrint CouldNotDecodeByteString =
    "Could not decode JSON for bytestring"
  prettyPrint (CouldNotDecodeFile path) =
    "Could not decode JSON for file " <> T.pack path
  prettyPrint (CouldNotFindExprHashForBindings missing) =
    "Could not find expressions in the store for the following: "
      <> T.intercalate "," (prettyPrint <$> missing)
  prettyPrint (CouldNotFindExprHashForInfixes missing) =
    "Could not find expressions in the store for the following: "
      <> T.intercalate "," (prettyPrint <$> missing)
  prettyPrint (CouldNotFindExprHashForTypeBindings missing) =
    "Could not find type expressions in the store for the following: "
      <> T.intercalate "," (prettyPrint <$> missing)
  prettyPrint (CouldNotFindExprHashForTypeNameBindings missing) =
    "Could not find type name expressions in the store for the following: "
      <> T.intercalate "," (prettyPrint <$> missing)
  prettyPrint (CouldNotFindBinding name) =
    "Could not find binding " <> prettyPrint name
  prettyPrint (CouldNotFindStoreExpression exprHash) =
    "Could not find store expression for hash " <> prettyPrint exprHash
  prettyPrint (CouldNotFindModule modHash) =
    "Could not find module for hash " <> prettyPrint modHash
  prettyPrint UnknownStoreError =
    "Unknown store error"

instance Semigroup StoreError where
  _ <> b = b

instance Monoid StoreError where
  mempty = UnknownStoreError
