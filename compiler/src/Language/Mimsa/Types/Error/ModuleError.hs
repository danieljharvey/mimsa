{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Error.ModuleError (ModuleError (..), moduleErrorDiagnostic) where

import Data.Set (Set)
import Data.Text (Text)
import qualified Error.Diagnose as Diag
import Language.Mimsa.Core
import Language.Mimsa.Types.Error.TypeError

data ModuleError
  = DuplicateDefinition DefIdentifier
  | DuplicateTypeName TypeName
  | DuplicateConstructor TyCon
  | DefinitionConflictsWithImport DefIdentifier ModuleHash
  | TypeConflictsWithImport TypeName ModuleHash
  | CannotFindValues (Set DefIdentifier)
  | CannotFindTypes (Set TypeName)
  | CannotFindConstructors (Set TyCon)
  | DefDoesNotTypeCheck Text DefIdentifier TypeError
  | NamedImportNotFound (Set ModuleName) ModuleName
  | MissingModule ModuleHash
  | MissingModuleDep DefIdentifier ModuleHash
  | MissingModuleTypeDep TypeName ModuleHash
  | DefMissingReturnType DefIdentifier
  | DefMissingTypeAnnotation DefIdentifier Name
  | EmptyTestName (Expr Name ())
  deriving stock (Eq, Ord, Show)

instance Printer ModuleError where
  prettyPrint (DuplicateDefinition name) =
    "Duplicate definition: " <> prettyPrint name
  prettyPrint (DuplicateTypeName tyName) =
    "Duplicate type name: " <> prettyPrint tyName
  prettyPrint (DuplicateConstructor tyCon) =
    "Duplicate constructor name: " <> prettyPrint tyCon
  prettyPrint (CannotFindValues names) =
    "Cannot find values: " <> prettyPrint names
  prettyPrint (CannotFindTypes names) =
    "Cannot find types: " <> prettyPrint names
  prettyPrint (CannotFindConstructors names) =
    "Cannot find constructors: " <> prettyPrint names
  prettyPrint (DefDoesNotTypeCheck _ name typeErr) =
    prettyPrint name <> " had a typechecking error: " <> prettyPrint typeErr
  prettyPrint (MissingModule mHash) =
    "Could not find module for " <> prettyPrint mHash
  prettyPrint (DefinitionConflictsWithImport name mHash) =
    "Cannot define " <> prettyPrint name <> " as it is already defined in import " <> prettyPrint mHash
  prettyPrint (TypeConflictsWithImport typeName mHash) =
    "Cannot define type " <> prettyPrint typeName <> " as it is already defined in import " <> prettyPrint mHash
  prettyPrint (MissingModuleDep name mHash) =
    "Cannot find dep " <> prettyPrint name <> " in module " <> prettyPrint mHash
  prettyPrint (MissingModuleTypeDep typeName mHash) =
    "Cannot find type " <> prettyPrint typeName <> " in module " <> prettyPrint mHash
  prettyPrint (DefMissingReturnType defName) =
    "Definition " <> prettyPrint defName <> " was expected to have a return type but it is missing"
  prettyPrint (DefMissingTypeAnnotation defName name) =
    "Argument " <> prettyPrint name <> " in " <> prettyPrint defName <> " was expected to have a type annotation but it does not."
  prettyPrint (EmptyTestName expr) =
    "Test name must be non-empty for expression " <> prettyPrint expr
  prettyPrint (NamedImportNotFound haystack needle) =
    "Could not find import for " <> prettyPrint needle <> " in " <> prettyPrint haystack

moduleErrorDiagnostic :: ModuleError -> Diag.Diagnostic Text
moduleErrorDiagnostic (DefDoesNotTypeCheck input _ typeErr) = typeErrorDiagnostic input typeErr
moduleErrorDiagnostic other =
  let report =
        Diag.Err
          Nothing
          (prettyPrint other)
          []
          []
   in Diag.addReport Diag.def report
