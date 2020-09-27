module Language.Mimsa.Store.ExtractTypes
  ( extractTypes,
    extractTypeDecl,
    extractDataTypes,
  )
where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.Typechecker.DataTypes (builtInTypes)
import Language.Mimsa.Types.AST (DataType (DataType), Expr (..))
import Language.Mimsa.Types.Identifiers
  ( Name,
    TyCon,
    TypeName (ConsName, VarName),
  )

-- this works out which external types have been used in a given expression
-- therefore, we must remove any which are declared in the expression itself
extractTypes :: Expr Name -> Set TyCon
extractTypes = filterBuiltIns . extractTypes_

extractTypes_ :: Expr Name -> Set TyCon
extractTypes_ (MyVar _) = mempty
extractTypes_ (MyIf a b c) = extractTypes_ a <> extractTypes_ b <> extractTypes_ c
extractTypes_ (MyLet _ a b) = extractTypes_ a <> extractTypes_ b
extractTypes_ (MyLambda _ a) = extractTypes_ a
extractTypes_ (MyApp a b) = extractTypes_ a <> extractTypes_ b
extractTypes_ (MyLiteral _) = mempty
extractTypes_ (MyLetPair _ _ a b) =
  extractTypes_ a <> extractTypes_ b
extractTypes_ (MyPair a b) = extractTypes_ a <> extractTypes_ b
extractTypes_ (MyRecord map') = foldMap extractTypes_ map'
extractTypes_ (MyRecordAccess a _) = extractTypes_ a
extractTypes_ (MyData dt a) =
  S.difference
    (extractConstructors dt <> extractTypes_ a)
    (extractLocalTypeDeclarations dt)
extractTypes_ (MyConstructor t) = S.singleton t
extractTypes_ (MyConsApp a b) = extractTypes_ a <> extractTypes_ b
extractTypes_ (MyCaseMatch sum' matches catchAll) =
  extractTypes_ sum'
    <> mconcat (extractTypes_ . snd <$> NE.toList matches)
    <> mconcat (S.singleton . fst <$> NE.toList matches)
    <> maybe mempty extractTypes catchAll

filterBuiltIns :: Set TyCon -> Set TyCon
filterBuiltIns = S.filter (\c -> not $ M.member c builtInTypes)

-- get all the constructors mentioned in the datatype
extractConstructors :: DataType -> Set TyCon
extractConstructors (DataType _ _ cons) = mconcat (extractFromCons . snd <$> M.toList cons)
  where
    extractFromCons as = mconcat (extractFromCon <$> as)
    extractFromCon (VarName _) = mempty
    extractFromCon (ConsName name as) = S.singleton name <> mconcat (extractFromCon <$> as)

-- get all the names of constructors (type and data) declared in the datatype
extractLocalTypeDeclarations :: DataType -> Set TyCon
extractLocalTypeDeclarations (DataType cName _ cons) =
  S.singleton cName
    <> mconcat (S.singleton . fst <$> M.toList cons)

-----------

extractTypeDecl :: Expr a -> Set TyCon
extractTypeDecl = withDataTypes extractLocalTypeDeclarations

extractDataTypes :: Expr a -> Set DataType
extractDataTypes = withDataTypes S.singleton

withDataTypes :: (Monoid b) => (DataType -> b) -> Expr a -> b
withDataTypes _ (MyVar _) = mempty
withDataTypes f (MyIf a b c) = withDataTypes f a <> withDataTypes f b <> withDataTypes f c
withDataTypes f (MyLet _ a b) = withDataTypes f a <> withDataTypes f b
withDataTypes f (MyLambda _ a) = withDataTypes f a
withDataTypes f (MyApp a b) = withDataTypes f a <> withDataTypes f b
withDataTypes _ (MyLiteral _) = mempty
withDataTypes f (MyLetPair _ _ a b) =
  withDataTypes f a <> withDataTypes f b
withDataTypes f (MyPair a b) = withDataTypes f a <> withDataTypes f b
withDataTypes f (MyRecord map') = foldMap (withDataTypes f) map'
withDataTypes f (MyRecordAccess a _) = withDataTypes f a
withDataTypes f (MyData dt a) =
  withDataTypes f a
    <> f dt
withDataTypes _ (MyConstructor _) = mempty
withDataTypes f (MyConsApp a b) = withDataTypes f a <> withDataTypes f b
withDataTypes f (MyCaseMatch sum' matches catchAll) =
  withDataTypes f sum'
    <> mconcat (withDataTypes f . snd <$> NE.toList matches)
    <> maybe mempty (withDataTypes f) catchAll