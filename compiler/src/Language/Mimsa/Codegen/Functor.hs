{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Codegen.Functor
  ( functorMap,
  )
where

import Control.Monad.Except
import Data.Coerce
import Data.Foldable (foldl')
import qualified Data.List.NonEmpty as NE
import Data.Semigroup
import Data.Text (Text)
import Language.Mimsa.Codegen.Utils
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker
import Prelude hiding (fmap)

functorMap :: DataType () -> Either Text (Expr Name ())
functorMap = runCodegenM . functorMap_

-- | A newtype is a datatype with one constructor
-- | with one argument
functorMap_ ::
  DataType () ->
  CodegenM (Expr Name ())
functorMap_ (DataType tyCon vars items) = do
  let tyName = tyConToName tyCon
  fVar <- getFunctorVar vars
  case getMapItems items of
    Nothing -> throwError "Type should have at least one constructor"
    Just constructors -> do
      matches <-
        traverse
          ( uncurry
              ( createMatch tyCon fVar
              )
          )
          constructors
      pure
        ( MyLet
            mempty
            "fmap"
            ( MyLambda
                mempty
                "f"
                ( MyLambda
                    mempty
                    tyName
                    ( MyPatternMatch
                        mempty
                        (MyVar mempty tyName)
                        (NE.toList matches)
                    )
                )
            )
            (MyVar mempty "fmap")
        )

data FieldItemType
  = VariableField Name
  | RecurseField Name
  | Func2 Name Name Name

toFieldItemType :: TyCon -> Type () -> CodegenM FieldItemType
toFieldItemType typeName = \case
  MTVar _ (TVName a) -> pure (VariableField (coerce a))
  MTData _ fieldConsName _fields
    | fieldConsName == typeName ->
      RecurseField <$> nextName typeName
  MTFunction _ (MTVar _ (TVName a)) (MTVar _ (TVName b)) ->
    pure $ Func2 (coerce a <> "to" <> coerce b) (coerce a) (coerce b)
  _ -> throwError "Expected VarName"

reconstructField :: Name -> FieldItemType -> Expr Name ()
reconstructField matchVar fieldItem =
  case fieldItem of
    VariableField varName ->
      if varName == matchVar
        then MyApp mempty (MyVar mempty "f") (MyVar mempty varName)
        else MyVar mempty varName
    RecurseField restVar ->
      MyApp
        mempty
        ( MyApp
            mempty
            (MyVar mempty "fmap")
            (MyVar mempty "f")
        )
        (MyVar mempty restVar)
    Func2 fromF from to ->
      if to == matchVar
        then
          MyLambda
            mempty
            from
            ( MyApp
                mempty
                (MyVar mempty "f")
                ( MyApp
                    mempty
                    (MyVar mempty fromF)
                    (MyVar mempty from)
                )
            )
        else
          MyLambda
            mempty
            from
            ( MyApp
                mempty
                (MyVar mempty fromF)
                (MyVar mempty from)
            )

createPattern :: TyCon -> [FieldItemType] -> Pattern Name ()
createPattern tyCon fields =
  PConstructor mempty tyCon (toPattern <$> fields)
  where
    toPattern (VariableField a) = PVar mempty a
    toPattern (RecurseField a) = PVar mempty a
    toPattern (Func2 a _ _) = PVar mempty a

-- | A match is one item in a case match, that will deconstruct and then
-- rebuild whatever is inside, mapping over `f` as it does.
-- TODO: allow the `f` to be configurable and to allow multiple ones so we can
-- implement bifunctor with the same code
createMatch ::
  TyCon ->
  Name ->
  TyCon ->
  [Type ()] ->
  CodegenM (Pattern Name (), Expr Name ())
createMatch typeName matchVar tyCon fields = do
  regFields <-
    traverse (toFieldItemType typeName) fields
  let withConsApp =
        foldl'
          ( \expr' fieldItem ->
              let reconstruct = reconstructField matchVar fieldItem
               in MyConsApp mempty expr' reconstruct
          )
          (MyConstructor mempty tyCon)
          regFields
  pure (createPattern tyCon regFields, withConsApp)
