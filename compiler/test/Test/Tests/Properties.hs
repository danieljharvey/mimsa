{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Tests.Properties
  ( spec,
  )
where

import Control.Monad.IO.Class
import Data.Either (isLeft, isRight)
import Data.Functor
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Language.Mimsa.Core
import Language.Mimsa.Store.ResolveDataTypes
import Language.Mimsa.Tests.Generate
import Language.Mimsa.Tests.Helpers
import Language.Mimsa.Typechecker.CreateEnv
import Language.Mimsa.Typechecker.Elaborate
import Language.Mimsa.Typechecker.NumberVars
import Language.Mimsa.Typechecker.Typecheck
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Test.Data.Project
import Test.Hspec
import Test.Utils.Helpers

getStoreExprs :: Project Annotation -> [StoreExpression Annotation]
getStoreExprs =
  M.elems
    . getStore
    . prjStore

itTypeChecks :: MonoType -> Expr Name Annotation -> Either TypeError ()
itTypeChecks mt expr = do
  let numberedExpr =
        fromRight
          ( addNumbersToStoreExpression
              expr
              mempty
          )
  let elabbed =
        fmap (\(_, _, a, _) -> a)
          . typecheck
            mempty
            ( createEnv
                mempty
                (createTypeMap $ getStoreExprs testStdlib)
                mempty
                mempty
            )
          $ numberedExpr
  generatedMt <- getTypeFromAnn <$> elabbed
  unifies mt generatedMt

itGenerates :: MonoType -> Expectation
itGenerates mt = do
  samples <- liftIO $ generateFromMonoType @() (createTypeMap $ getStoreExprs testStdlib) mt
  let success = traverse (itTypeChecks mt) (fmap ($> mempty) samples)
  success `shouldSatisfy` isRight

spec :: Spec
spec = do
  -- skipping as these tests need types found in the stdlib
  -- will need it to learn to use modules
  xdescribe "Properties" $ do
    describe "Test the testing" $ do
      it "typechecking check works" $ do
        itTypeChecks (MTPrim mempty MTInt) (MyLiteral mempty (MyInt 100))
          `shouldSatisfy` isRight
      it "typechecking fail works" $ do
        itTypeChecks (MTPrim mempty MTBool) (MyLiteral mempty (MyInt 100))
          `shouldSatisfy` isLeft
    describe "isRecursive" $ do
      it "unit is not recursive" $ do
        isRecursive "Unit" [] `shouldBe` False
      it "maybe is not recursive 2" $ do
        isRecursive "Maybe" [MTPrim mempty MTInt] `shouldBe` False
      it "list is recursive" $ do
        isRecursive
          "List"
          [ MTTypeApp mempty (MTConstructor mempty Nothing "List") (MTPrim mempty MTInt)
          ]
          `shouldBe` True

    describe "Test generators" $ do
      it "Bool" $ do
        itGenerates mtBool
      it "Int" $ do
        itGenerates mtInt
      it "String" $ do
        itGenerates mtString
      it "Array of ints" $ do
        itGenerates (MTArray mempty mtInt)
      it "Pair of int and string" $ do
        itGenerates (MTTuple mempty mtInt (NE.singleton mtString))
      it "Records" $ do
        let record = MTRecord mempty (M.fromList [("dog", mtInt), ("cat", mtBool)]) Nothing
        itGenerates record
      it "Functions" $ do
        itGenerates (MTFunction mempty mtBool mtInt)
      it "Nested functions" $ do
        itGenerates (MTFunction mempty mtString (MTFunction mempty mtBool mtInt))
      it "Constructor" $ do
        itGenerates (MTConstructor mempty Nothing "TrafficLight")
      it "Constructor with var" $ do
        itGenerates (MTTypeApp mempty (MTConstructor mempty Nothing "Maybe") mtInt)
      it "Constructor with nested vars" $ do
        itGenerates (MTTypeApp mempty (MTConstructor mempty Nothing "Tree") mtBool)
