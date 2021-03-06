{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Actions.BindExpression
  ( spec,
  )
where

import Data.Either (isLeft)
import qualified Data.Map as M
import Data.Maybe (isJust)
import qualified Data.Set as S
import qualified Language.Mimsa.Actions.AddUnitTest as Actions
import qualified Language.Mimsa.Actions.BindExpression as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Test.Data.Project
import Test.Hspec
import Test.Utils.Helpers

brokenExpr :: Expr Name Annotation
brokenExpr = MyInfix mempty Equals (int 1) (bool True)

projectStoreSize :: Project ann -> Int
projectStoreSize = length . getStore . prjStore

unitTestsSize :: Project ann -> Int
unitTestsSize = M.size . prjUnitTests

testWithIdInExpr :: Expr Name Annotation
testWithIdInExpr =
  MyInfix
    mempty
    Equals
    (MyApp mempty (MyVar mempty "id") (int 1))
    (int 1)

spec :: Spec
spec = do
  describe "BindExpression" $ do
    it "Fails on a syntax error" $ do
      Actions.run
        stdLib
        ( Actions.bindExpression
            brokenExpr
            "broken"
            "1 == True"
        )
        `shouldSatisfy` isLeft
    it "Adds a fresh new function to Bindings and to Store" $ do
      let expr = int 1
      case Actions.run stdLib (Actions.bindExpression expr "one" "1") of
        Left _ -> error "Should not have failed"
        Right (newProject, outcomes, _) -> do
          -- one more item in store
          projectStoreSize newProject
            `shouldBe` projectStoreSize stdLib + 1
          -- one more binding
          lookupBindingName
            newProject
            "one"
            `shouldSatisfy` isJust
          -- one new store expression
          S.size (Actions.storeExpressionsFromOutcomes outcomes)
            `shouldBe` 1
    it "Updating an existing binding updates binding" $ do
      let newIdExpr = MyLambda mempty "b" (MyVar mempty "b")
      let action =
            Actions.bindExpression newIdExpr "id" "\\b -> b"
      case Actions.run stdLib action of
        Left _ -> error "Should not have failed"
        Right (newProject, outcomes, _) -> do
          -- one more item
          projectStoreSize newProject
            `shouldBe` projectStoreSize stdLib + 1
          -- one new expression
          S.size (Actions.storeExpressionsFromOutcomes outcomes)
            `shouldBe` 1
          -- binding hash has changed
          lookupBindingName
            newProject
            "id"
            `shouldNotBe` lookupBindingName stdLib "id"
    it "Updating an existing binding updates tests" $ do
      let newIdExpr = MyLambda mempty "blob" (MyVar mempty "blob")
      let action = do
            _ <- Actions.addUnitTest testWithIdInExpr (TestName "Check id is OK") "id(1) == 1"
            Actions.bindExpression newIdExpr "id" "\\blob -> blob"
      case Actions.run stdLib action of
        Left _ -> error "Should not have failed"
        Right (newProject, outcomes, _) -> do
          -- three more items
          projectStoreSize newProject
            `shouldBe` projectStoreSize stdLib + 3
          -- one new expression, two new tests
          S.size (Actions.storeExpressionsFromOutcomes outcomes)
            `shouldBe` 3
          -- two more unit tests
          unitTestsSize newProject
            `shouldBe` unitTestsSize stdLib + 2
          -- binding hash has changed
          lookupBindingName
            newProject
            "id"
            `shouldNotBe` lookupBindingName stdLib "id"
