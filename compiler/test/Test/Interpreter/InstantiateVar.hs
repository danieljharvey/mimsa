{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Interpreter.InstantiateVar
  ( spec,
  )
where

import Control.Monad.Except
import Control.Monad.State
import Language.Mimsa.Interpreter.InstantiateVar
import Language.Mimsa.Interpreter.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Test.Hspec
import Test.Utils.Helpers

testInstantiate ::
  Expr Variable () ->
  Either (InterpreterError ()) (Expr Variable ())
testInstantiate expr = fst either'
  where
    initialState =
      InterpretState
        { isVarNum = 1,
          isScope = mempty,
          isInfix = mempty,
          isApplyCount = 0,
          isSwaps = mempty
        }
    fn = instantiateVar expr
    either' =
      runState (runExceptT (getApp fn)) initialState

spec :: Spec
spec =
  describe "InstantiateVar" $ do
    it "Replaces id function" $ do
      testInstantiate (MyLambda mempty (named "a") (MyVar mempty (named "a")))
        `shouldBe` Right (MyLambda mempty (numbered 1) (MyVar mempty (numbered 1)))
    it "Replaces pairing function" $ do
      testInstantiate
        ( MyLambda
            mempty
            (named "a")
            ( MyLambda
                mempty
                (named "b")
                ( MyPair
                    mempty
                    (MyVar mempty (named "a"))
                    (MyVar mempty (named "b"))
                )
            )
        )
        `shouldBe` Right
          ( MyLambda
              mempty
              (numbered 1)
              ( MyLambda
                  mempty
                  (numbered 2)
                  ( MyPair
                      mempty
                      (MyVar mempty (numbered 1))
                      (MyVar mempty (numbered 2))
                  )
              )
          )
