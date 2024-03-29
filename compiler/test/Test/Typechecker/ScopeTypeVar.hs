{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Typechecker.ScopeTypeVar
  ( spec,
  )
where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State.Strict
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Language.Mimsa.Core
import Language.Mimsa.Typechecker.ScopeTypeVar
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Typechecker
import Test.Hspec

runTC ::
  ExceptT TypeError (StateT TypecheckState Identity) a ->
  Either TypeError a
runTC action =
  fst either'
  where
    defaultState =
      TypecheckState 1 mempty
    either' =
      runState
        (runExceptT action)
        defaultState

spec :: Spec
spec = do
  describe "ScopeTypeVar" $ do
    it "Empty set changes value" $ do
      let mt = MTVar mempty (TVName "a")
      let result =
            runTC
              ( freshNamedType
                  mempty
                  mt
              )
      snd <$> result `shouldBe` Right (MTVar mempty (TVScopedVar 1 "a"))
    it "Empty set changes to same value" $ do
      let mt =
            MTTuple
              mempty
              (MTVar mempty (TVName "a"))
              (NE.singleton $ MTVar mempty (TVName "a"))

      let result =
            runTC
              ( freshNamedType
                  mempty
                  mt
              )
      let expected =
            MTTuple
              mempty
              (MTVar mempty (TVScopedVar 1 "a"))
              (NE.singleton $ MTVar mempty (TVScopedVar 1 "a"))

      snd <$> result `shouldBe` Right expected

    it "If set contains name leave it" $ do
      let mt = MTVar mempty (TVName "a")
      let result =
            runTC
              ( freshNamedType
                  ( Environment mempty mempty mempty (M.singleton "a" 1) mempty
                  )
                  mt
              )
      snd <$> result `shouldBe` Right (MTVar mempty (TVScopedVar 1 "a"))
