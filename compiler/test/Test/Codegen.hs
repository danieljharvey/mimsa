{-# LANGUAGE OverloadedStrings #-}

module Test.Codegen
  ( spec,
  )
where

import Language.Mimsa.Codegen
import qualified Test.Codegen.Applicative as Applicative
import qualified Test.Codegen.Enum as Enum
import qualified Test.Codegen.Foldable as Foldable
import qualified Test.Codegen.Functor as Functor
import qualified Test.Codegen.Newtype as Newtype
import Test.Codegen.Shared
import Test.Hspec

spec :: Spec
spec = do
  describe "Codegen" $ do
    Foldable.spec
    Enum.spec
    Newtype.spec
    Functor.spec
    Applicative.spec
  describe "typeclassMatches" $ do
    it "No instances for Void" $ do
      typeclassMatches dtVoid `shouldBe` mempty
    it "Enum instance for TrafficLights" $ do
      typeclassMatches dtTrafficLights `shouldSatisfy` elem Enum
    it "No enum instance for WrappedString" $ do
      typeclassMatches dtWrappedString `shouldNotSatisfy` elem Enum
    it "Newtype instance for WrappedString" $ do
      typeclassMatches dtWrappedString `shouldSatisfy` elem Newtype
    it "Functor instance for Identity" $ do
      typeclassMatches dtIdentity `shouldSatisfy` elem Functor
    it "Newtype instance for Identity" $ do
      typeclassMatches dtIdentity `shouldSatisfy` elem Newtype
    it "Functor instance for Maybe" $ do
      typeclassMatches dtMaybe `shouldSatisfy` elem Functor
    it "No newtype instance for Maybe" $ do
      typeclassMatches dtMaybe `shouldNotSatisfy` elem Newtype
    it "Instances for Env" $ do
      typeclassMatches dtEnv `shouldBe` [Functor, Foldable]
