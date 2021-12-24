{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Mimsa.Server.Helpers.TestData
  ( UnitTestData (..),
    PropertyTestData (..),
    TestData (..),
    makeTestData,
    mkUnitTestData,
  )
where

import qualified Data.Aeson as JSON
import Data.Coerce
import Data.Either
import Data.Map (Map)
import Data.OpenApi hiding (get)
import qualified Data.Set as S
import Data.Text (Text)
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Project
import Language.Mimsa.Tests.Test
import Language.Mimsa.Tests.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store

data TestData = TestData
  { tdUnitTests :: [UnitTestData],
    tdPropertyTests :: [PropertyTestData]
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

data UnitTestData = UnitTestData
  { utdTestName :: Text,
    utdTestSuccess :: Bool,
    utdBindings :: Map Name Text
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

mkUnitTestData :: Project ann -> UnitTest -> UnitTestData
mkUnitTestData project unitTest = do
  let getDep = (`findBindingNameForExprHash` project)
  let depMap =
        mconcat
          ( getDep
              <$> S.toList
                (getDirectDepsOfTest project (UTest unitTest))
          )
  UnitTestData
    (coerce $ utName unitTest)
    (coerce $ utSuccess unitTest)
    (coerce <$> depMap)

data PropertyTestData = PropertyTestData
  { ptdTestName :: Text,
    ptdTestFailures :: [Text],
    ptdBindings :: Map Name Text
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

mkPropertyTestData ::
  Project ann ->
  PropertyTest ->
  PropertyTestResult Variable ann ->
  PropertyTestData
mkPropertyTestData project propertyTest result = do
  let getDep = (`findBindingNameForExprHash` project)
  let depMap =
        mconcat
          ( getDep
              <$> S.toList
                (getDirectDepsOfTest project (PTest propertyTest))
          )
  let failures = case result of
        PropertyTestSuccess -> mempty
        PropertyTestFailures es -> prettyPrint <$> S.toList es
  PropertyTestData
    (coerce $ ptName propertyTest)
    failures
    (coerce <$> depMap)

data RuntimeData = RuntimeData
  { rtdName :: Text,
    rtdDescription :: Text,
    rtdMonoType :: Text
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

splitTestResults ::
  [TestResult var ann] ->
  ( [UnitTest],
    [(PropertyTest, PropertyTestResult var ann)]
  )
splitTestResults results =
  let f res = case res of
        PTestResult pt res' -> Right (pt, res')
        UTestResult ut -> Left ut
   in partitionEithers
        ( f
            <$> results
        )

makeTestData ::
  Project Annotation ->
  [TestResult Variable Annotation] ->
  TestData
makeTestData project testResults =
  let (uts, pts) = splitTestResults testResults

      unitTests =
        mkUnitTestData project
          <$> uts

      propertyTests =
        uncurry (mkPropertyTestData project) <$> pts
   in TestData unitTests propertyTests