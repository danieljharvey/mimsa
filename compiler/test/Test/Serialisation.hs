{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Serialisation
  ( spec,
  )
where

import Data.Either (partitionEithers)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding
import Language.Mimsa.ExprUtils
import Language.Mimsa.Parser (parseExprAndFormatError)
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Test.Hspec
import Test.Utils.Serialisation

type StoreExpr = StoreExpression ()

parseExprFromPretty :: String -> IO (Either Text Text)
parseExprFromPretty filename =
  loadRegression
    filename
    (prettyPrintingParses . toStrict . decodeUtf8)

-- remove annotations for comparison
toEmptyAnn :: Expr a b -> Expr a ()
toEmptyAnn = toEmptyAnnotation

-- does the output of our prettyprinting still make sense to the parser?
prettyPrintingParses :: Text -> Either Text Text
prettyPrintingParses input = do
  expr1 <- parseExprAndFormatError input
  case parseExprAndFormatError (prettyPrint expr1) of
    Left e -> Left e
    Right expr2 ->
      if toEmptyAnn expr1 /= toEmptyAnn expr2
        then Left $ prettyPrint expr1 <> " does not match " <> prettyPrint expr2
        else pure input

catEithers :: [Either e a] -> [a]
catEithers as = snd $ partitionEithers as

spec :: Spec
spec =
  describe "Serialisation" $ do
    it "StoreExpression JSON" $ do
      files <- getAllFilesInDir "StoreExpr" "json"
      loaded <- traverse (loadJSON @StoreExpr) files
      length (catEithers loaded) `shouldBe` length loaded

    it "Project JSON" $ do
      files <- getAllFilesInDir "SaveProject" "json"
      loaded <- traverse (loadJSON @SaveProject) files
      length (catEithers loaded) `shouldBe` length loaded

    it "Pretty printing" $ do
      files <- getAllFilesInDir "PrettyPrint" "mimsa"
      loaded <- traverse parseExprFromPretty files
      length (catEithers loaded) `shouldBe` length loaded
