{-# LANGUAGE OverloadedStrings #-}

module CoreTest.Prettier
  ( spec,
  )
where

import CoreTest.Utils.Helpers
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Text.IO as T
import Language.Mimsa.Core
import Test.Hspec

spec :: Spec
spec =
  describe "Prettier" $ do
    describe "Expr" $ do
      it "Cons with infix" $ do
        let expr' = unsafeParseExpr "Some (1 == 1)"
            doc = prettyDoc expr'
        renderWithWidth 50 doc `shouldBe` "Some (1 == 1)"

      it "Many + operators" $ do
        let expr' = unsafeParseExpr "1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10"
            doc = prettyDoc expr'
        renderWithWidth 50 doc `shouldBe` "1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10"
        renderWithWidth 5 doc `shouldBe` "1 + 2\n  + 3\n  + 4\n  + 5\n  + 6\n  + 7\n  + 8\n  + 9\n  + 10"

      it "Nested lambdas" $ do
        let expr' = unsafeParseExpr "\\f -> \\g -> \\a -> f (g a)"
            doc = prettyDoc expr'
        renderWithWidth 50 doc `shouldBe` "\\f -> \\g -> \\a -> f (g a)"
        renderWithWidth 5 doc `shouldBe` "\\f ->\n  \\g ->\n    \\a ->\n      f (g a)"

      it "Line between let bindings" $ do
        let expr' = unsafeParseExpr "let a = 1; a"
            doc = prettyDoc expr'
        renderWithWidth 50 doc `shouldBe` "let a = 1 in a"
        renderWithWidth 5 doc `shouldBe` "let a =\n  1;\n\na"

      it "Line between let pair bindings" $ do
        let expr' = unsafeParseExpr "let (a,b) = (1,2); a"
            doc = prettyDoc expr'
        renderWithWidth 50 doc `shouldBe` "let (a, b) = ((1, 2)) in a"
        renderWithWidth 5 doc `shouldBe` "let (a, b) =\n  ((1,\n    2));\n\na"

      it "Spreads long pairs across two lines" $ do
        let expr' = unsafeParseExpr "(\"horseshorseshorses1\",\"horseshorseshorses2\")"
            doc = prettyDoc expr'
        renderWithWidth 50 doc `shouldBe` "(\"horseshorseshorses1\", \"horseshorseshorses2\")"
        renderWithWidth 5 doc `shouldBe` "(\"horseshorseshorses1\",\n \"horseshorseshorses2\")"

      it "Renders empty record nicely" $ do
        let expr' = unsafeParseExpr "{}"
            doc = prettyDoc expr'
        renderWithWidth 50 doc `shouldBe` "{}"
        renderWithWidth 5 doc `shouldBe` "{}"

      it "Renders records nicely" $ do
        let expr' = unsafeParseExpr "{a:1,b:2,c:3,d:4,e:5}"
            doc = prettyDoc expr'
        renderWithWidth 50 doc `shouldBe` "{ a: 1, b: 2, c: 3, d: 4, e: 5 }"
        renderWithWidth 5 doc `shouldBe` "{ a: 1,\n  b: 2,\n  c: 3,\n  d: 4,\n  e: 5 }"

      it "Renders if nicely" $ do
        let expr' = unsafeParseExpr "if True then 1 else 2"
            doc = prettyDoc expr'
        renderWithWidth 50 doc `shouldBe` "if True then 1 else 2"
        renderWithWidth 4 doc `shouldBe` "if True\nthen\n  1\nelse\n  2"

      it "Renders datatype nicely with two line break" $ do
        let expr' = unsafeParseDataType "type These a = That a"
            doc = prettyDoc expr'
        renderWithWidth 50 doc `shouldBe` "type These a    = That a"
        renderWithWidth 5 doc `shouldBe` "type These a \n  = That\n  a"

      it "Renders new function syntax nicely" $ do
        let expr' = unsafeParseExpr "let const a b = a in 1"
            doc = prettyDoc expr'
        renderWithWidth 50 doc `shouldBe` "let const a b = a in 1"
        renderWithWidth 5 doc `shouldBe` "let const a b =\n  a;\n\n1"

      it "Renders annotation for let" $ do
        let expr' = unsafeParseExpr "let (num: Int) = 3; True"
            doc = prettyDoc expr'
        renderWithWidth 50 doc `shouldBe` "let (num: Int) = 3 in True"

      it "Renders annotation for let function" $ do
        let expr' = unsafeParseExpr "let (const: a -> b -> a) a b = a; True"
            doc = prettyDoc expr'
        renderWithWidth 50 doc `shouldBe` "let (const: a -> b -> a) a b = a in True"

    describe "MonoType" $ do
      it "String" $
        T.putStrLn (prettyPrint MTString)
      it "Function" $
        let mt :: MonoType
            mt =
              MTFunction
                mempty
                (MTFunction mempty (MTPrim mempty MTInt) (MTPrim mempty MTString))
                (MTPrim mempty MTBool)
         in T.putStrLn
              ( prettyPrint mt
              )
      it "Record" $
        let mt :: MonoType
            mt =
              MTRecord
                mempty
                ( M.fromList
                    [ ("dog", MTPrim mempty MTBool),
                      ("horse", MTPrim mempty MTString),
                      ( "maybeDog",
                        dataTypeWithVars
                          mempty
                          Nothing
                          "Maybe"
                          [MTPrim mempty MTString]
                      )
                    ]
                )
                Nothing
         in T.putStrLn
              ( prettyPrint mt
              )
      it "Pair" $
        let mt :: MonoType
            mt =
              MTTuple
                mempty
                (MTFunction mempty (MTPrim mempty MTInt) (MTPrim mempty MTInt))
                (NE.singleton $ MTPrim mempty MTString)
         in T.putStrLn
              (prettyPrint mt)
      it "Variables" $
        let mt :: MonoType
            mt =
              MTFunction
                mempty
                ( MTVar mempty $
                    tvNamed "catch"
                )
                (MTVar mempty $ TVUnificationVar 22)
         in T.putStrLn
              ( prettyPrint mt
              )
      it "Names type vars" $ do
        let mt = MTVar () (TVUnificationVar 1)
        prettyPrint mt `shouldBe` "a"
      it "Names type vars 2" $ do
        let mt = MTVar () (TVUnificationVar 26)
        prettyPrint mt `shouldBe` "z"
      it "Names type vars 3" $ do
        let mt = MTVar () (TVUnificationVar 27)
        prettyPrint mt `shouldBe` "a1"
