{-# LANGUAGE OverloadedStrings #-}

module Test.Typecheck.SubtypeSpec (spec) where

import Control.Monad.Trans.Writer.CPS (runWriterT)
import Data.Either
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty (..))
import Smol.Core
import Smol.Core.Typecheck.FromParsedExpr
import Smol.Core.Typecheck.Simplify
import Test.Helpers
import Test.Hspec

-- Repeat after me, Duck is a subtype of Bird
-- so Duck <: Bird
-- 1 is a subtype of 1 | 2
-- so 1 <: 1 | 2
-- 1 | 2 is a subtype of Int
-- so 1 | 2 <: Int
--
spec :: Spec
spec = do
  describe "Subtyping" $ do
    describe "generaliseLiteral" $ do
      it "Negative literal makes int" $ do
        generaliseLiteral (tyIntLit [-1])
          `shouldBe` TPrim () TPInt

    describe "Subtype" $ do
      describe "Everything defeats TUnknown" $ do
        let things = [TPrim () TPBool, TVar () "horse", TFunc () mempty (TVar () "a") (TVar () "b")]
        traverse_
          ( \ty -> it (show ty <> " combines with TUnknown") $ do
              fst <$> runWriterT (isSubtypeOf ty (TUnknown () 0)) `shouldSatisfy` isRight
          )
          things

      describe "Combine two datatypes" $ do
        it "Maybe Nat <: Maybe i1" $ do
          let one = fromParsedType $ tyCons "Maybe" [tyInt]
              two = fromParsedType $ tyCons "Maybe" [tyUnknown 1]
              expected = (one, [TCWSubstitution $ Substitution (SubUnknown 1) (TPrim () TPInt)])

          runWriterT (one `isSubtypeOf` two)
            `shouldBe` Right expected

        it "Maybe Nat <: i1" $ do
          let one = fromParsedType $ tyCons "Maybe" [tyInt]
              two = fromParsedType $ TUnknown () 1
              expected = (one, [TCWSubstitution $ Substitution (SubUnknown 1) one])

          runWriterT (one `isSubtypeOf` two)
            `shouldBe` Right expected

        it "Maybe Nat <: a b" $ do
          let one = fromParsedType $ tyCons "Maybe" [tyInt]
              two = fromParsedType $ TApp () (TVar () "a") (TVar () "b")
              expected =
                ( one,
                  [ TCWSubstitution $ Substitution (SubId "a") (TConstructor () "Maybe"),
                    TCWSubstitution $ Substitution (SubId "b") (TPrim () TPInt)
                  ]
                )

          runWriterT (one `isSubtypeOf` two)
            `shouldBe` Right expected

        it "Maybe Nat <: i1 i2" $ do
          let one = fromParsedType $ tyCons "Maybe" [tyInt]
              two = fromParsedType $ TApp () (TUnknown () 1) (TUnknown () 2)
              expected =
                ( one,
                  [ TCWSubstitution $ Substitution (SubUnknown 1) (TConstructor () "Maybe"),
                    TCWSubstitution $ Substitution (SubUnknown 2) (TPrim () TPInt)
                  ]
                )

          runWriterT (one `isSubtypeOf` two)
            `shouldBe` Right expected

        it "(a -> Maybe Nat) <: (a -> i1)" $ do
          let maybeNat = tyCons "Maybe" [tyInt]
              one = fromParsedType $ TFunc () mempty (tyVar "a") maybeNat
              two = fromParsedType $ TFunc () mempty (tyVar "a") (TUnknown () 1)
              expected = (one, [TCWSubstitution $ Substitution (SubUnknown 1) (fromParsedType maybeNat)])

          runWriterT (one `isSubtypeOf` two)
            `shouldBe` Right expected

      describe "Combine" $ do
        let inputs =
              [ ("1", "2", "1 | 2"),
                ("1 | 2", "2", "1 | 2"),
                ("1 | 2", "3", "1 | 2 | 3"),
                ("\"eg\"", "\"g\"", "\"eg\" | \"g\"")
              ]
        traverse_
          ( \(one, two, result) -> it (show one <> " <> " <> show two) $ do
              let a =
                    combineMany $
                      fromParsedType (unsafeParseType one)
                        :| [fromParsedType (unsafeParseType two)]
              fst <$> runWriterT a `shouldBe` Right (fromParsedType (unsafeParseType result))
          )
          inputs

      describe "Type addition" $ do
        let inputs =
              [ ("1", "1", "2"),
                ("1", "2", "3"),
                ("1 | 2", "2", "3 | 4"),
                ("1 | 2", "3 | 4", "4 | 5 | 6"),
                ("Int", "Int", "Int"),
                ("String", "String", "String"),
                ("\"a\"", "String", "String"),
                ("String", "\"a\"", "String"),
                ("\"po\"", "\"po\"", "\"popo\"")
              ]
        traverse_
          ( \(one, two, result) -> it (show one <> " + " <> show two <> " = " <> show result) $ do
              let a =
                    simplifyType
                      ( TInfix
                          ()
                          OpAdd
                          (fromParsedType (unsafeParseType one))
                          (fromParsedType (unsafeParseType two))
                      )
              a `shouldBe` fromParsedType (unsafeParseType result)

              let b =
                    simplifyType
                      ( TInfix
                          ()
                          OpAdd
                          (fromParsedType (unsafeParseType two))
                          (fromParsedType (unsafeParseType one))
                      )
              b `shouldBe` fromParsedType (unsafeParseType result)
          )
          inputs

      describe "Valid pairs" $ do
        let validPairs =
              [ ("True", "True"),
                ("False", "False"),
                ("True", "Bool"),
                ("1", "a"),
                ("(True, False)", "(True,Bool)"),
                ("Maybe", "Maybe"),
                ("Maybe 1", "Maybe a"),
                ("{ item: 1 }", "{}"),
                ("[1 | 2]", "[Int]"),
                ("1", "1 | 2"),
                ("(Int,Int)", "(a,b)")
              ]
        traverse_
          ( \(lhs, rhs) -> it (show lhs <> " <: " <> show rhs) $ do
              fst
                <$> runWriterT
                  ( isSubtypeOf
                      (fromParsedType $ unsafeParseType lhs)
                      (fromParsedType $ unsafeParseType rhs)
                  )
                `shouldSatisfy` isRight
          )
          validPairs

      describe "Invalid pairs" $ do
        let validPairs =
              [ ("Bool", "True"),
                ("1", "2 | 3")
              ]
        traverse_
          ( \(lhs, rhs) -> it (show lhs <> " <: " <> show rhs) $ do
              fst
                <$> runWriterT
                  ( isSubtypeOf
                      (fromParsedType $ unsafeParseType lhs)
                      (fromParsedType $ unsafeParseType rhs)
                  )
                `shouldSatisfy` isLeft
          )
          validPairs

      describe "Plus" $ do
        it "U1 + U2 <: Int" $ do
          fst
            <$> runWriterT
              ( isSubtypeOf
                  (TInfix () OpAdd (TUnknown () 1) (TUnknown () 2))
                  tyInt
              )
            `shouldSatisfy` isRight

        it "U1 == U2 <: Bool" $ do
          fst
            <$> runWriterT
              ( isSubtypeOf
                  (TInfix () OpEquals (TUnknown () 1) (TUnknown () 2))
                  tyBool
              )
            `shouldSatisfy` isRight
