{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Mimsa.Types.Error.PatternMatchError
  ( PatternMatchErrorF (..),
    PatternMatchError,
    renderPatternMatchError,
  )
where

import Data.Set (Set)
import qualified Data.Text as T
import Language.Mimsa.Core
import Prettyprinter
import Text.Megaparsec

data PatternMatchErrorF var ann
  = -- | No patterns provided
    EmptyPatternMatch ann
  | -- | "Just 1 2" or "Nothing 3", for instance
    -- | ann, offending tyCon, expected, actual
    ConstructorArgumentLengthMismatch ann TyCon Int Int
  | -- | Cases not covered in pattern matches
    -- | ann, [missing patterns]
    MissingPatterns ann [Pattern var ann]
  | -- | Unnecessary cases covered by previous matches
    RedundantPatterns ann [Pattern var ann]
  | -- | Multiple instances of the same variable
    DuplicateVariableUse ann (Set var)
  deriving stock (Eq, Ord, Show, Foldable)

type PatternMatchError = PatternMatchErrorF Name Annotation

------

instance Semigroup (PatternMatchErrorF var ann) where
  a <> _ = a

instance
  ( Printer ann,
    Printer var,
    Printer (Pattern var ann)
  ) =>
  Printer (PatternMatchErrorF var ann)
  where
  prettyDoc = vsep . renderPatternMatchError

instance ShowErrorComponent PatternMatchError where
  showErrorComponent = T.unpack . prettyPrint
  errorComponentLen pmErr = let (_, len) = getErrorPos pmErr in len

type Start = Int

type Length = Int

-- | Single combined error area for Megaparsec
fromAnnotation :: Annotation -> (Start, Length)
fromAnnotation (Location a b) = (a, b - a)
fromAnnotation _ = (0, 0)

getErrorPos :: PatternMatchError -> (Start, Length)
getErrorPos = fromAnnotation . mconcat . getAllAnnotations

getAllAnnotations :: PatternMatchError -> [Annotation]
getAllAnnotations = foldMap pure

-----

renderPatternMatchError ::
  (Printer var, Printer (Pattern var ann)) =>
  PatternMatchErrorF var ann ->
  [Doc a]
renderPatternMatchError (EmptyPatternMatch _) =
  ["Pattern match needs at least one pattern to match"]
renderPatternMatchError
  ( ConstructorArgumentLengthMismatch
      _
      tyCon
      expected
      actual
    ) =
    [ "Constructor argument length mismatch. "
        <> prettyDoc tyCon
        <> " expected "
        <> prettyDoc expected
        <> " but got "
        <> prettyDoc actual
    ]
renderPatternMatchError (MissingPatterns _ missing) =
  ["Pattern match is not exhaustive. These patterns are missing:"]
    <> (prettyDoc <$> missing)
renderPatternMatchError (RedundantPatterns _ redundant) =
  ["Pattern match has unreachable patterns, you should remove them"] <> (prettyDoc <$> redundant)
renderPatternMatchError (DuplicateVariableUse _ vars) =
  [ "Pattern match variables must be unique.",
    "Variables " <> prettyDoc vars <> " are used multiple times"
  ]
