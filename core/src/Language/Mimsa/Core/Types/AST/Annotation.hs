{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Core.Types.AST.Annotation (Annotation (..)) where

import qualified Data.Aeson as JSON
import GHC.Generics
import Language.Mimsa.Core.Printer
import Prettyprinter

-- | Source code annotations - this is stored in parsing and used to improve
-- errors. Discarded when we store the expressions
data Annotation
  = -- | No annotation
    None ()
  | -- | Start and end of this item in the original source
    Location {annStart :: Int, annEnd :: Int}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, JSON.FromJSON)

instance Semigroup Annotation where
  Location a b <> Location a' b' = Location (min a a') (max b b')
  Location a b <> None _ = Location a b
  None _ <> a = a

instance Monoid Annotation where
  mempty = None ()

instance Printer Annotation where
  prettyDoc (None _) = "-"
  prettyDoc (Location a b) = pretty a <> " - " <> pretty b
