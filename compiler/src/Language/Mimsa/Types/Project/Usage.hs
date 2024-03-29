{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Project.Usage where

import qualified Data.Aeson as JSON
import GHC.Generics
import Language.Mimsa.Core
import Language.Mimsa.Types.Store.ExprHash

data Usage
  = Transient Name ExprHash
  | Direct Name ExprHash
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON)

instance Printer Usage where
  prettyPrint (Transient name _) =
    "Transient dependency of "
      <> prettyPrint name
  prettyPrint (Direct name _) =
    "Direct dependency of "
      <> prettyPrint name

----------
