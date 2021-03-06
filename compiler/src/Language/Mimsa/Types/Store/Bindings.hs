{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Store.Bindings where

import qualified Data.Aeson as JSON
import Data.Map (Map)
import qualified Data.Map as M
import Data.Swagger
import qualified Data.Text as T
import Language.Mimsa.Printer
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store.ExprHash

-- a list of names to hashes
newtype Bindings = Bindings {getBindings :: Map Name ExprHash}
  deriving newtype
    ( Eq,
      Ord,
      Show,
      Semigroup,
      Monoid,
      JSON.FromJSON,
      JSON.ToJSON,
      ToSchema
    )

instance Printer Bindings where
  prettyPrint (Bindings b) =
    "{ " <> T.intercalate ", " (prettyPrint <$> M.keys b) <> " }"
