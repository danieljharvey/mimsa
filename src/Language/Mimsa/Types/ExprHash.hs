{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Mimsa.Types.ExprHash where

import qualified Data.Aeson as JSON
import qualified Data.Text as T
import Language.Mimsa.Printer

newtype ExprHash = ExprHash Int
  deriving (Eq, Ord, Show)
  deriving newtype (JSON.FromJSON, JSON.ToJSON)

instance Printer ExprHash where
  prettyPrint (ExprHash a) = T.pack . show $ a