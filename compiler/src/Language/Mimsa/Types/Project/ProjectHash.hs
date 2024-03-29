{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Mimsa.Types.Project.ProjectHash where

import qualified Data.Aeson as JSON
import Data.OpenApi
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Language.Mimsa.Core

-- because of the size of the ints
-- and JS's limitations in the browser
-- we JSON encode these as strings
newtype ProjectHash = ProjectHash Text
  deriving stock (Eq, Ord, Generic)
  deriving newtype (ToParamSchema, ToSchema)
  deriving newtype
    ( JSON.FromJSON,
      JSON.FromJSONKey,
      JSON.ToJSON,
      JSON.ToJSONKey
    )

instance Show ProjectHash where
  show (ProjectHash a) = T.unpack a

instance Printer ProjectHash where
  prettyPrint (ProjectHash a) = a
