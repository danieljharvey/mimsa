{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Core.Types.Identifiers.TypeIdentifier
  ( TypeIdentifier (..),
    renderTypeIdentifier,
    printTypeNum,
    getUniVar,
  )
where

-- the two types of id a type var can have - a named or numbered one

import qualified Data.Aeson as JSON
import GHC.Generics
import Language.Mimsa.Core.Printer
import Language.Mimsa.Core.Types.Identifiers.Name
import Language.Mimsa.Core.Types.Identifiers.TyVar
import Prettyprinter

data TypeIdentifier
  = TVName -- a type variable from a type signature
      { tiVar :: TyVar
      }
  | TVUnificationVar -- invented type for unification
      { tiUniVar :: Int
      }
  | TVScopedVar -- named variable with unique int to allow scoping
      { tiUniVar :: Int,
        tvName :: Name
      }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Generic
    )
  deriving anyclass
    ( JSON.ToJSON,
      JSON.ToJSONKey,
      JSON.FromJSON
    )

instance Printer TypeIdentifier where
  prettyDoc = renderTypeIdentifier

printTypeNum :: Int -> String
printTypeNum i = [toEnum (index + start)] <> suffix
  where
    index = (i - 1) `mod` 26
    start = fromEnum 'a'
    suffix =
      let diff = (i - 1) `div` 26
       in if diff < 1 then "" else show diff

renderTypeIdentifier :: TypeIdentifier -> Doc ann
renderTypeIdentifier (TVName n) = renderTyVar n
renderTypeIdentifier (TVUnificationVar i) = pretty (printTypeNum i)
renderTypeIdentifier (TVScopedVar i _) = pretty (printTypeNum i)

getUniVar :: TypeIdentifier -> Maybe Int
getUniVar (TVName _) = Nothing
getUniVar (TVUnificationVar i) = Just i
getUniVar (TVScopedVar i _) = Just i
