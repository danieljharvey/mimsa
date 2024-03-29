module Language.Mimsa.Store.ExtractTypes
  ( extractNamedTypeVars,
    extractTypenames,
  )
where

import Data.Coerce
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.Core

-- these functions don't really feel Store-specific anymore

extractTypenames :: Type ann -> Set TypeName
extractTypenames (MTConstructor _ _ typeName) =
  S.singleton typeName
extractTypenames other = withMonoidType extractTypenames other

-----

extractNamedTypeVars :: Type ann -> Set TyVar
extractNamedTypeVars (MTVar _ (TVName tv)) = S.singleton tv
extractNamedTypeVars (MTVar _ (TVScopedVar _ name)) = S.singleton (coerce name)
extractNamedTypeVars other = withMonoidType extractNamedTypeVars other
