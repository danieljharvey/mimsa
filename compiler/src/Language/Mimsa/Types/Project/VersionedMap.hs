{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Mimsa.Types.Project.VersionedMap where

import qualified Data.Aeson as JSON
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

------
-- A versioned Map is a Map whose contents are a unique nonempty list
-- When adding a new item, it goes at the end, removing previous occurances
------

newtype VersionedMap k a = VersionedMap {getVersionedMap :: Map k (NonEmpty a)}
  deriving newtype (Eq, Ord, Show, Monoid)
  deriving newtype (JSON.ToJSON, JSON.FromJSON)

instance (Ord k, Eq a) => Semigroup (VersionedMap k a) where
  (VersionedMap a) <> (VersionedMap b) =
    VersionedMap (M.unionWith combineUnique a b)

-- we don't want duplicates in list
-- nub keeps first instance, we want last instance, hence the reversing
combineUnique :: (Eq a) => NonEmpty a -> NonEmpty a -> NonEmpty a
combineUnique as bs =
  let as' = NE.toList as
      bs' = NE.toList bs
   in NE.fromList . reverse . nub . reverse $ as' <> bs'

-- remove an item from a versioned map
delete :: (Ord k) => k -> VersionedMap k a -> VersionedMap k a
delete key = VersionedMap . M.delete key . getVersionedMap
