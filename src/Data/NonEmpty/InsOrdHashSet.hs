{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations #-}

module Data.NonEmpty.InsOrdHashSet
  ( NEInsOrdHashSet
  , singleton
  , size
  , member
  , notMember
  , insert
  , delete
  , union
  , map
  , difference
  , intersection
  , filter
  , toList
  , toNonEmptyList
  , fromList
  , fromNonEmptyList
  , toInsOrdHashSet
  , nonEmpty
  ) where

import Data.HashSet.InsOrd (InsOrdHashSet)
import Data.HashSet.InsOrd qualified as IOHS
import Data.Hashable (Hashable)
import Data.List.NonEmpty qualified as NEL
import Prelude hiding (filter, map)

type role NEInsOrdHashSet nominal

newtype NEInsOrdHashSet k =
  NEInsOrdHashSet
    { toInsOrdHashSet :: InsOrdHashSet k
    } deriving (Eq, Semigroup, Show)

singleton :: Hashable k => k -> NEInsOrdHashSet k
singleton =
  NEInsOrdHashSet . IOHS.singleton

size :: NEInsOrdHashSet k -> Int
size =
  IOHS.size . toInsOrdHashSet

member :: Hashable k => k -> NEInsOrdHashSet k -> Bool
member k =
  IOHS.member k . toInsOrdHashSet

notMember :: Hashable k => k -> NEInsOrdHashSet k -> Bool
notMember k =
  not . member k

insert :: Hashable k => k -> NEInsOrdHashSet k -> NEInsOrdHashSet k
insert k =
  NEInsOrdHashSet . IOHS.insert k . toInsOrdHashSet

delete :: Hashable k => k -> NEInsOrdHashSet k -> Maybe (NEInsOrdHashSet k)
delete k =
  nonEmpty . IOHS.delete k . toInsOrdHashSet

union :: Hashable k
      => NEInsOrdHashSet k -> NEInsOrdHashSet k -> NEInsOrdHashSet k
union (NEInsOrdHashSet iohs1) (NEInsOrdHashSet iohs2) =
  NEInsOrdHashSet $ IOHS.union iohs1 iohs2

map :: Hashable a => (k -> a) -> NEInsOrdHashSet k -> NEInsOrdHashSet a
map fn =
  NEInsOrdHashSet . IOHS.map fn . toInsOrdHashSet

difference :: Hashable k
           => NEInsOrdHashSet k
           -> NEInsOrdHashSet k
           -> Maybe (NEInsOrdHashSet k)
difference (NEInsOrdHashSet iohs1) (NEInsOrdHashSet iohs2) =
  nonEmpty $ IOHS.difference iohs1 iohs2

intersection :: Hashable k
             => NEInsOrdHashSet k
             -> NEInsOrdHashSet k
             -> Maybe (NEInsOrdHashSet k)
intersection (NEInsOrdHashSet iohs1) (NEInsOrdHashSet iohs2) =
  nonEmpty $ IOHS.intersection iohs1 iohs2

filter :: (k -> Bool) -> NEInsOrdHashSet k -> Maybe (NEInsOrdHashSet k)
filter fn =
  nonEmpty . IOHS.filter fn . toInsOrdHashSet

toList :: NEInsOrdHashSet k -> [k]
toList =
  IOHS.toList . toInsOrdHashSet

-- Note that while this is using the partial 'NEL.fromList', this should never
-- actually fail because the only way we ever expose a means to create or edit
-- 'NEInsOrdHashSet's is through smart constructors and other functions that
-- ensure strictness. See the test suite for assurance that this is safe to
-- use.
--
toNonEmptyList :: NEInsOrdHashSet k -> NEL.NonEmpty k
toNonEmptyList =
  NEL.fromList . toList

fromList :: Hashable k => [k] -> Maybe (NEInsOrdHashSet k)
fromList =
  fmap fromNonEmptyList . NEL.nonEmpty

fromNonEmptyList :: Hashable k => NEL.NonEmpty k -> NEInsOrdHashSet k
fromNonEmptyList =
  NEInsOrdHashSet . IOHS.fromList . NEL.toList

nonEmpty :: InsOrdHashSet k -> Maybe (NEInsOrdHashSet k)
nonEmpty iohs =
  if IOHS.null iohs
    then Nothing
    else Just $ NEInsOrdHashSet iohs
