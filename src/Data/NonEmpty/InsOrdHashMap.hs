{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations #-}

module Data.NonEmpty.InsOrdHashMap
  ( NEInsOrdHashMap
  , singleton
  , size
  , member
  , notMember
  , lookup
  , insert
  , insertWith
  , delete
  , adjust
  , update
  , alter
  , union
  , unionWith
  , unionWithKey
  , unions
  , map
  , mapKeys
  , traverseKeys
  , mapWithKey
  , traverseWithKey
  , unorderedTraverse
  , unorderedTraverseWithKey
  , difference
  , intersection
  , intersectionWith
  , intersectionWithKey
  , foldl'
  , foldl1'
  , foldlWithKey'
  , foldl1WithKey'
  , foldr
  , foldr1
  , foldrWithKey
  , foldr1WithKey
  , foldMapWithKey
  , unorderedFoldMap
  , unorderedFoldMapWithKey
  , filter
  , filterWithKey
  , mapMaybe
  , mapMaybeWithKey
  , keys
  , elems
  , toList
  , toNonEmptyList
  , fromList
  , fromNonEmptyList
  , toInsOrdHashMap
  , nonEmpty
  ) where

import Data.Foldable1 qualified as Foldable1
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.HashMap.Strict.InsOrd qualified as IOHM
import Data.Hashable (Hashable)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NEL
#if __GLASGOW_HASKELL__ < 910
import Prelude hiding (filter, foldr, foldr1, lookup, map)
#else
import Prelude hiding (filter, foldl', foldr, foldr1, lookup, map)
#endif

type role NEInsOrdHashMap nominal representational

newtype NEInsOrdHashMap k v =
  NEInsOrdHashMap
    { toInsOrdHashMap :: InsOrdHashMap k v
    } deriving (Eq, Semigroup, Show)

singleton :: Hashable k => k -> v -> NEInsOrdHashMap k v
singleton k =
  NEInsOrdHashMap . IOHM.singleton k

size :: NEInsOrdHashMap k v -> Int
size =
  IOHM.size . toInsOrdHashMap

member :: Hashable k => k -> NEInsOrdHashMap k v -> Bool
member k =
  IOHM.member k . toInsOrdHashMap

notMember :: Hashable k => k -> NEInsOrdHashMap k v -> Bool
notMember k =
  not . member k

lookup :: Hashable k => k -> NEInsOrdHashMap k v -> Maybe v
lookup k =
  IOHM.lookup k . toInsOrdHashMap

insert :: Hashable k => k -> v -> NEInsOrdHashMap k v -> NEInsOrdHashMap k v
insert k v =
  NEInsOrdHashMap . IOHM.insert k v . toInsOrdHashMap

insertWith :: Hashable k
           => (v -> v -> v)
           -> k
           -> v
           -> NEInsOrdHashMap k v
           -> NEInsOrdHashMap k v
insertWith fn k v =
  NEInsOrdHashMap . IOHM.insertWith fn k v . toInsOrdHashMap

delete :: Hashable k => k -> NEInsOrdHashMap k v -> Maybe (NEInsOrdHashMap k v)
delete k =
  nonEmpty . IOHM.delete k . toInsOrdHashMap

adjust :: Hashable k
       => (v -> v) -> k -> NEInsOrdHashMap k v-> NEInsOrdHashMap k v
adjust fn k =
  NEInsOrdHashMap . IOHM.adjust fn k . toInsOrdHashMap

update :: Hashable k
       => (v -> Maybe v)
       -> k
       -> NEInsOrdHashMap k v
       -> Maybe (NEInsOrdHashMap k v)
update fn k =
  nonEmpty . IOHM.update fn k . toInsOrdHashMap

alter :: Hashable k
      => (Maybe v -> Maybe v)
      -> k
      -> NEInsOrdHashMap k v
      -> Maybe (NEInsOrdHashMap k v)
alter fn k =
  nonEmpty . IOHM.alter fn k . toInsOrdHashMap

union :: Hashable k
      => NEInsOrdHashMap k v -> NEInsOrdHashMap k v -> NEInsOrdHashMap k v
union (NEInsOrdHashMap iohm1) (NEInsOrdHashMap iohm2) =
  NEInsOrdHashMap $ IOHM.union iohm1 iohm2

unionWith :: Hashable k
          => (v -> v -> v)
          -> NEInsOrdHashMap k v
          -> NEInsOrdHashMap k v
          -> NEInsOrdHashMap k v
unionWith fn (NEInsOrdHashMap iohm1) (NEInsOrdHashMap iohm2) =
  NEInsOrdHashMap $ IOHM.unionWith fn iohm1 iohm2

unionWithKey :: Hashable k
             => (k -> v -> v -> v)
             -> NEInsOrdHashMap k v
             -> NEInsOrdHashMap k v
             -> NEInsOrdHashMap k v
unionWithKey fn (NEInsOrdHashMap iohm1) (NEInsOrdHashMap iohm2) =
  NEInsOrdHashMap $ IOHM.unionWithKey fn iohm1 iohm2

unions :: (Foldable1.Foldable1 f, Hashable k)
       => f (NEInsOrdHashMap k v) -> NEInsOrdHashMap k v
unions =
  Foldable1.foldl1' union

map :: (v -> a) -> NEInsOrdHashMap k v -> NEInsOrdHashMap k a
map fn =
  NEInsOrdHashMap . IOHM.map fn . toInsOrdHashMap

mapKeys :: Hashable k2
        => (k1 -> k2) -> NEInsOrdHashMap k1 v -> NEInsOrdHashMap k2 v
mapKeys fn =
  NEInsOrdHashMap . IOHM.mapKeys fn . toInsOrdHashMap

traverseKeys :: (Applicative f, Hashable k2)
             => (k1 -> f k2)
             -> NEInsOrdHashMap k1 v
             -> f (NEInsOrdHashMap k2 v)
traverseKeys fn =
  fmap NEInsOrdHashMap . IOHM.traverseKeys fn . toInsOrdHashMap

mapWithKey :: (k -> v -> a) -> NEInsOrdHashMap k v -> NEInsOrdHashMap k a
mapWithKey fn =
  NEInsOrdHashMap . IOHM.mapWithKey fn . toInsOrdHashMap

traverseWithKey :: Applicative f
                => (k -> v -> f a)
                -> NEInsOrdHashMap k v
                -> f (NEInsOrdHashMap k a)
traverseWithKey fn =
  fmap NEInsOrdHashMap . IOHM.traverseWithKey fn . toInsOrdHashMap

unorderedTraverse :: Applicative f
                  => (v -> f a)
                  -> NEInsOrdHashMap k v
                  -> f (NEInsOrdHashMap k a)
unorderedTraverse fn =
  fmap NEInsOrdHashMap . IOHM.unorderedTraverse fn . toInsOrdHashMap

unorderedTraverseWithKey :: Applicative f
                         => (k -> v -> f a)
                         -> NEInsOrdHashMap k v
                         -> f (NEInsOrdHashMap k a)
unorderedTraverseWithKey fn =
  fmap NEInsOrdHashMap . IOHM.unorderedTraverseWithKey fn . toInsOrdHashMap

difference :: Hashable k
           => NEInsOrdHashMap k v
           -> NEInsOrdHashMap k v
           -> Maybe (NEInsOrdHashMap k v)
difference (NEInsOrdHashMap iohm1) (NEInsOrdHashMap iohm2) =
  nonEmpty $ IOHM.difference iohm1 iohm2

intersection :: Hashable k
             => NEInsOrdHashMap k v
             -> NEInsOrdHashMap k v
             -> Maybe (NEInsOrdHashMap k v)
intersection (NEInsOrdHashMap iohm1) (NEInsOrdHashMap iohm2) =
  nonEmpty $ IOHM.intersection iohm1 iohm2

intersectionWith :: Hashable k
                 => (v1 -> v2 -> v3)
                 -> NEInsOrdHashMap k v1
                 -> NEInsOrdHashMap k v2
                 -> Maybe (NEInsOrdHashMap k v3)
intersectionWith fn (NEInsOrdHashMap iohm1) (NEInsOrdHashMap iohm2) =
  nonEmpty $ IOHM.intersectionWith fn iohm1 iohm2

intersectionWithKey :: Hashable k
                    => (k -> v1 -> v2 -> v3)
                    -> NEInsOrdHashMap k v1
                    -> NEInsOrdHashMap k v2
                    -> Maybe (NEInsOrdHashMap k v3)
intersectionWithKey fn (NEInsOrdHashMap iohm1) (NEInsOrdHashMap iohm2) =
  nonEmpty $ IOHM.intersectionWithKey fn iohm1 iohm2

foldl' :: (a -> v -> a) -> a -> NEInsOrdHashMap k v -> a
foldl' fn a =
  IOHM.foldl' fn a . toInsOrdHashMap

-- Note that while this is using the partial 'NEL.fromList' (through 'elems'),
-- this should never actually fail because the only way we ever expose a means
-- to create or edit 'NEInsOrdHashMap's is through smart constructors and other
-- functions that ensure strictness. See the test suite for assurance that this
-- is safe to use.
--
foldl1' :: (v -> v -> v) -> NEInsOrdHashMap k v -> v
foldl1' fn =
  Foldable1.foldl1 fn . elems

foldlWithKey' :: (a -> k -> v -> a) -> a -> NEInsOrdHashMap k v -> a
foldlWithKey' fn a =
  IOHM.foldlWithKey' fn a . toInsOrdHashMap

-- Note that while this is using the partial 'NEL.fromList' (through
-- 'toNonEmptyList'), this should never actually fail because the only way we
-- ever expose a means to create or edit 'NEInsOrdHashMap's is through smart
-- constructors and other functions that ensure strictness. See the test suite
-- for assurance that this is safe to use.
--
foldl1WithKey' :: (v -> k -> v -> v) -> NEInsOrdHashMap k v -> v
foldl1WithKey' fn m =
  let
    assocs = toNonEmptyList m
  in
    List.foldl'
      (\acc (k, v) -> fn acc k v)
      (snd $ NEL.last assocs)
      (NEL.init assocs)

foldr :: (v -> a -> a) -> a -> NEInsOrdHashMap k v -> a
foldr fn a =
  IOHM.foldr fn a . toInsOrdHashMap

-- Note that while this is using the partial 'NEL.fromList' (through 'elems'),
-- this should never actually fail because the only way we ever expose a means
-- to create or edit 'NEInsOrdHashMap's is through smart constructors and other
-- functions that ensure strictness. See the test suite for assurance that this
-- is safe to use.
--
foldr1 :: (v -> v -> v) -> NEInsOrdHashMap k v -> v
foldr1 fn =
  Foldable1.foldr1 fn . elems

foldrWithKey :: (k -> v -> a -> a) -> a -> NEInsOrdHashMap k v -> a
foldrWithKey fn a =
  IOHM.foldrWithKey fn a . toInsOrdHashMap

-- Note that while this is using the partial 'NEL.fromList' (through
-- 'toNonEmptyList'), this should never actually fail because the only way we
-- ever expose a means to create or edit 'NEInsOrdHashMap's is through smart
-- constructors and other functions that ensure strictness. See the test suite
-- for assurance that this is safe to use.
--
foldr1WithKey :: (k -> v -> v -> v) -> NEInsOrdHashMap k v -> v
foldr1WithKey fn m =
  let
    assocs = toNonEmptyList m
  in
    List.foldr
      (\(k, v) acc -> fn k v acc)
      (snd $ NEL.last assocs)
      (NEL.init assocs)

foldMapWithKey :: Monoid m => (k -> v -> m) -> NEInsOrdHashMap k v -> m
foldMapWithKey fn =
  IOHM.foldMapWithKey fn . toInsOrdHashMap

unorderedFoldMap :: Monoid m => (v -> m) -> NEInsOrdHashMap k v -> m
unorderedFoldMap fn =
  IOHM.unorderedFoldMap fn . toInsOrdHashMap

unorderedFoldMapWithKey :: Monoid m
                        => (k -> v -> m) -> NEInsOrdHashMap k v -> m
unorderedFoldMapWithKey fn =
  IOHM.unorderedFoldMapWithKey fn . toInsOrdHashMap

filter :: (v -> Bool) -> NEInsOrdHashMap k v -> Maybe (NEInsOrdHashMap k v)
filter fn =
  nonEmpty . IOHM.filter fn . toInsOrdHashMap

filterWithKey :: (k -> v -> Bool)
              -> NEInsOrdHashMap k v
              -> Maybe (NEInsOrdHashMap k v)
filterWithKey fn =
  nonEmpty . IOHM.filterWithKey fn . toInsOrdHashMap

mapMaybe :: (v -> Maybe a)
         -> NEInsOrdHashMap k v
         -> Maybe (NEInsOrdHashMap k a)
mapMaybe fn =
  nonEmpty . IOHM.mapMaybe fn . toInsOrdHashMap

mapMaybeWithKey :: (k -> v -> Maybe a)
                -> NEInsOrdHashMap k v
                -> Maybe (NEInsOrdHashMap k a)
mapMaybeWithKey fn =
  nonEmpty . IOHM.mapMaybeWithKey fn . toInsOrdHashMap

-- Note that while this is using the partial 'NEL.fromList', this should never
-- actually fail because the only way we ever expose a means to create or edit
-- 'NEInsOrdHashMap's is through smart constructors and other functions that
-- ensure strictness. See the test suite for assurance that this is safe to
-- use.
--
keys :: NEInsOrdHashMap k v -> NEL.NonEmpty k
keys =
  NEL.fromList . IOHM.keys . toInsOrdHashMap

-- Note that while this is using the partial 'NEL.fromList', this should never
-- actually fail because the only way we ever expose a means to create or edit
-- 'NEInsOrdHashMap's is through smart constructors and other functions that
-- ensure strictness. See the test suite for assurance that this is safe to
-- use.
--
elems :: NEInsOrdHashMap k v -> NEL.NonEmpty v
elems =
  NEL.fromList . IOHM.elems . toInsOrdHashMap

toList :: NEInsOrdHashMap k v -> [(k, v)]
toList =
  IOHM.toList . toInsOrdHashMap

-- Note that while this is using the partial 'NEL.fromList', this should never
-- actually fail because the only way we ever expose a means to create or edit
-- 'NEInsOrdHashMap's is through smart constructors and other functions that
-- ensure strictness. See the test suite for assurance that this is safe to
-- use.
--
toNonEmptyList :: NEInsOrdHashMap k v -> NEL.NonEmpty (k, v)
toNonEmptyList =
  NEL.fromList . toList

fromList :: Hashable k => [(k, v)] -> Maybe (NEInsOrdHashMap k v)
fromList =
  fmap fromNonEmptyList . NEL.nonEmpty

fromNonEmptyList :: Hashable k => NEL.NonEmpty (k, v) -> NEInsOrdHashMap k v
fromNonEmptyList =
  NEInsOrdHashMap . IOHM.fromList . NEL.toList

nonEmpty :: InsOrdHashMap k v -> Maybe (NEInsOrdHashMap k v)
nonEmpty iohm =
  if IOHM.null iohm
    then Nothing
    else Just $ NEInsOrdHashMap iohm
