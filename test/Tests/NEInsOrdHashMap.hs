{-# LANGUAGE TypeApplications #-}

module Tests.NEInsOrdHashMap
  ( neInsOrdHashMapTests
  ) where

import Control.DeepSeq (NFData, force)
import Control.Exception (SomeException, evaluate, try)
import Data.Foldable1 qualified as Foldable1
import Data.List.NonEmpty qualified as NEL
import Hedgehog ((===))
import Hedgehog qualified as HH
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as TastyHU
import Test.Tasty.Hedgehog qualified as TastyHH

import Data.NonEmpty.InsOrdHashMap (NEInsOrdHashMap)
import Data.NonEmpty.InsOrdHashMap qualified as NEIOHM

neInsOrdHashMapTests :: Tasty.TestTree
neInsOrdHashMapTests =
  Tasty.testGroup "NEInsOrdHashMap"
    [ TastyHH.testProperty "no partial crashes" noPartialCrashes
    , TastyHH.testProperty "keys/elems matches toNonEmptyList" keysElemsMatchesToNonEmptyList
    , TastyHH.testProperty "foldl1' matches NEL.foldl1'" foldl1'Matches
    , TastyHH.testProperty "foldr1 matches NEL.foldr1" foldr1Matches
    , TastyHU.testCase "nonEmpty mempty == Nothing" nonEmptyEmpty
    , TastyHU.testCase "nonEmpty singleton /= Nothing" nonEmptySingleton
    , TastyHU.testCase "filter false => Nothing" filterFalseIsNothing
    , TastyHU.testCase "mapMaybe Nothing => Nothing" mapMaybeNothingIsNothing
    , TastyHU.testCase "delete all => Nothing" deleteAllIsNothing
    , TastyHH.testProperty "unions preserves non-emptiness" unionsPreservesNonEmptiness
    ]

assertNoThrowNF :: NFData a => a -> HH.PropertyT IO ()
assertNoThrowNF x = do
  r <- HH.evalIO . try @SomeException . evaluate $ force x

  case r of
    Left _err -> HH.failure
    Right _a -> HH.success

genNEIOHM :: HH.Gen (NEInsOrdHashMap Int Int)
genNEIOHM = do
  let
    intGen = Gen.int $ Range.constant 1 10000

  size <- intGen
  nel <-
    NEL.zip
      <$> Gen.nonEmpty (Range.singleton size) intGen
      <*> Gen.nonEmpty (Range.singleton size) intGen

  pure $ NEIOHM.fromNonEmptyList nel

singleton :: NEInsOrdHashMap Int Int
singleton =
  NEIOHM.singleton 1 1

noPartialCrashes :: HH.Property
noPartialCrashes =
  HH.property $ do
    m <- HH.forAll genNEIOHM

    assertNoThrowNF (NEIOHM.toNonEmptyList m)
    assertNoThrowNF (NEIOHM.keys m)
    assertNoThrowNF (NEIOHM.elems m)
    assertNoThrowNF (NEIOHM.foldl1' (+) m)
    assertNoThrowNF (NEIOHM.foldr1 (+) m)
    assertNoThrowNF (NEIOHM.foldl1WithKey' (\acc k v -> acc + k + v) m)
    assertNoThrowNF (NEIOHM.foldr1WithKey (\k v acc -> k + v + acc) m)

keysElemsMatchesToNonEmptyList :: HH.Property
keysElemsMatchesToNonEmptyList =
  HH.property $ do
    m <- HH.forAll genNEIOHM

    let
      ne = NEIOHM.toNonEmptyList m

    NEIOHM.keys m === fmap fst ne
    NEIOHM.elems m === fmap snd ne

foldl1'Matches :: HH.Property
foldl1'Matches =
  HH.property $ do
    m <- HH.forAll genNEIOHM

    NEIOHM.foldl1' (+) m
      === Foldable1.foldl1' (+) (snd <$> NEIOHM.toNonEmptyList m)

foldr1Matches :: HH.Property
foldr1Matches =
  HH.property $ do
    m <- HH.forAll genNEIOHM

    NEIOHM.foldr1 (+) m
      === Foldable1.foldr1 (+) (snd <$> NEIOHM.toNonEmptyList m)

nonEmptyEmpty :: TastyHU.Assertion
nonEmptyEmpty =
  NEIOHM.nonEmpty mempty @?= (Nothing :: Maybe (NEInsOrdHashMap Int Int))

nonEmptySingleton :: TastyHU.Assertion
nonEmptySingleton =
  TastyHU.assertBool "singleton is empty" $
    NEIOHM.nonEmpty (NEIOHM.toInsOrdHashMap singleton)
      /= (Nothing :: Maybe (NEInsOrdHashMap Int Int))

filterFalseIsNothing :: TastyHU.Assertion
filterFalseIsNothing =
  NEIOHM.filter (const False) singleton
    @?= (Nothing :: Maybe (NEInsOrdHashMap Int Int))

mapMaybeNothingIsNothing :: TastyHU.Assertion
mapMaybeNothingIsNothing =
  NEIOHM.mapMaybe (const Nothing) singleton
    @?= (Nothing :: Maybe (NEInsOrdHashMap Int Int))

deleteAllIsNothing :: TastyHU.Assertion
deleteAllIsNothing =
  NEIOHM.delete 1 singleton
    @?= (Nothing :: Maybe (NEInsOrdHashMap Int Int))

unionsPreservesNonEmptiness :: HH.Property
unionsPreservesNonEmptiness =
  HH.property $ do
    ms <- HH.forAll $ Gen.nonEmpty (Range.linear 1 10) genNEIOHM

    assertNoThrowNF . NEIOHM.toNonEmptyList $ NEIOHM.unions ms
