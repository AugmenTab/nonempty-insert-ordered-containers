{-# LANGUAGE TypeApplications #-}

module Tests.NEInsOrdHashSet
  ( neInsOrdHashSetTests
  ) where

import Control.DeepSeq (NFData, force)
import Control.Exception (SomeException, evaluate, try)
import Hedgehog ((===), (/==))
import Hedgehog qualified as HH
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as TastyHU
import Test.Tasty.Hedgehog qualified as TastyHH

import Data.NonEmpty.InsOrdHashSet (NEInsOrdHashSet)
import Data.NonEmpty.InsOrdHashSet qualified as NEIOHS

neInsOrdHashSetTests :: Tasty.TestTree
neInsOrdHashSetTests =
  Tasty.testGroup "NEInsOrdHashSet"
    [ TastyHU.testCase "nonEmpty mempty == Nothing" nonEmptyEmpty
    , TastyHU.testCase "nonEmpty singleton /= Nothing" nonEmptySingleton
    , TastyHU.testCase "fromList [] == Nothing" fromListNullIsNothing
    , TastyHH.testProperty "fromList NEL == Just" fromNEListIsJust
    , TastyHU.testCase "delete last element => Nothing" deleteLastElementIsNothing
    , TastyHH.testProperty "filter (const False) => Nothing" filterAllIsNothing
    , TastyHH.testProperty "union never crashes" unionNeverCrashes
    , TastyHH.testProperty "difference s s => Nothing" differenceSelfIsNothing
    , TastyHH.testProperty "toNonEmptyList never crashes" toNonEmptyListNeverCrashes
    , TastyHH.testProperty "map never crashes" mapNeverCrashes
    , TastyHU.testCase "member x (insert x s) == True" memberAfterInsertionIsTrue
    , TastyHU.testCase "notMember definition" notMemberDefinition
    ]

assertNoThrowNF :: NFData a => a -> HH.PropertyT IO ()
assertNoThrowNF x = do
  r <- HH.evalIO . try @SomeException . evaluate $ force x

  case r of
    Left _err -> HH.failure
    Right _a -> HH.success

genNEIOHS :: HH.Gen (NEInsOrdHashSet Int)
genNEIOHS =
  fmap NEIOHS.fromNonEmptyList $
    Gen.nonEmpty
      (Range.constant 1 10000)
      (Gen.int $ Range.constant 1 10000)

singleton :: NEInsOrdHashSet Int
singleton =
  NEIOHS.singleton 1

nonEmptyEmpty :: TastyHU.Assertion
nonEmptyEmpty =
  NEIOHS.nonEmpty mempty @?= (Nothing :: Maybe (NEInsOrdHashSet Int))

nonEmptySingleton :: TastyHU.Assertion
nonEmptySingleton =
  TastyHU.assertBool "singleton is empty" $
    NEIOHS.nonEmpty (NEIOHS.toInsOrdHashSet singleton)
      /= (Nothing :: Maybe (NEInsOrdHashSet Int))

fromListNullIsNothing :: TastyHU.Assertion
fromListNullIsNothing =
  NEIOHS.fromList [] @?= (Nothing :: Maybe (NEInsOrdHashSet Int))

fromNEListIsJust :: HH.Property
fromNEListIsJust =
  HH.property $ do
    xs <-
      HH.forAll $
        Gen.list (Range.constant 1 10000) (Gen.int $ Range.constant 1 10000)

    NEIOHS.fromList xs /== (Nothing :: Maybe (NEInsOrdHashSet Int))

deleteLastElementIsNothing :: TastyHU.Assertion
deleteLastElementIsNothing =
  NEIOHS.delete 1 singleton
    @?= (Nothing :: Maybe (NEInsOrdHashSet Int))

filterAllIsNothing :: HH.Property
filterAllIsNothing =
  HH.property $ do
    s <- HH.forAll genNEIOHS

    NEIOHS.filter (const False) s === (Nothing :: Maybe (NEInsOrdHashSet Int))

unionNeverCrashes :: HH.Property
unionNeverCrashes =
  HH.property $ do
    a <- HH.forAll genNEIOHS
    b <- HH.forAll genNEIOHS

    assertNoThrowNF . NEIOHS.toNonEmptyList $ NEIOHS.union a b

differenceSelfIsNothing :: HH.Property
differenceSelfIsNothing =
  HH.property $ do
    s <- HH.forAll genNEIOHS
    NEIOHS.difference s s === (Nothing :: Maybe (NEInsOrdHashSet Int))

toNonEmptyListNeverCrashes :: HH.Property
toNonEmptyListNeverCrashes =
  HH.property $
    assertNoThrowNF . NEIOHS.toNonEmptyList =<< HH.forAll genNEIOHS

mapNeverCrashes :: HH.Property
mapNeverCrashes =
  HH.property $ do
    let
      zero = 0 :: Int

    assertNoThrowNF . NEIOHS.toNonEmptyList . NEIOHS.map (const zero)
      =<< HH.forAll genNEIOHS

memberAfterInsertionIsTrue :: TastyHU.Assertion
memberAfterInsertionIsTrue =
  NEIOHS.member 2 (NEIOHS.insert 2 singleton) @?= True

notMemberDefinition :: TastyHU.Assertion
notMemberDefinition =
  NEIOHS.notMember 1 singleton @?= not (NEIOHS.member 1 singleton)
