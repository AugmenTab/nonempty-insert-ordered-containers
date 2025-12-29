module Main
  ( main
  ) where

import Test.Tasty qualified as Tasty

import Tests.NEInsOrdHashMap qualified as NEIOHM
import Tests.NEInsOrdHashSet qualified as NEIOHS

main :: IO ()
main = do
  Tasty.defaultMain $
    Tasty.testGroup "NonEmpty Insert Ordered Containers Tests"
      [ NEIOHM.neInsOrdHashMapTests
      , NEIOHS.neInsOrdHashSetTests
      ]
