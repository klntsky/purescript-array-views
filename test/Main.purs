module Test.Main where

import Prelude
import Data.ArrayView as AV
import Data.ArrayView (ArrayView, fromArray, toArray)
import Data.Array as A
import Test.Assert
import Effect
import Data.Foldable
import Effect.Console
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe

debug :: Boolean
debug = false

logDebug :: String -> Effect Unit
logDebug = if debug then log else const (pure unit)

main :: Effect Unit
main = do
  for_ (0 A... 12) \len -> do
    let a  = A.range 1 len
        av = AV.range 1 len

    -- fromArray >>> toArray == identity
    assertEqual { expected: a
                , actual: toArray (fromArray a) }

    for_ (-10 A... 10) \i -> do
      for_ (-10 A...10) \j -> do
          logDebug (" a: "   <> show a <>
                    " av: "  <> show av <>
                    " len: " <> show len <>
                    " i: "   <> show i <>
                    " j: "   <> show j)
          let aslice = A.slice i j a
              avslice = AV.slice i j av

          -- Eq
          assert (avslice == avslice)

          -- Ord
          assertEqual { expected: aslice `compare` aslice
                      , actual: avslice `compare` avslice }
          assertEqual { expected: A.reverse aslice `compare` aslice
                      , actual: AV.reverse avslice `compare` avslice }

          -- fromArray <<< toArray == identity
          assertEqual { expected: avslice
                      , actual: fromArray (toArray avslice) }

          assertEqual { expected: aslice
                      , actual: toArray avslice }

          assertEqual { expected: avslice
                      , actual: fromArray aslice }

          -- null
          assertEqual { expected: A.null aslice
                      , actual: AV.null avslice }

          -- length
          assertEqual { expected: A.length aslice
                      , actual: AV.length avslice }

          -- cons
          assertEqual { expected: A.cons 0 aslice
                      , actual: toArray (AV.cons 0 avslice) }

          -- snoc
          assertEqual { expected: A.snoc aslice 0
                      , actual: toArray (AV.snoc avslice 0) }

          -- head
          assertEqual { expected: A.head aslice
                      , actual: AV.head avslice }

          -- last
          assertEqual { expected: A.last aslice
                      , actual: AV.last avslice }

          -- tail
          assertEqual { expected: A.tail aslice
                      , actual: map toArray (AV.tail avslice) }

          -- init
          assertEqual { expected: A.init aslice
                      , actual: map toArray (AV.init avslice) }

          -- uncons
          assertEqual { expected: A.uncons aslice
                      , actual: map fixTail (AV.uncons avslice) }

          -- unsnoc
          assertEqual { expected: A.unsnoc aslice
                      , actual: map fixInit (AV.unsnoc avslice) }

          for_ (-1 A... 10) \ix -> do
            -- index
            assertEqual  { expected: A.index aslice ix
                         , actual: AV.index avslice ix }

            -- elemIndex
            assertEqual { expected: A.elemIndex ix (aslice <> aslice)
                        , actual: AV.elemIndex ix  (avslice <> avslice) }

            -- elemLastIndex
            assertEqual { expected: A.elemLastIndex ix (aslice <> aslice)
                        , actual: AV.elemLastIndex ix  (avslice <> avslice) }

            -- findIndex
            assertEqual { expected: A.findIndex (_ == ix) (aslice <> aslice)
                        , actual: AV.findIndex (_ == ix)  (avslice <> avslice) }

            -- findLastIndex
            assertEqual { expected: A.findIndex (_ == ix) (aslice <> aslice)
                        , actual: AV.findIndex (_ == ix)  (avslice <> avslice) }

            -- insertAt
            assertEqualsMaybe
              (AV.insertAt ix ix avslice)
              (A.insertAt ix ix aslice)

            -- deleteAt
            assertEqualsMaybe
              (AV.deleteAt ix avslice)
              (A.deleteAt ix aslice)

            -- updateAt
            assertEqualsMaybe
              (AV.updateAt ix 0 avslice)
              (A.updateAt ix 0 aslice)

            -- modifyAt
            assertEqualsMaybe
              (AV.modifyAt ix (_ + 1) avslice)
              (A.modifyAt ix (_ + 1) aslice)

            -- modifyAtIndices
            -- alterAt
            -- reverse
            -- concat
            -- concatMap

            -- filter
            assertEquals
              (AV.filter even avslice)
              (A.filter even aslice)

            -- partition
            -- filterA
            -- mapMaybe
            -- catMaybes
            -- mapWithIndex
            -- sort
            -- sortBy
            -- sortWith
            -- slice

            assertEquals
              (AV.slice i ix avslice)
              (A.slice  i ix aslice)

fixTail :: forall a. { tail :: ArrayView a, head :: a } -> { head :: a, tail :: Array a }
fixTail { head, tail } = { head, tail: toArray tail }

fixInit :: forall a. { init :: ArrayView a, last :: a } -> { last :: a, init :: Array a }
fixInit { last, init } = { last, init: toArray init }

even :: Int -> Boolean
even n = n `mod` 2 == 0

assertEquals :: forall a. Eq a => Show a => ArrayView a -> Array a -> Effect Unit
assertEquals av a = do
  assertEqual { expected: av
              , actual: fromArray a }
  assertEqual { expected: a
              , actual: toArray av }

assertEqualsMaybe :: forall a. Eq a => Show a => Maybe (ArrayView a) -> Maybe (Array a) -> Effect Unit
assertEqualsMaybe av a = do
  assertEqual { expected: av
              , actual: map fromArray a }
  assertEqual { expected: a
              , actual: map toArray av }
