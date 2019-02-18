module Test.Main where

import Test.ArrayView (checkCombinations, checkSlices, checkWithIndex, checkWithPredicate)
import Test.ArrayView.Common (both, inspect)
import Test.ArrayView.Laws (checkLaws)
import Test.ArrayView.NonEmpty (checkNonEmptyCombinations, checkNonEmptySlices, checkNonEmptyWithIndex, checkNonEmptyWithPredicate)
import Test.ArrayView.API

import Data.Array as A
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NE
import Data.ArrayView as AV
import Data.ArrayView.Internal (ArrayView, NonEmptyArrayView, fromArray, toArray)
import Data.ArrayView.NonEmpty as NEAV
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, compare, const, discard, mod, negate, pure, show, unit, (<), (<>), (==), (>))
import Test.Assert (assertEqual)


-- | Some constants used in tests
config :: { debug :: Boolean
          , lengths :: Array Int
          , indices :: Array Int
          , additionalIndex :: Array Int
          , predicates :: Array (Int -> Boolean)
          }
config =
  { debug: false --  set to true to get verbose logs
  , lengths: 0 A... 12
  , indices: -10 A... 10
  , additionalIndex: -5 A... 10
  , predicates:
    [ (_ > 5)
    , const false
    , const true
    , (\x -> x `mod` 2 == 1)
    , (\x -> x `mod` 2 == 0)
    , (_ < 5)
    ]
  }

main :: Effect Unit
main = do
  log "checking assetions"
  checkAssertions
  log "checking edge cases"
  checkEdgeCases
  checkLaws
  log "checking ArrayToView"
  checkArrayToView


checkAssertions :: Effect Unit
checkAssertions = do
  -- Good old assertion testing.
  -- In our case we need to check some properties after slicing, to ensure that
  -- indices are correct.

  -- For all possible lengths...
  for_ config.lengths \len -> do
    let a  = A.range 1 len
        av = AV.range 1 len

    assertEqual { expected: a
                , actual: toArray (fromArray a) }

    logDebug ("-----------------------\n" <>
              " a: "   <> show a <>
              " av: "  <> inspect av)

    -- for all possible indices i & j...
    for_ config.indices \i -> do
      for_ config.indices \j -> do
        -- ...check that slices from i to j are equal
        let aslice = A.slice i j a
            avslice = AV.slice i j av
            nonempties = both (NE.fromArray aslice) (NEAV.fromArrayView avslice)

        logDebug (" len: " <> show len <>
                  " i: "   <> show i <>
                  " j: "   <> show j <>
                  " aslice: " <> show aslice <>
                  " avslice: " <> inspect avslice)

        checkSlices i j avslice aslice
        for_ nonempties (checkNonEmptySlices i j)

        -- test functions that require additional index
        for_ config.additionalIndex \ix -> do
          checkWithIndex ix avslice aslice
          for_ nonempties (checkNonEmptyWithIndex ix)

        -- test functions that require a predicate
        for_ config.predicates \pred -> do
          checkWithPredicate pred avslice aslice
          for_ nonempties (checkNonEmptyWithPredicate pred)

        -- test functions that require two array views
        for_ config.additionalIndex \n -> do
          checkCombinations
            (AV.take n avslice)
            (AV.drop n avslice)
            (A.take n aslice)
            (A.drop n aslice)
          for_ nonempties (checkNonEmptyCombinations n)


-- Covering edge cases
checkEdgeCases :: Effect Unit
checkEdgeCases = do

  let arrs = [ -- check for off-by-one errors
               Tuple [1,2,3,4] [1,2,3,5]
             , Tuple [1,2,3,4] [1,2,3,3]
             , Tuple [1,2,3,5] [1,2,3,4]
             , Tuple [1,2,3,3] [1,2,3,4]

             , Tuple [1,2,3,4] [0,2,3,4]
             , Tuple [1,2,3,4] [2,2,3,4]
             , Tuple [0,2,3,4] [1,2,3,4]
             , Tuple [2,2,3,4] [1,2,3,4]

             , Tuple [1,2,3,4] [1,2,3,4,5]
             , Tuple [1,2,3,4] [1,2,3,4,3]
             , Tuple [1,2,3,4,3] [1,2,3,4]
             , Tuple [1,2,3,4,5] [1,2,3,4]

               -- check that `compare` traverses the structure
               -- in correct order (i.e. left-to-right)
             , Tuple [1,1,1,1,1] [1,0,1,2,1]
             , Tuple [1,1,1,1,1] [1,2,1,0,1]
             , Tuple [1,1,1,1,1] [1,0,1,2,1,1]
             , Tuple [1,1,1,1,1] [1,2,1,0,1,1]

             , Tuple [] []
             , Tuple [] [1]
             , Tuple [] [1,2,3]
             , Tuple [1] []
             , Tuple [1,2,3] []
             ]

  for_ arrs \(Tuple xs ys) -> do
    logDebug ("xs: " <> show xs <> ", " <>
              "ys: " <> show ys)

    -- in Eq instance definition
    assertEqual { expected: xs == ys
                , actual: fromArray xs == fromArray ys }
    assertEqual { expected: NE.fromArray xs == NE.fromArray ys
                , actual: NEAV.fromArrayView (fromArray xs) == NEAV.fromArrayView (fromArray ys) }

    -- in Ord instance defintion
    assertEqual { expected: xs `compare` ys
                , actual: fromArray xs `compare` fromArray ys }
    assertEqual { expected: NE.fromArray xs `compare` NE.fromArray ys
                , actual: NEAV.fromArrayView (fromArray xs) `compare` NEAV.fromArrayView (fromArray ys) }


checkArrayToView :: Effect Unit
checkArrayToView = do
  let arr = [1,2,3]
  -- check that view converts between Array and ArrayView
  assertEqual { expected: (AV.use (AV.use :: Array Int -> ArrayView Int) ::
                              ArrayView Int -> Array Int) (fromArray arr)
              , actual: arr }
  assertEqual { expected: (AV.use (AV.use :: ArrayView Int -> Array Int) ::
                              Array Int -> ArrayView Int) arr
              , actual: fromArray arr }

  -- and between NonEmptyArrayArray and NonEmptyArrayView
  assertEqual { expected: (AV.use (AV.use :: NonEmptyArray Int -> NonEmptyArrayView Int) ::
                              NonEmptyArrayView Int -> NonEmptyArray Int) (pure 0)
              , actual: pure 0 }
  assertEqual { expected: (AV.use (AV.use :: NonEmptyArrayView Int -> NonEmptyArray Int) ::
                              NonEmptyArray Int -> NonEmptyArrayView Int) (pure 0)
              , actual: pure 0 }


logDebug :: String -> Effect Unit
logDebug = if config.debug then log else const (pure unit)
