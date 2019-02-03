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
          assertEqual { expected: aslice == aslice
                      , actual: avslice == avslice }

          -- Ord
          assertEqual { expected: aslice `compare` aslice
                      , actual: avslice `compare` avslice }

          -- fromArray <<< toArray == identity
          assertEqual { expected: avslice
                      , actual: fromArray (toArray avslice) }

          assertEqual { expected: aslice
                      , actual: toArray avslice }

          assertEqual { expected: A.head aslice
                      , actual: AV.head avslice }

          assertEqual { expected: A.last aslice
                      , actual: AV.last avslice }

          assertEqual { expected: A.tail aslice
                      , actual: map toArray (AV.tail avslice) }

          assertEqual { expected: A.init aslice
                      , actual: map toArray (AV.init avslice) }

          assertEqual { expected: A.uncons aslice
                      , actual: map fixTail (AV.uncons avslice) }

          assertEqual { expected: A.unsnoc aslice
                      , actual: map fixInit (AV.unsnoc avslice) }

fixTail :: forall a. { tail :: ArrayView a, head :: a } -> { head :: a, tail :: Array a }
fixTail { head, tail } = { head, tail: toArray tail }

fixInit :: forall a. { init :: ArrayView a, last :: a } -> { last :: a, init :: Array a }
fixInit { last, init } = { last, init: toArray init }
