module Test.Main where

import Prelude
import Data.ArrayView as AV
import Data.Array as A
import Test.Assert
import Effect
import Data.Foldable
import Effect.Console

debug :: Boolean
debug = false

logDebug :: String -> Effect Unit
logDebug = if debug then log else const (pure unit)

main :: Effect Unit
main = do
  for_ (-10 A... 10) \i -> do
    for_ (-10 A...10) \j -> do
      for_ (0 A... 12) \len -> do
          let a  = A.range 1 len
              av = AV.range 1 len
          logDebug (" a: "   <> show a <>
                    " av: "  <> show av <>
                    " len: " <> show len <>
                    " i: "   <> show i <>
                    " j: "   <> show j)
          assertEqual { expected: A.slice i j a
                      , actual: AV.toArray (AV.slice i j av) }
