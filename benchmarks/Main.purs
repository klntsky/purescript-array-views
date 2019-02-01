module BenchMain where

import Data.ArrayView as AV

import Prelude
import Effect (Effect)
import Data.List.Lazy as LL
import Data.List as L
import Data.Array as A
import Data.Maybe

import Benchotron.Core (Benchmark, benchFn, mkBenchmark)
import Benchotron.UI.Console (runSuite)
import Test.QuickCheck.Gen (stateful)


type Uncons list a = list a -> Maybe { head :: a, tail :: list a }


withUncons :: forall list.
           Uncons list Unit ->
           list Unit -> Unit
withUncons uncons = go
  where
    go :: list Unit -> Unit
    go list = case uncons list of
      Just { tail } -> go tail
      Nothing -> unit


benchWithUncons :: Benchmark
benchWithUncons = mkBenchmark
  { slug: "withUncons"
  , title: "Iterate list using uncons"
  , sizes: A.range 0 15 <#> (_ * 200)
  , sizeInterpretation: "n"
  , inputsPerSize: 1
  , gen: \n -> stateful (\seed -> pure n)
  , functions: [ benchFn "ArrayView" (\n -> withUncons AV.uncons (AV.replicate n unit))
               , benchFn "Array"     (\n -> withUncons A.uncons  (A.replicate  n unit))
               ]
  }


main :: Effect Unit
main = do
  runSuite [ benchWithUncons ]
