module Test.ArrayView.NonEmpty
       ( checkNonEmptySlices
       , checkNonEmptyWithIndex
       , checkNonEmptyWithPredicate
       , checkNonEmptyCombinations
       )
where

import Test.ArrayView.Common

import Data.Array as A
import Data.ArrayView as AV
import Data.ArrayView.NonEmpty as NEAV
import Data.Array.NonEmpty as NE
import Data.Array.NonEmpty (NonEmptyArray)
import Data.ArrayView.Internal
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Profunctor.Strong ((***))
import Data.Traversable (class Foldable, class Traversable, for_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import Prelude (class Applicative, class Apply, class Bind, class Eq, class Functor, class Monad, class Monoid, class Semigroup, class Show, class Ord, Unit, compare, const, discard, flip, eq, map, mod, negate, not, pure, show, unit, ($), (&&), (+), (<), (<$>), (<>), (==), (>), (>=), (>>>), (<*>), (#))
import Test.Assert (assert, assertEqual, assertThrows)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Laws.Control (checkApplicative, checkApply, checkBind, checkMonad)
import Test.QuickCheck.Laws.Data (checkEq, checkFoldable, checkFunctor, checkMonoid, checkSemigroup)
import Type.Proxy (Proxy(..), Proxy2(..))


checkNonEmptySlices :: forall a.
                       Eq a => Show a =>
                       Int -> Int ->
                       Tuple (NonEmptyArray a) (NonEmptyArrayView a) -> Effect Unit
checkNonEmptySlices i j (Tuple nea neav) = do
  pure unit


checkNonEmptyWithIndex :: Int -> Tuple (NonEmptyArray Int) (NonEmptyArrayView Int) -> Effect Unit
checkNonEmptyWithIndex ix (Tuple ne neav) = do
  equal (NEAV.takeEnd ix neav) (NE.takeEnd ix ne)


checkNonEmptyWithPredicate :: forall a.
                              Eq a => Show a =>
                              (a -> Boolean) -> Tuple (NonEmptyArray a) (NonEmptyArrayView a) -> Effect Unit
checkNonEmptyWithPredicate f (Tuple nea neav) = do
  pure unit


checkNonEmptyCombinations :: forall a.
                             Eq a => Show a =>
                             Int -> Tuple (NonEmptyArray a) (NonEmptyArrayView a) -> Effect Unit
checkNonEmptyCombinations n (Tuple ne neav) = do
  pure unit
