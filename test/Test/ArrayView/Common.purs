module Test.ArrayView.Common
       ( fixYesNo
       , fixTail
       , fixInit
       , fixInitRest
       , assertEquals
       , assertEqualsMaybe
       , equal
       , both
       , inspect
       )
where

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

fixYesNo :: forall a.
            { no :: Array a,     yes :: Array a } ->
            { no :: ArrayView a, yes :: ArrayView a }
fixYesNo { yes, no } = { yes: fromArray yes, no: fromArray no }

fixTail :: forall a.
           { tail :: ArrayView a, head :: a } ->
           { tail :: Array a,     head :: a }
fixTail { head, tail } = { head, tail: toArray tail }

fixInit :: forall a.
           { init :: ArrayView a, last :: a } ->
           { init :: Array a,     last :: a }
fixInit { last, init } = { last, init: toArray init }

fixInitRest :: forall a.
               { init :: Array a ,    rest :: Array a} ->
               { init :: ArrayView a, rest :: ArrayView a }
fixInitRest { init, rest } = { init: fromArray init
                             , rest: fromArray rest }

assertEquals :: forall a. Eq a => Show a =>
                ArrayView a -> Array a -> Effect Unit
assertEquals av a = do
  assertEqual { expected: av
              , actual: fromArray a }
  assertEqual { expected: a
              , actual: toArray av }

assertEqualsMaybe :: forall a. Eq a => Show a =>
                     Maybe (ArrayView a) -> Maybe (Array a) -> Effect Unit
assertEqualsMaybe av a = do
  assertEqual { expected: av
              , actual: map fromArray a }
  assertEqual { expected: a
              , actual: map toArray av }

both :: forall a b. Maybe a -> Maybe b -> Maybe (Tuple a b)
both ma mb = (ma # map Tuple) <*> mb

inspect :: forall a. Show a => ArrayView a -> String
inspect = genericShow

equal :: forall a b. Eq b => Show b => ArrayToView a b => a -> b -> Effect Unit
equal a b =
  assertEqual { actual: use a
              , expected: b }
