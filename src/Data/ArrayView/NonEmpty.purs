module Data.ArrayView.NonEmpty
       ( NonEmptyArrayView (..)
       )
where

import Data.Array.NonEmpty as NEA
import Data.ArrayView (ArrayView, concatMap, cons, fromArray, toArray)
import Data.Eq (class Eq1)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex, foldlWithIndex, foldrWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.NonEmpty (NonEmpty, fromNonEmpty, (:|))
import Data.Ord (class Ord1)
import Data.Semigroup.Foldable (class Foldable1, fold1Default, foldMap1)
import Data.Semigroup.Traversable (class Traversable1, sequence1, traverse1Default)
import Data.Traversable (class Foldable, class Traversable, foldMap, foldl, foldr)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndexDefault)
import Data.Unfoldable1 (class Unfoldable1)
import Prelude (class Applicative, class Apply, class Bind, class Eq, class Functor, class Monad, class Ord, class Semigroup, class Show, type (~>), ap, map, mempty, show, (+), (<<<), (<>), (>>>))


newtype NonEmptyArrayView a = NonEmptyArrayView (NonEmpty ArrayView a)

derive instance newtypeNonEmptyArrayView :: Newtype (NonEmptyArrayView a) _

instance showNonEmptyArrayView :: Show a => Show (NonEmptyArrayView a) where
  show (NonEmptyArrayView neav) =
    "NonEmptyArrayView (" <>
    fromNonEmpty (\x xs -> show x <> ":|" <> show xs) neav <>
    ")"

derive newtype instance eqNEArrayView :: Eq a => Eq (NonEmptyArrayView a)
derive newtype instance eq1NEArrayView :: Eq1 NonEmptyArrayView
derive newtype instance ordNEArrayView :: Ord a => Ord (NonEmptyArrayView a)
derive newtype instance ord1NEArrayView :: Ord1 NonEmptyArrayView
derive newtype instance functorNEArrayView :: Functor NonEmptyArrayView

instance semigroupNonEmptyArrayView :: Semigroup (NonEmptyArrayView a) where
  append (NonEmptyArrayView (a :| as)) nebs =
    wrap (a :| as <> fromNEAV nebs)

instance functorWithIndexNonEmptyArrayView :: FunctorWithIndex Int NonEmptyArrayView where
  mapWithIndex f (NonEmptyArrayView (a :| fa)) =
    wrap (f 0 a :| mapWithIndex (f <<< (_ + 1)) fa)

instance applyNonEmptyArrayView :: Apply NonEmptyArrayView where
  apply = ap

instance bindNonEmptyArrayView :: Bind NonEmptyArrayView where
  bind (NonEmptyArrayView (x :| xs)) f =
    case unwrap (f x) of
      (y :| ys) ->
        wrap (y :| ys <> concatMap (fromNEAV <<< f) xs)

instance applicativeNonEmptyArrayView :: Applicative NonEmptyArrayView where
  pure x = NonEmptyArrayView (x :| mempty)

instance monadNonEmptyArrayView :: Monad NonEmptyArrayView

instance foldableNonEmptyArrayView :: Foldable NonEmptyArrayView where
  foldl f z = unwrap >>> foldl f z
  foldr f z = unwrap >>> foldr f z
  foldMap f = unwrap >>> foldMap f

instance foldableWithIndexNonEmptyArrayView :: FoldableWithIndex Int NonEmptyArrayView where
  foldrWithIndex f z = fromNEAV >>> foldrWithIndex f z
  foldlWithIndex f z = fromNEAV >>> foldlWithIndex f z
  foldMapWithIndex f = fromNEAV >>> foldMapWithIndex f

instance foldable1NonEmptyArrayView :: Foldable1 NonEmptyArrayView where
  foldMap1 f m = foldMap1 f (unwrap m)
  fold1 = fold1Default

derive newtype instance unfoldable1NonEmptyArrayView :: Unfoldable1 NonEmptyArrayView
derive newtype instance traversableNonEmptyArrayView :: Traversable NonEmptyArrayView

instance traversableWithIndexNonEmptyArrayView :: TraversableWithIndex Int NonEmptyArrayView where
  traverseWithIndex = traverseWithIndexDefault --  using `mapWithIndex` & `sequence`

instance traversable1NonEmptyArrayView :: Traversable1 NonEmptyArrayView where
  traverse1 = traverse1Default
  sequence1 m = map fromNonEmptyArray (sequence1 (toNonEmptyArray m))

-- * internal

fromNEAV :: forall a. NonEmptyArrayView a -> ArrayView a
fromNEAV (NonEmptyArrayView m) = fromNonEmpty cons m

fromNonEmptyArray :: NEA.NonEmptyArray ~> NonEmptyArrayView
fromNonEmptyArray m =
  wrap (case NEA.toNonEmpty m of
           a :| as -> a :| fromArray as)

toNonEmptyArray :: NonEmptyArrayView ~> NEA.NonEmptyArray
toNonEmptyArray (NonEmptyArrayView (x :| xs)) = NEA.fromNonEmpty (x :| toArray xs)
