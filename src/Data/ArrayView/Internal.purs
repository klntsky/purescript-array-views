module Data.ArrayView.Internal
  ( ArrayView (..)
  , fromArray
  , toArray

  , singleton
  , length
  , index, (!!)
  , concatMap

  , NonEmptyArrayView (..)
  , class ArrayToView
  , use

  , whenNonEmpty

  , fromNEAV
  )
where

import Data.Array as A
import Data.Array.NonEmpty as NEA
import Data.Eq (class Eq1)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex, foldlWithIndex, foldrWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.NonEmpty (NonEmpty, (:|))
import Data.NonEmpty as NE
import Data.Ord (class Ord1)
import Data.Ordering (Ordering(..))
import Data.Traversable (class Foldable, class Traversable, foldMap, foldl, foldr, sequenceDefault, traverse)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndexDefault)
import Data.Unfoldable (class Unfoldable, unfoldr)
import Data.Unfoldable1 (class Unfoldable1, unfoldr1)
import Prelude (class Applicative, class Apply, class Bind, class Eq, class Functor, class Monad, class Monoid, class Ord, class Semigroup, class Show, type (~>), append, apply, compare, eq, map, otherwise, show, (&&), (+), (-), (<), (<<<), (<>), (==), (>=), (>>>))
import Data.Array.NonEmpty as NEA

import Data.Eq (class Eq1)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex, foldlWithIndex, foldrWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Ord (class Ord1)
import Data.Semigroup.Foldable (class Foldable1, fold1Default, foldMap1)
import Data.Semigroup.Traversable (class Traversable1, sequence1, traverse1Default)
import Data.Traversable (class Foldable, class Traversable, foldMap, foldl, foldr)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndexDefault)
import Data.Unfoldable1 (class Unfoldable1)
import Prelude (class Applicative, class Apply, class Bind, class Eq, class Functor, class Monad, class Ord, class Semigroup, class Show, type (~>), ap, map, mempty, show, (+), (<<<), (<>), (>>>))

-- * ArrayView

newtype ArrayView a = View { from :: Int, len :: Int, arr :: Array a }

derive instance newtypeArrayView :: Newtype (ArrayView a) _

derive instance genericArrayView :: Generic (ArrayView a) _

instance showArrayView :: Show a => Show (ArrayView a) where
  show av  = "fromArray " <> show (toArray av)

instance eqArrayView :: Eq a => Eq (ArrayView a) where
  eq xs ys = lenXs == length ys && go lenXs
    where
      lenXs = length xs
      go (-1) = true
      go i    = if xs !! i == ys !! i then
                  go (i - 1)
                else
                  false

instance eq1ArrayView :: Eq1 ArrayView where
  eq1 xs ys = xs `eq` ys

instance ordArrayView :: Ord a => Ord (ArrayView a) where
  compare xs ys = go 0
    where
      compareLengths = compare (length xs) (length ys)
      go i =
        case xs !! i, ys !! i of
          Just x, Just y -> let cmprsn = compare x y in
            if cmprsn == EQ then
              go (i + 1)
            else cmprsn
          _, _ -> compareLengths

instance ord1ArrayView :: Ord1 ArrayView where
  compare1 xs ys = xs `compare` ys

instance functorArrayView :: Functor ArrayView where
  map f = toArray >>> map f >>> fromArray

instance applyArrayView :: Apply ArrayView where
  apply f arr = fromArray (apply (toArray f) (toArray arr))

instance bindArrayView :: Bind ArrayView where
  bind m f = concatMap f m

instance applicativeArrayView :: Applicative ArrayView where
  pure = singleton

instance monadArrayView :: Monad ArrayView

instance functorWithIndexArrayView :: FunctorWithIndex Int ArrayView where
  mapWithIndex f = use (A.mapWithIndex f)

instance foldableWithIndexArrayView :: FoldableWithIndex Int ArrayView where
  foldrWithIndex f z = toArray >>> foldrWithIndex f z
  foldlWithIndex f z = toArray >>> foldlWithIndex f z
  foldMapWithIndex f = toArray >>> foldMapWithIndex f

instance traversableWithIndexArrayView :: TraversableWithIndex Int ArrayView where
  traverseWithIndex = traverseWithIndexDefault

instance foldableArrayView :: Foldable ArrayView where
  foldl f z = toArray >>> foldl f z
  foldr f z = toArray >>> foldr f z
  foldMap f = toArray >>> foldMap f

instance traversableArrayView :: Traversable ArrayView where
  traverse f av = map fromArray (traverse f (toArray av))
  sequence = sequenceDefault

instance unfoldable1ArrayView :: Unfoldable1 ArrayView where
  unfoldr1 f z = fromArray (unfoldr1 f z)

instance unfoldableArrayView :: Unfoldable ArrayView where
  unfoldr f z = fromArray (unfoldr f z)

instance semigroupArrayView :: Semigroup (ArrayView a) where
  append = use (append :: Array a -> Array a -> Array a)

instance monoidArrayView :: Monoid (ArrayView a) where
  mempty = empty


index :: forall a. ArrayView a -> Int -> Maybe a
index av @ (View { from, len, arr }) ix
  | ix >= 0 && ix < len = arr A.!! (from + ix)
  | otherwise = Nothing

infixl 8 index as !!

singleton :: forall a. a -> ArrayView a
singleton a = View { from: 0, len: 1, arr: [a] }

length :: forall a. ArrayView a -> Int
length (View { len }) = len


fromArray :: Array ~> ArrayView
fromArray arr = let len = A.length arr in
  View { from: 0, len, arr }

toArray :: ArrayView ~> Array
toArray (View { from, len, arr })
  | from == 0 && A.length arr == len =
    arr
  | otherwise =
    A.slice from (from + len) arr

concatMap :: forall a b. (a -> ArrayView b) -> ArrayView a -> ArrayView b
concatMap = use (A.concatMap :: (a -> Array b) -> Array a -> Array b)

-- * NonEmptyArrayView

newtype NonEmptyArrayView a = NonEmptyArrayView (NonEmpty ArrayView a)

derive instance newtypeNonEmptyArrayView :: Newtype (NonEmptyArrayView a) _

instance showNonEmptyArrayView :: Show a => Show (NonEmptyArrayView a) where
  show (NonEmptyArrayView neav) =
    "NonEmptyArrayView (" <>
    NE.fromNonEmpty (\x xs -> show x <> ":|" <> show xs) neav <>
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


-- internal

fromNonEmpty :: NEA.NonEmptyArray ~> NonEmpty ArrayView
fromNonEmpty nav = let t = NEA.uncons nav in
  t.head :| fromArray (t.tail)

toNonEmpty :: NonEmpty ArrayView ~> NEA.NonEmptyArray
toNonEmpty narr = NEA.cons' (NE.head narr) (toArray (NE.tail narr))

empty :: forall a. ArrayView a
empty = View { from: 0, len: 0, arr: [] }


-- | This typeclass allows to convert any function that operates on `Array` to a
-- | function that operates on `ArrayView` and vice versa. `use` only inserts
-- | `fromArray` and `toArray` in the right places, so don't expect it to
-- | increase performance.
-- |
-- | *Note*: either type annotation or partial application of some number of
-- | arguments is needed, because otherwise the type inference will not be
-- | able to guess the correct type.
-- |
-- | ```
-- | import Data.Array as A
-- |
-- | -- OK
-- | zipWith :: forall a b c. (a -> b -> c) -> ArrayView a -> ArrayView b -> ArrayView c
-- | zipWith = use (A.zipWith :: (a -> b -> c) -> Array a -> Array b -> Array c)
-- |
-- | -- OK
-- | zipWith :: forall a b c. (a -> b -> c) -> ArrayView a -> ArrayView b -> ArrayView c
-- | zipWith f = use (A.zipWith f) -- all three type parameters are tied to `f`
-- |
-- | -- Type error
-- | zipWith :: forall a b c. (a -> b -> c) -> ArrayView a -> ArrayView b -> ArrayView c
-- | zipWith = use A.zipWith
-- | ```
class ArrayToView a b where
  use :: a -> b

instance arrayToViewId :: ArrayToView a a where
  use x = x

else instance arrayToViewBi :: (ArrayToView b a, ArrayToView c d) => ArrayToView (a -> c) (b -> d) where
  use f x = use (f (use x))

else instance arrayToViewFrom :: ArrayToView a b => ArrayToView (Array a) (ArrayView b) where
  use = fromArray <<< map use

else instance arrayToViewTo :: ArrayToView a b => ArrayToView (ArrayView a) (Array b) where
  use = toArray <<< map use

else instance arrayToViewFromNEA :: ArrayToView (NEA.NonEmptyArray a) (NonEmpty ArrayView a) where
  use = fromNonEmpty

else instance arrayToViewToNEA :: ArrayToView (NE.NonEmpty ArrayView a) (NEA.NonEmptyArray a) where
  use = toNonEmpty

else instance arrayToViewFunctor :: (Functor f, ArrayToView a b) => ArrayToView (f a) (f b) where
  use = map use


fromNEAV :: forall a. NonEmptyArrayView a -> ArrayView a
fromNEAV (NonEmptyArrayView m) = NE.fromNonEmpty (use (A.cons :: a -> Array a -> Array a)) m

fromNonEmptyArray :: NEA.NonEmptyArray ~> NonEmptyArrayView
fromNonEmptyArray m =
  wrap (case NEA.toNonEmpty m of
           a :| as -> a :| fromArray as)

toNonEmptyArray :: NonEmptyArrayView ~> NEA.NonEmptyArray
toNonEmptyArray (NonEmptyArrayView (x :| xs)) = NEA.fromNonEmpty (x :| toArray xs)

whenNonEmpty :: forall a b. (ArrayView a -> b) -> ArrayView a -> Maybe b
whenNonEmpty _ (View { len: 0 }) = Nothing
whenNonEmpty f av           = Just (f av)
