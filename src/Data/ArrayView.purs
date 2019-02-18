module Data.ArrayView
  ( fromFoldable
  , toUnfoldable
  , (..), range
  , replicate
  , some
  , many

  , null
  , (:), cons
  , snoc
  , insert
  , insertBy

  , head
  , last
  , tail
  , init
  , uncons
  , unsnoc

  , elemIndex
  , elemLastIndex
  , findIndex
  , findLastIndex
  , insertAt
  , deleteAt
  , updateAt
  , updateAtIndices
  , modifyAt
  , modifyAtIndices
  , alterAt

  , reverse
  , concat
  , filter
  , partition
  , filterA
  , mapMaybe
  , catMaybes
  , mapWithIndex

  , sort
  , sortBy
  , sortWith



  , slice
  , take
  , takeEnd
  , takeWhile
  , drop
  , dropEnd
  , dropWhile
  , span
  , group
  , group'
  , groupBy

  , nub
  , nubEq
  , nubBy
  , nubByEq
  , union
  , unionBy
  , delete
  , deleteBy

  , (\\), difference
  , intersect
  , intersectBy

  , zipWith
  , zipWithA
  , zip
  , unzip

  , foldM
  , foldRecM

  , unsafeIndex

  , force
  , module Exports
  )
where


import Data.ArrayView.Internal
  ( ArrayView
  , fromArray
  , toArray
  , singleton
  , length
  , index, (!!)
  , concatMap
  , class ArrayToView
  , use
  ) as Exports

import Control.Alternative (class Alternative)
import Control.Lazy (class Lazy)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Array as A
import Data.Array.NonEmpty (NonEmptyArray)
import Data.ArrayView.Internal
import Data.Foldable (foldl, foldr, foldMap, fold, intercalate, elem, notElem, find, findMap, any, all) as Exports
import Data.Maybe (Maybe(..))
import Data.Traversable (class Foldable)
import Data.Traversable (scanl, scanr) as Exports
import Data.Tuple (Tuple)
import Data.Unfoldable (class Unfoldable)
import Prelude (class Applicative, class Eq, class Monad, class Ord, type (~>), Ordering, bind, mempty, otherwise, pure, (&&), (+), (-), (<), (<=), (>), (>=), (>>>), (||))


fromFoldable :: forall f. Foldable f => f ~> ArrayView
fromFoldable = A.fromFoldable >>> fromArray

toUnfoldable :: forall f. Unfoldable f => ArrayView ~> f
toUnfoldable = toArray >>> A.toUnfoldable


range :: Int -> Int -> ArrayView Int
range = use A.range

infix 8 range as ..

null :: forall a. ArrayView a -> Boolean
null (View { len: 0 }) = true
null _                 = false

replicate :: forall a. Int -> a -> ArrayView a
replicate = use (A.replicate :: Int -> a -> Array a)

some :: forall f a. Alternative f => Lazy (f (Array a)) => f a -> f (ArrayView a)
some v = use (A.some v)

many :: forall f a. Alternative f => Lazy (f (Array a)) => f a -> f (ArrayView a)
many v = use (A.many v)

-- | *O(n)*
cons :: forall a. a -> ArrayView a -> ArrayView a
cons a = use (A.cons a)

infix 6 cons as :

-- | *O(n)*
snoc :: forall a. ArrayView a -> a -> ArrayView a
snoc = use (A.snoc :: Array a -> a -> Array a)

insert :: forall a. Ord a => a -> ArrayView a -> ArrayView a
insert a = use (A.insert a)

insertBy :: forall a. (a -> a -> Ordering) -> a -> ArrayView a -> ArrayView a
insertBy f = use (A.insertBy f)

head :: forall a. ArrayView a -> Maybe a
head av = av !! 0

last :: forall a. ArrayView a -> Maybe a
last av = av !! (length av - 1)


-- | Perform deferred `slice`. This function allows the garbage collector to
-- | free unused parts of the array referenced by given `ArrayView`.
-- |
-- | *O(n)*
-- |
-- | ```purescript
-- | force = toArray >>> fromArray
-- | ```
force :: forall a. ArrayView a -> ArrayView a
force = toArray >>> fromArray

elemIndex :: forall a. Eq a => a -> ArrayView a -> Maybe Int
elemIndex e = use (A.elemIndex e)

elemLastIndex :: forall a. Eq a => a -> ArrayView a -> Maybe Int
elemLastIndex e = use (A.elemLastIndex e)

findIndex :: forall a. (a -> Boolean) -> ArrayView a -> Maybe Int
findIndex p = use (A.findIndex p)

findLastIndex :: forall a. (a -> Boolean) -> ArrayView a -> Maybe Int
findLastIndex p = use (A.findLastIndex p)

insertAt :: forall a. Int -> a -> ArrayView a -> Maybe (ArrayView a)
insertAt ix e = use (A.insertAt ix e)

deleteAt :: forall a. Int -> ArrayView a -> Maybe (ArrayView a)
deleteAt = use (A.deleteAt :: Int -> Array a -> Maybe (Array a))

updateAt :: forall a. Int -> a -> ArrayView a -> Maybe (ArrayView a)
updateAt ix v = use (A.updateAt ix v)

updateAtIndices :: forall t a. Foldable t => t (Tuple Int a) -> ArrayView a -> ArrayView a
updateAtIndices t = use (A.updateAtIndices t)

modifyAt :: forall a. Int -> (a -> a) -> ArrayView a -> Maybe (ArrayView a)
modifyAt ix f = use (A.modifyAt ix f)

modifyAtIndices :: forall t a. Foldable t => t Int -> (a -> a) -> ArrayView a -> ArrayView a
modifyAtIndices t f = use (A.modifyAtIndices t f)

alterAt :: forall a. Int -> (a -> Maybe a) -> ArrayView a -> Maybe (ArrayView a)
alterAt = use (A.alterAt :: Int -> (a -> Maybe a) -> Array a -> Maybe (Array a))

reverse :: forall a. ArrayView a -> ArrayView a
reverse = use (A.reverse :: Array a -> Array a)

concat :: forall a. ArrayView (ArrayView a) -> ArrayView a
concat = use (A.concat :: Array (Array a) -> Array a)

filter :: forall a. (a -> Boolean) -> ArrayView a -> ArrayView a
filter f = use (A.filter f)

partition :: forall a. (a -> Boolean) -> ArrayView a -> { yes :: ArrayView a, no :: ArrayView a }
partition p = use (A.partition p) >>> fix
  where
    fix :: { no :: Array a, yes :: Array a } -> { yes :: ArrayView a, no :: ArrayView a }
    fix { yes, no } = { yes: fromArray yes, no: fromArray no }


-- | *O(1)*
slice :: forall a. Int -> Int -> ArrayView a -> ArrayView a
slice start' end' (View view @ { from, len, arr }) =
  if end <= start || start >= len
  then mempty --  forget about the original array
             -- (allow it to be GC'ed)
  else View view { from = from + start, len = end - start }
  where
    start = between 0 len (fix start')
    end   = between 0 len (fix end')
    between lb ub n =
      if n < lb
      then lb
      else if n > ub
           then ub
           else n
    fix n
      | n < 0 = len + n
      | otherwise = n

-- | *O(1)*
tail :: forall a. ArrayView a -> Maybe (ArrayView a)
tail = whenNonEmpty \(View view) -> View view { from = view.from + 1, len = view.len - 1 }

-- | *O(1)*
init :: forall a. ArrayView a -> Maybe (ArrayView a)
init = whenNonEmpty \(View view) -> View view { len = view.len - 1 }

-- | *O(1)*
uncons :: forall a. ArrayView a -> Maybe { head :: a, tail :: ArrayView a }
uncons av @ (View { from, arr }) = do
  head <- arr A.!! from
  tail <- tail av
  pure { head, tail }

-- | *O(1)*
unsnoc :: forall a. ArrayView a -> Maybe { init :: ArrayView a, last :: a }
unsnoc av @ (View { from, len, arr }) = do
  init <- init av
  last <- arr A.!! (from + len - 1)
  pure { init, last }

unsafeIndex :: forall a. Partial => ArrayView a -> Int -> a
unsafeIndex (View view @ { from, len, arr }) ix
  | ix < len && ix >= 0 = A.unsafeIndex arr (ix + from)

-- | *O(1)*
take :: forall a. Int -> ArrayView a -> ArrayView a
take n = slice 0 (toNonNegative n)

-- | *O(1)*
takeEnd :: forall a. Int -> ArrayView a -> ArrayView a
takeEnd n xs = drop (length xs - n) xs

-- | See also: `span`.
takeWhile :: forall a. (a -> Boolean) -> ArrayView a -> ArrayView a
takeWhile p xs = (span p xs).init

-- | *O(1)*
drop :: forall a. Int -> ArrayView a -> ArrayView a
drop n av = slice (toNonNegative n) (length av) av

-- | *O(1)*
dropEnd :: forall a. Int -> ArrayView a -> ArrayView a
dropEnd n xs = take (length xs - n) xs -- `toNonNegative` is not needed because
                                       -- `slice` will just return the whole
                                       -- `ArrayView` if the second argument is
                                       -- greater than the length.

-- | See also: `span`.
dropWhile :: forall a. (a -> Boolean) -> ArrayView a -> ArrayView a
dropWhile p xs = (span p xs).rest

-- | The time complexity of `span` only depends on the length of the resulting
-- | `init` ArrayView.
span :: forall a. (a -> Boolean) -> ArrayView a ->
        { init :: ArrayView a, rest :: ArrayView a }
span p av =
  -- `span` implementation from Data/Array.purs is copypasted here instead of
  -- reusing `Data.Array.span` because `slice` on `ArrayView` is O(1), and
  -- we can take advantage of it.
  case go 0 of
    Just 0 ->
      { init: mempty, rest: av }
    Just i ->
      { init: slice 0 i av, rest: slice i (length av) av }
    Nothing ->
      { init: av, rest: mempty }
  where
    go i =
      case index av i of
        Just x -> if p x then go (i + 1) else Just i
        Nothing -> Nothing


filterA :: forall a f. Applicative f => (a -> f Boolean) -> ArrayView a -> f (ArrayView a)
filterA f = use (A.filterA f)

mapMaybe :: forall a b. (a -> Maybe b) -> ArrayView a -> ArrayView b
mapMaybe f = use (A.mapMaybe f)

catMaybes :: forall a. ArrayView (Maybe a) -> ArrayView a
catMaybes = use (A.catMaybes :: Array (Maybe a) -> Array a)

mapWithIndex :: forall a b. (Int -> a -> b) -> ArrayView a -> ArrayView b
mapWithIndex f = use (A.mapWithIndex f)

sort :: forall a. Ord a => ArrayView a -> ArrayView a
sort = use (A.sort :: Array a -> Array a)

sortBy :: forall a. (a -> a -> Ordering) -> ArrayView a -> ArrayView a
sortBy f = use (A.sortBy f)

sortWith :: forall a b. Ord b => (a -> b) -> ArrayView a -> ArrayView a
sortWith f = use (A.sortWith f)

group :: forall a. Eq a => ArrayView a -> ArrayView (NonEmptyArrayView a)
group = use (A.group :: Array a -> Array (NonEmptyArray a))

group' :: forall a. Ord a => ArrayView a -> ArrayView (NonEmptyArrayView a)
group' = use (A.group' :: Array a -> Array (NonEmptyArray a))

groupBy :: forall a. (a -> a -> Boolean) -> ArrayView a -> ArrayView (NonEmptyArrayView a)
groupBy f = use (A.groupBy f)

nub :: forall a. Ord a => ArrayView a -> ArrayView a
nub = use (A.nub :: Array a -> Array a)

nubEq :: forall a. Eq a => ArrayView a -> ArrayView a
nubEq = use (A.nubEq :: Array a -> Array a)

nubBy :: forall a. (a -> a -> Ordering) -> ArrayView a -> ArrayView a
nubBy f = use (A.nubBy f)

nubByEq :: forall a. (a -> a -> Boolean) -> ArrayView a -> ArrayView a
nubByEq p = use (A.nubByEq p)

union :: forall a. Eq a => ArrayView a -> ArrayView a -> ArrayView a
union = use (A.union :: Array a -> Array a -> Array a)

unionBy :: forall a. (a -> a -> Boolean) -> ArrayView a -> ArrayView a -> ArrayView a
unionBy p = use (A.unionBy p)

delete :: forall a. Eq a => a -> ArrayView a -> ArrayView a
delete a = use (A.delete a)

deleteBy :: forall a. (a -> a -> Boolean) -> a -> ArrayView a -> ArrayView a
deleteBy f = use (A.deleteBy f)

difference :: forall a. Eq a => ArrayView a -> ArrayView a -> ArrayView a
difference = use (A.difference :: Array a -> Array a -> Array a)

infix 5 difference as \\

intersect :: forall a. Eq a => ArrayView a -> ArrayView a -> ArrayView a
intersect = use (A.intersect :: Array a -> Array a -> Array a)

intersectBy :: forall a. (a -> a -> Boolean) -> ArrayView a -> ArrayView a -> ArrayView a
intersectBy f = use (A.intersectBy f)

zipWith :: forall a b c. (a -> b -> c) -> ArrayView a -> ArrayView b -> ArrayView c
zipWith f = use (A.zipWith f)

zipWithA :: forall m a b c. Applicative m => (a -> b -> m c) -> ArrayView a -> ArrayView b -> m (ArrayView c)
zipWithA f = use (A.zipWithA f)

zip :: forall a b. ArrayView a -> ArrayView b -> ArrayView (Tuple a b)
zip = use (A.zip :: Array a -> Array b -> Array (Tuple a b))

unzip :: forall a b. ArrayView (Tuple a b) -> Tuple (ArrayView a) (ArrayView b)
unzip = use (A.unzip :: Array (Tuple a b) -> Tuple (Array a) (Array b))

foldM :: forall m a b. Monad m => (a -> b -> m a) -> a -> ArrayView b -> m a
foldM f = use (A.foldM f)

foldRecM :: forall m a b. MonadRec m => (a -> b -> m a) -> a -> ArrayView b -> m a
foldRecM f = use (A.foldRecM f)

-- * internal

whenNonEmpty :: forall a b. (ArrayView a -> b) -> ArrayView a -> Maybe b
whenNonEmpty _ (View { len: 0 }) = Nothing
whenNonEmpty f av           = Just (f av)

toNonNegative :: Int -> Int
toNonNegative n = if n > 0 then n else 0
