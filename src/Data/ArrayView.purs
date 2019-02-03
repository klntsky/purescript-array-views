module Data.ArrayView
       ( ArrayView
       , fromArray
       , toArray

       , fromFoldable
       , toUnfoldable
       , singleton
       , range, (..)
       , replicate
       , some
       , many
       , null
       , length
       , cons, (:)
       , snoc
       , insert
       , insertBy
       , head
       , last
       , tail
       , init
       , uncons
       , unsnoc
       , index, (!!)
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
       , concatMap
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
       , difference
       , intersect
       , intersectBy
       , zipWith
       , zipWithA
       , zip
       , unzip
       , foldM
       , foldRecM
       , unsafeIndex
       )
where


import Control.Alternative (class Alternative)
import Control.Lazy (class Lazy)
import Control.Monad.Rec.Class (class MonadRec)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Traversable (class Foldable, class Traversable, foldMap, foldl, foldr, sequenceDefault, traverse)
import Data.Tuple (Tuple)
import Data.Unfoldable (class Unfoldable, unfoldr)
import Data.Unfoldable1 (class Unfoldable1, unfoldr1)
import Prelude (class Applicative, class Apply, class Bind, class Eq, class Functor, class Monad, class Monoid, class Ord, class Semigroup, class Show, type (~>), Ordering, apply, show, join, map, otherwise, (&&), (+), (-), (<), (>), (<#>), (<<<), (<=), (<>), (==), (>=), (>>=), (>>>), (||))
import Data.Array as A
import Data.Array.NonEmpty as NE
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong ((***))

data ArrayView a = View Int Int (Array a)

derive instance genericArrayView :: Generic (ArrayView a) _

instance showArrayView :: Show a => Show (ArrayView a) where
  show av  = "fromArray " <> show (toArray av)

derive instance eqArrayView :: Eq a => Eq (ArrayView a)

instance functorArrayView :: Functor ArrayView where
  map f = toArray >>> map f >>> fromArray

instance applyArrayView :: Apply ArrayView where
  apply f arr = fromArray (apply (toArray f) (toArray arr))

instance bindArrayView :: Bind ArrayView where
  bind m f = fromArray ((toArray m) >>= (f >>> toArray))

instance applicativeArrayView :: Applicative ArrayView where
  pure = singleton

instance monadArrayView :: Monad ArrayView

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
  append a b = fromArray (toArray a <> toArray b)

instance monoidArrayView :: Monoid (ArrayView a) where
  mempty = View 0 0 []

fromFoldable :: forall f. Foldable f => f ~> ArrayView
fromFoldable = A.fromFoldable >>> fromArray

toUnfoldable :: forall f. Unfoldable f => ArrayView ~> f
toUnfoldable = toArray >>> A.toUnfoldable

singleton :: forall a. a -> ArrayView a
singleton a = View 0 1 [a]

range :: Int -> Int -> ArrayView Int
range f = fromArray <<< A.range f

infix 8 range as ..

replicate :: forall a. Int -> a -> ArrayView a
replicate i = fromArray <<< A.replicate i

-- Using `Lazy (f (ArrayView a))` constraint is impossible due to `OrphanInstances`.
some :: forall f a. Alternative f => Lazy (f (Array a)) => f a -> f (ArrayView a)
some = map fromArray <<< A.some

-- Using `Lazy (f (ArrayView a))` constraint is impossible due to `OrphanInstances`.
many :: forall f a. Alternative f => Lazy (f (Array a)) => f a -> f (ArrayView a)
many = map fromArray <<< A.many

null :: forall a. ArrayView a -> Boolean
null (View _ 0 _) = true
null _            = false

length :: forall a. ArrayView a -> Int
length (View _ len _) = len

-- | O(n)
cons :: forall a. a -> ArrayView a -> ArrayView a
cons a av = fromArray (A.cons a (toArray av))

infix 6 cons as :

-- | O(n)
snoc :: forall a. ArrayView a -> a -> ArrayView a
snoc av a = fromArray (A.snoc (toArray av) a)

insert :: forall a. Ord a => a -> ArrayView a -> ArrayView a
insert a = toArray >>> A.insert a >>> fromArray

insertBy :: forall a. (a -> a -> Ordering) -> a -> ArrayView a -> ArrayView a
insertBy f a = toArray >>> A.insertBy f a >>> fromArray

head :: forall a. ArrayView a -> Maybe a
head (View _    0 _)   = Nothing
head (View from _ arr) = arr A.!! from

last :: forall a. ArrayView a -> Maybe a
last (View _    0   _)   = Nothing
last (View from len arr) = arr A.!! (from + len - 1)

tail :: forall a. ArrayView a -> Maybe (ArrayView a)
tail (View _ 0 _) = Nothing
tail av           = Just (unsafeTail av)

init :: forall a. ArrayView a -> Maybe (ArrayView a)
init = justNonEmpty unsafeInit

uncons :: forall a. ArrayView a -> Maybe { head :: a, tail :: ArrayView a }
uncons =
  join <<< justNonEmpty
  (\av @ (View from len arr) ->
    arr A.!! from <#> \head -> { head, tail: unsafeTail av })

unsnoc :: forall a. ArrayView a -> Maybe { init :: ArrayView a, last :: a }
unsnoc =
  join <<< justNonEmpty
  (\av @ (View from len arr) ->
    arr A.!! (from + len) <#> \last -> { init: unsafeInit av, last })

index :: forall a. ArrayView a -> Int -> Maybe a
index av @ (View from len arr) ix
  | ix >= len = Nothing
  | otherwise = arr A.!! (from + ix)

infixl 8 index as !!

elemIndex :: forall a. Eq a => a -> ArrayView a -> Maybe Int
elemIndex e = toArray >>> A.elemIndex e

elemLastIndex :: forall a. Eq a => a -> ArrayView a -> Maybe Int
elemLastIndex e = toArray >>> A.elemLastIndex e

findIndex :: forall a. (a -> Boolean) -> ArrayView a -> Maybe Int
findIndex p = toArray >>> A.findIndex p

findLastIndex :: forall a. (a -> Boolean) -> ArrayView a -> Maybe Int
findLastIndex p = toArray >>> A.findLastIndex p

insertAt :: forall a. Int -> a -> ArrayView a -> Maybe (ArrayView a)
insertAt ix e = toArray >>> A.insertAt ix e >>> map fromArray

deleteAt :: forall a. Int -> ArrayView a -> Maybe (ArrayView a)
deleteAt = useAndMap A.deleteAt

updateAt :: forall a. Int -> a -> ArrayView a -> Maybe (ArrayView a)
updateAt ix = useAndMap (A.updateAt ix)

updateAtIndices :: forall t a. Foldable t => t (Tuple Int a) -> ArrayView a -> ArrayView a
updateAtIndices = use A.updateAtIndices

modifyAt :: forall a. Int -> (a -> a) -> ArrayView a -> Maybe (ArrayView a)
modifyAt ix = useAndMap (A.modifyAt ix)

modifyAtIndices :: forall t a. Foldable t => t Int -> (a -> a) -> ArrayView a -> ArrayView a
modifyAtIndices ixs f = via (A.modifyAtIndices ixs f)

alterAt :: forall a. Int -> (a -> Maybe a) -> ArrayView a -> Maybe (ArrayView a)
alterAt ix = useAndMap (A.alterAt ix)

reverse :: forall a. ArrayView a -> ArrayView a
reverse = via A.reverse

concat :: forall a. ArrayView (ArrayView a) -> ArrayView a
concat = toArray >>> map toArray >>> A.concat >>> fromArray

concatMap :: forall a b. (a -> ArrayView b) -> ArrayView a -> ArrayView b
concatMap f = concat <<< map f

filter :: forall a. (a -> Boolean) -> ArrayView a -> ArrayView a
filter = use A.filter

partition :: forall a. (a -> Boolean) -> ArrayView a -> { yes :: ArrayView a, no :: ArrayView a }
partition p = toArray >>> A.partition p >>> go
  where
    go { yes, no } = { yes: fromArray yes, no: fromArray no }

filterA :: forall a f. Applicative f => (a -> f Boolean) -> ArrayView a -> f (ArrayView a)
filterA = useAndMap A.filterA

mapMaybe :: forall a b. (a -> Maybe b) -> ArrayView a -> ArrayView b
mapMaybe = use A.mapMaybe

catMaybes :: forall a. ArrayView (Maybe a) -> ArrayView a
catMaybes = via A.catMaybes

mapWithIndex :: forall a b. (Int -> a -> b) -> ArrayView a -> ArrayView b
mapWithIndex = use A.mapWithIndex

sort :: forall a. Ord a => ArrayView a -> ArrayView a
sort = via A.sort

sortBy :: forall a. (a -> a -> Ordering) -> ArrayView a -> ArrayView a
sortBy f = via (A.sortBy f)

sortWith :: forall a b. Ord b => (a -> b) -> ArrayView a -> ArrayView a
sortWith f = via (A.sortWith f)

-- | O(1)
slice :: forall a. Int -> Int -> ArrayView a -> ArrayView a
slice f' to' (View from len arr) =
  if to <= f || f >= len then View 0 0 [] --  forget about the original array
                                          -- (allow it to be GC'ed)
  else View (from + f) (to - f) arr
  where
    larr = A.length arr
    f  = between 0 larr (fix f')
    to = between 0 larr (fix to')
    between x y n =
      if n < x
      then x
      else if n > y
           then y
           else n
    fix n
      | n < 0 = len + n
      | otherwise = n

-- | O(1)
take :: forall a. Int -> ArrayView a -> ArrayView a
take n = slice 0 n

-- | O(1)
takeEnd :: forall a. Int -> ArrayView a -> ArrayView a
takeEnd n xs = drop (length xs - n) xs

takeWhile :: forall a. (a -> Boolean) -> ArrayView a -> ArrayView a
takeWhile p xs = (span p xs).init

-- | O(1)
drop :: forall a. Int -> ArrayView a -> ArrayView a
drop n av = slice n (length av) av

-- | O(1)
dropEnd :: forall a. Int -> ArrayView a -> ArrayView a
dropEnd n xs = take (length xs - n) xs

dropWhile :: forall a. (a -> Boolean) -> ArrayView a -> ArrayView a
dropWhile p xs = (span p xs).rest

-- | The time complexity of `span` only depends on the size of the resulting
-- | `init` value.
span :: forall a. (a -> Boolean) -> ArrayView a ->
        { init :: ArrayView a, rest :: ArrayView a }
span p av =
  -- `span` implementation from Data/Array.purs is copypasted here instead of
  -- reusing `Data.Array.span` because `slice` on `ArrayView` is O(1), and
  -- we can take advantage of it.
  case go 0 of
    Just 0 ->
      { init: View 0 0 [], rest: av }
    Just i ->
      { init: slice 0 i av, rest: slice i (length av) av }
    Nothing ->
      { init: av, rest: View 0 0 [] }
  where
    go i =
      case index av i of
        Just x -> if p x then go (i + 1) else Just i
        Nothing -> Nothing

group :: forall a. Eq a => ArrayView a -> ArrayView (NonEmpty ArrayView a)
group av = fromArray (A.group (toArray av) <#> fromNonEmpty)

group' :: forall a. Ord a => ArrayView a -> ArrayView (NonEmpty ArrayView a)
group' av = fromArray (A.group' (toArray av) <#> fromNonEmpty)

fromNonEmpty :: NE.NonEmptyArray ~> NonEmpty ArrayView
fromNonEmpty narr = let arr' = NE.uncons narr in
  arr'.head :| fromArray (arr'.tail)

groupBy :: forall a. (a -> a -> Boolean) -> ArrayView a -> ArrayView (NonEmpty ArrayView a)
groupBy f = map fromNonEmpty <<< via (A.groupBy f)

nub :: forall a. Ord a => ArrayView a -> ArrayView a
nub = via A.nub

nubEq :: forall a. Eq a => ArrayView a -> ArrayView a
nubEq = via A.nubEq

nubBy :: forall a. (a -> a -> Ordering) -> ArrayView a -> ArrayView a
nubBy = use A.nubBy

nubByEq :: forall a. (a -> a -> Boolean) -> ArrayView a -> ArrayView a
nubByEq = use A.nubByEq

union :: forall a. Eq a => ArrayView a -> ArrayView a -> ArrayView a
union = via2 A.union

unionBy :: forall a. (a -> a -> Boolean) -> ArrayView a -> ArrayView a -> ArrayView a
unionBy = via2 <<< A.unionBy

delete :: forall a. Eq a => a -> ArrayView a -> ArrayView a
delete = use A.delete

deleteBy :: forall a. (a -> a -> Boolean) -> a -> ArrayView a -> ArrayView a
deleteBy f = use (A.deleteBy f)

difference :: forall a. Eq a => ArrayView a -> ArrayView a -> ArrayView a
difference = via2 A.difference

intersect :: forall a. Eq a => ArrayView a -> ArrayView a -> ArrayView a
intersect = via2 A.intersect

intersectBy :: forall a. (a -> a -> Boolean) -> ArrayView a -> ArrayView a -> ArrayView a
intersectBy f = via2 (A.intersectBy f)

zipWith :: forall a b c. (a -> b -> c) -> ArrayView a -> ArrayView b -> ArrayView c
zipWith f = via2 (A.zipWith f)

zipWithA :: forall m a b c. Applicative m => (a -> b -> m c) -> ArrayView a -> ArrayView b -> m (ArrayView c)
zipWithA f a b = A.zipWithA f (toArray a) (toArray b) <#> fromArray

zip :: forall a b. ArrayView a -> ArrayView b -> ArrayView (Tuple a b)
zip = via2 A.zip

unzip :: forall a b. ArrayView (Tuple a b) -> Tuple (ArrayView a) (ArrayView b)
unzip = (fromArray *** fromArray) <<< A.unzip <<< toArray

foldM :: forall m a b. Monad m => (a -> b -> m a) -> a -> ArrayView b -> m a
foldM f a = A.foldM f a <<< toArray

foldRecM :: forall m a b. MonadRec m => (a -> b -> m a) -> a -> ArrayView b -> m a
foldRecM f a = A.foldRecM f a <<< toArray

unsafeIndex :: forall a. Partial => ArrayView a -> Int -> a
unsafeIndex = A.unsafeIndex <<< toArray

fromArray :: Array ~> ArrayView
fromArray arr = let len = A.length arr in
  View 0 len arr

toArray :: ArrayView ~> Array
toArray (View from len arr)
  | from == 0 && A.length arr == len =
    arr
  | otherwise =
    A.slice from (from + len) arr

-- internal

justNonEmpty :: forall a b. (ArrayView a -> b) -> ArrayView a -> Maybe b
justNonEmpty _ (View _ 0 _) = Nothing
justNonEmpty f av           = Just (f av)

via2 :: forall a b c. (Array a -> Array b -> Array c) -> ArrayView a -> ArrayView b -> ArrayView c
via2 f x y = fromArray (f (toArray x) (toArray y))

via :: forall a b. (Array a -> Array b) -> ArrayView a -> ArrayView b
via f = toArray >>> f >>> fromArray

use :: forall a b c. (a -> Array c -> Array b) -> a -> ArrayView c -> ArrayView b
use f = via <<< f

useAndMap :: forall a b c f. Functor f => (a -> Array c -> f (Array b)) -> a -> ArrayView c -> f (ArrayView b)
useAndMap f a av = f a (toArray av) <#> fromArray

unsafeInit :: forall a. ArrayView a -> ArrayView a
unsafeInit (View from len arr) = View from (len - 1) arr

unsafeTail :: forall a. ArrayView a -> ArrayView a
unsafeTail (View from len arr) = View (from + 1) (len - 1) arr
