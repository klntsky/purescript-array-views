module Data.ArrayView.NonEmpty
  ( module Exports
  , fromArrayView
  , fromNonEmpty
  , toNonEmpty
  , fromFoldable
  , fromFoldable1
  , toUnfoldable
  , toUnfoldable1
  , singleton
  , (..), range

  , replicate
--   , some

  , length

  , (:), cons
  , cons'
  , snoc
  , snoc'
  , appendArrayView
  , insert
  , insertBy

  , head
  , last
  , tail
  , init
  , uncons
  , unsnoc

  , (!!), index
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

  , nub
  , nubBy
  , nubEq
  , nubByEq
  , union
  , union'
  , unionBy
  , unionBy'
  , delete
  , deleteBy

  , (\\), difference
  , difference'
  , intersect
  , intersect'
  , intersectBy
  , intersectBy'

  , zipWith
  , zipWithA
  , zip
  , unzip

  , foldM
  , foldRecM

  , unsafeIndex

  , force
  ) where

import Control.Monad.Rec.Class (class MonadRec)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NE
import Data.ArrayView (use)
import Data.ArrayView as AV
import Data.ArrayView.Internal (ArrayView(..), NonEmptyArrayView(..), fromArray)
import Data.ArrayView.Internal (NonEmptyArrayView, toArrayView) as Exports
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Semigroup.Foldable (class Foldable1)
import Data.Tuple (Tuple)
import Data.Unfoldable (class Unfoldable)
import Data.Unfoldable1 (class Unfoldable1)
import Prelude (class Applicative, class Eq, class Monad, class Ord, Ordering, bind, eq, flip, map, mempty, otherwise, pure, (+), (-), (<>), (==), (>>>), (<))


-- | *O(1)*
fromArrayView :: forall a. ArrayView a -> Maybe (NonEmptyArrayView a)
fromArrayView av = case AV.uncons av of
  Just { head, tail } -> Just (wrap (head :| tail))
  Nothing -> Nothing

fromNonEmpty :: forall a. NonEmpty ArrayView a -> NonEmptyArrayView a
fromNonEmpty = wrap

toNonEmpty :: forall a. NonEmptyArrayView a -> NonEmpty ArrayView a
toNonEmpty = unwrap

fromFoldable :: forall f a. Foldable f => f a -> Maybe (NonEmptyArrayView a)
fromFoldable = use (NE.fromFoldable :: f a -> Maybe (NonEmptyArray a))

fromFoldable1 :: forall f a. Foldable1 f => f a -> NonEmptyArrayView a
fromFoldable1 = use (NE.fromFoldable1 :: f a -> NonEmptyArray a)

toUnfoldable :: forall f a. Unfoldable f => NonEmptyArrayView a -> f a
toUnfoldable = use (NE.toUnfoldable :: NonEmptyArray a -> f a)

toUnfoldable1 :: forall f a. Unfoldable1 f => NonEmptyArrayView a -> f a
toUnfoldable1 = use (NE.toUnfoldable1 :: NonEmptyArray a -> f a)

singleton :: forall a. a -> NonEmptyArrayView a
singleton = pure

range :: Int -> Int -> NonEmptyArrayView Int
range = use NE.range

infix 8 range as ..

replicate :: forall a. Int -> a -> NonEmptyArrayView a
replicate = use (NE.replicate :: Int -> a -> NonEmptyArray a)

-- some
--   :: forall f a
--    . Alternative f
--   => Lazy (f (ArrayView a))
--   => f a -> f (NonEmptyArrayView a)


length :: forall a. NonEmptyArrayView a -> Int
length (NonEmptyArrayView (a :| View { len })) = len + 1

cons :: forall a. a -> NonEmptyArrayView a -> NonEmptyArrayView a
cons a = use (NE.cons a)

infixr 6 cons as :

cons' :: forall a. a -> ArrayView a -> NonEmptyArrayView a
cons' a = use (NE.cons' a)

snoc :: forall a. NonEmptyArrayView a -> a -> NonEmptyArrayView a
snoc = use (NE.snoc :: NonEmptyArray a -> a -> NonEmptyArray a)

snoc' :: forall a. ArrayView a -> a -> NonEmptyArrayView a
snoc' = use (NE.snoc' :: Array a -> a -> NonEmptyArray a)

appendArrayView :: forall a. NonEmptyArrayView a -> ArrayView a -> NonEmptyArrayView a
appendArrayView = use (NE.appendArray :: NonEmptyArray a -> Array a -> NonEmptyArray a)

insert :: forall a. Ord a => a -> NonEmptyArrayView a -> NonEmptyArrayView a
insert a = use (NE.insert a)

insertBy :: forall a. (a -> a -> Ordering) -> a -> NonEmptyArrayView a -> NonEmptyArrayView a
insertBy f = use (NE.insertBy f)

head :: forall a. NonEmptyArrayView a -> a
head (NonEmptyArrayView (a :| _)) = a

last :: forall a. NonEmptyArrayView a -> a
last (NonEmptyArrayView (a :| as)) =
  fromMaybe a (AV.last as)

tail :: forall a. NonEmptyArrayView a -> ArrayView a
tail (NonEmptyArrayView (_ :| as)) = as

init :: forall a. NonEmptyArrayView a -> ArrayView a
init = use (NE.init :: NonEmptyArray a -> Array a)

uncons :: forall a. NonEmptyArrayView a -> { head :: a, tail :: ArrayView a }
uncons (NonEmptyArrayView (head :| tail)) = { head, tail }

unsnoc :: forall a. NonEmptyArrayView a -> { init :: ArrayView a, last :: a }
unsnoc (NonEmptyArrayView (head :| tail)) = case AV.unsnoc tail of
  Just { init, last } -> { init: AV.singleton head <> init, last }
  Nothing -> { init: mempty, last: head }

index :: forall a. NonEmptyArrayView a -> Int -> Maybe a
index (NonEmptyArrayView (a :| _ )) 0 = Just a
index (NonEmptyArrayView (_ :| as)) n = AV.index as (n - 1)

infixl 8 index as !!

elemIndex :: forall a. Eq a => a -> NonEmptyArrayView a -> Maybe Int
elemIndex a = findIndex (_ == a)

elemLastIndex :: forall a. Eq a => a -> NonEmptyArrayView a -> Maybe Int
elemLastIndex a = findLastIndex (_ == a)

findIndex :: forall a. (a -> Boolean) -> NonEmptyArrayView a -> Maybe Int
findIndex p (NonEmptyArrayView (a :| as))
  | p a = Just 0
  | otherwise = map (_ + 1) (AV.findIndex p as)

findLastIndex :: forall a. (a -> Boolean) -> NonEmptyArrayView a -> Maybe Int
findLastIndex f (NonEmptyArrayView (a :| as)) =
  case AV.findLastIndex f as of
    Nothing
      | f a -> Just 0
      | otherwise -> Nothing
    Just n -> Just (n + 1)

insertAt :: forall a. Int -> a -> NonEmptyArrayView a -> Maybe (NonEmptyArrayView a)
insertAt =
  use (NE.insertAt :: Int -> a -> NonEmptyArray a -> Maybe (NonEmptyArray a))

deleteAt :: forall a. Int -> NonEmptyArrayView a -> Maybe (ArrayView a)
deleteAt =
  use (NE.deleteAt :: Int -> NonEmptyArray a -> Maybe (Array a))

updateAt :: forall a. Int -> a -> NonEmptyArrayView a -> Maybe (NonEmptyArrayView a)
updateAt =
  use (NE.updateAt :: Int -> a -> NonEmptyArray a -> Maybe (NonEmptyArray a))

updateAtIndices :: forall t a. Foldable t => t (Tuple Int a) -> NonEmptyArrayView a -> NonEmptyArrayView a
updateAtIndices t =
  use (NE.updateAtIndices t)

modifyAt :: forall a. Int -> (a -> a) -> NonEmptyArrayView a -> Maybe (NonEmptyArrayView a)
modifyAt i f =
  use (NE.modifyAt i f)

modifyAtIndices :: forall t a. Foldable t => t Int -> (a -> a) -> NonEmptyArrayView a -> NonEmptyArrayView a
modifyAtIndices ixs f =
  use (NE.modifyAtIndices ixs f)

alterAt :: forall a. Int -> (a -> Maybe a) -> NonEmptyArrayView a -> Maybe (ArrayView a)
alterAt i f =
  use (NE.alterAt i f)

reverse :: forall a. NonEmptyArrayView a -> NonEmptyArrayView a
reverse =
  use (NE.reverse :: NonEmptyArray a -> NonEmptyArray a)

concat :: forall a. NonEmptyArrayView (NonEmptyArrayView a) -> NonEmptyArrayView a
concat =
  use (NE.concat :: NonEmptyArray (NonEmptyArray a) -> NonEmptyArray a)

concatMap :: forall a b. (a -> NonEmptyArrayView b) -> NonEmptyArrayView a -> NonEmptyArrayView b
concatMap = flip bind

filter :: forall a. (a -> Boolean) -> NonEmptyArrayView a -> ArrayView a
filter f = use (NE.filter f)

partition
  :: forall a
   . (a -> Boolean)
  -> NonEmptyArrayView a
  -> { yes :: ArrayView a, no :: ArrayView a}
partition f = use (NE.partition f) >>> fix
  where
    fix :: { no :: Array a, yes :: Array a } -> { yes :: ArrayView a, no :: ArrayView a }
    fix { yes, no } = { yes: fromArray yes, no: fromArray no }

filterA
  :: forall a f
   . Applicative f
  => (a -> f Boolean)
  -> NonEmptyArrayView a
  -> f (ArrayView a)
filterA f = use (NE.filterA f)

mapMaybe :: forall a b. (a -> Maybe b) -> NonEmptyArrayView a -> ArrayView b
mapMaybe f = use (NE.mapMaybe f)

catMaybes :: forall a. NonEmptyArrayView (Maybe a) -> ArrayView a
catMaybes = use (NE.catMaybes :: NonEmptyArray (Maybe a) -> Array a)

sort :: forall a. Ord a => NonEmptyArrayView a -> NonEmptyArrayView a
sort = use (NE.sort :: NonEmptyArray a -> NonEmptyArray a)

sortBy :: forall a. (a -> a -> Ordering) -> NonEmptyArrayView a -> NonEmptyArrayView a
sortBy f = use (NE.sortBy f)

sortWith :: forall a b. Ord b => (a -> b) -> NonEmptyArrayView a -> NonEmptyArrayView a
sortWith f = use (NE.sortWith f)

slice :: forall a. Int -> Int -> NonEmptyArrayView a -> ArrayView a
slice = use (NE.slice :: Int -> Int -> NonEmptyArray a -> Array a)

take :: forall a. Int -> NonEmptyArrayView a -> ArrayView a
take = use (NE.take :: Int -> NonEmptyArray a -> Array a)

takeEnd :: forall a. Int -> NonEmptyArrayView a -> ArrayView a
takeEnd n neav = drop (length neav - n) neav

takeWhile :: forall a. (a -> Boolean) -> NonEmptyArrayView a -> ArrayView a
takeWhile p neav = (span p neav).init

drop :: forall a. Int -> NonEmptyArrayView a -> ArrayView a
drop n neav = slice (if n < 0 then 0 else n) (length neav) neav

dropEnd :: forall a. Int -> NonEmptyArrayView a -> ArrayView a
dropEnd n neav = take (length neav - n) neav

dropWhile :: forall a. (a -> Boolean) -> NonEmptyArrayView a -> ArrayView a
dropWhile p neav = (span p neav).rest

span
  :: forall a
   . (a -> Boolean)
  -> NonEmptyArrayView a
  -> { init :: ArrayView a, rest :: ArrayView a }
span p (NonEmptyArrayView (a :| as))
  | p a = let tmp = AV.span p as in
    tmp { init = pure a <> tmp.init }
  | otherwise = { init: mempty, rest: AV.cons a as }

nub :: forall a. Ord a => NonEmptyArrayView a -> NonEmptyArrayView a
nub = use (NE.nub :: NonEmptyArray a -> NonEmptyArray a)

nubEq :: forall a. Eq a => NonEmptyArrayView a -> NonEmptyArrayView a
nubEq = use (NE.nubEq :: NonEmptyArray a -> NonEmptyArray a)

nubBy :: forall a. (a -> a -> Ordering) -> NonEmptyArrayView a -> NonEmptyArrayView a
nubBy f = use (NE.nubBy f)

nubByEq :: forall a. (a -> a -> Boolean) -> NonEmptyArrayView a -> NonEmptyArrayView a
nubByEq f = use (NE.nubByEq f)

union :: forall a. Eq a => NonEmptyArrayView a -> NonEmptyArrayView a -> NonEmptyArrayView a
union = unionBy (==)

union' :: forall a. Eq a => NonEmptyArrayView a -> ArrayView a -> NonEmptyArrayView a
union' = unionBy' (==)

unionBy
  :: forall a
   . (a -> a -> Boolean)
  -> NonEmptyArrayView a
  -> NonEmptyArrayView a
  -> NonEmptyArrayView a
unionBy p = use (NE.unionBy p)

unionBy'
  :: forall a
   . (a -> a -> Boolean)
  -> NonEmptyArrayView a
  -> ArrayView a
  -> NonEmptyArrayView a
unionBy' p = use (NE.unionBy' p)

delete :: forall a. Eq a => a -> NonEmptyArrayView a -> ArrayView a
delete a = use (NE.delete a)

deleteBy :: forall a. (a -> a -> Boolean) -> a -> NonEmptyArrayView a -> ArrayView a
deleteBy p = use (NE.deleteBy p)

difference :: forall a. Eq a => NonEmptyArrayView a -> NonEmptyArrayView a -> ArrayView a
difference = use (NE.difference :: NonEmptyArray a -> NonEmptyArray a -> Array a)

difference' :: forall a. Eq a => NonEmptyArrayView a -> ArrayView a -> ArrayView a
difference' = use (NE.difference' :: NonEmptyArray a -> Array a -> Array a)

intersect :: forall a . Eq a => NonEmptyArrayView a -> NonEmptyArrayView a -> ArrayView a
intersect = intersectBy eq

intersect' :: forall a . Eq a => NonEmptyArrayView a -> ArrayView a -> ArrayView a
intersect' = intersectBy' eq

intersectBy
  :: forall a
   . (a -> a -> Boolean)
  -> NonEmptyArrayView a
  -> NonEmptyArrayView a
  -> ArrayView a
intersectBy p = use (NE.intersectBy p)

intersectBy'
  :: forall a
   . (a -> a -> Boolean)
  -> NonEmptyArrayView a
  -> ArrayView a
  -> ArrayView a
intersectBy' p = use (NE.intersectBy' p)

infix 5 difference as \\

zipWith
  :: forall a b c
   . (a -> b -> c)
  -> NonEmptyArrayView a
  -> NonEmptyArrayView b
  -> NonEmptyArrayView c
zipWith f = use (NE.zipWith f)


zipWithA
  :: forall m a b c
   . Applicative m
  => (a -> b -> m c)
  -> NonEmptyArrayView a
  -> NonEmptyArrayView b
  -> m (NonEmptyArrayView c)
zipWithA f = use (NE.zipWithA f)

zip :: forall a b. NonEmptyArrayView a -> NonEmptyArrayView b -> NonEmptyArrayView (Tuple a b)
zip = use (NE.zip :: NonEmptyArray a -> NonEmptyArray b -> NonEmptyArray (Tuple a b))

unzip :: forall a b. NonEmptyArrayView (Tuple a b) -> Tuple (NonEmptyArrayView a) (NonEmptyArrayView b)
unzip = use (NE.unzip :: NonEmptyArray (Tuple a b) -> Tuple (NonEmptyArray a) (NonEmptyArray b))

foldM :: forall m a b. Monad m => (a -> b -> m a) -> a -> NonEmptyArrayView b -> m a
foldM f = use (NE.foldM f)

foldRecM :: forall m a b. MonadRec m => (a -> b -> m a) -> a -> NonEmptyArrayView b -> m a
foldRecM f = use (NE.foldRecM f)

unsafeIndex :: forall a. Partial => NonEmptyArrayView a -> Int -> a
unsafeIndex = use (NE.unsafeIndex :: Partial => NonEmptyArray a -> Int -> a)

-- | Perform deferred `slice`. This function allows the garbage collector to
-- | free unused parts of the array referenced by given `NonEmptyArrayView`.
-- |
-- | *O(n)*
force :: forall a. NonEmptyArrayView a -> NonEmptyArrayView a
force (NonEmptyArrayView (a :| as)) = NonEmptyArrayView (a :| AV.force as)
