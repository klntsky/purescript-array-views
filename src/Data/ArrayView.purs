module Data.ArrayView
       ( ArrayView
       , fromArray
       , toArray
       , singleton
       , range, (..)
       , replicate
       , null
       , length
       , cons, (:)
       , snoc
       , head
       , last
       , tail
       , init
       , uncons
       , unsnoc
       , index, (!!)
       , elemIndex
       )
where

import Data.Array as A
import Prelude (class Eq, class Show, type (~>), join, otherwise, (+), (-), (<#>), (<<<), (>=))
import Data.Maybe (Maybe (..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)


data ArrayView a = View Int Int (Array a)


derive instance genericArrayView :: Generic (ArrayView a) _

instance showArrayView :: Show a => Show (ArrayView a) where
  show = genericShow


fromArray :: Array ~> ArrayView
fromArray arr = let len = A.length arr in
  View 0 len arr

toArray :: ArrayView ~> Array
toArray (View from len arr) = A.slice from len arr

singleton :: forall a. a -> ArrayView a
singleton a = View 0 1 [a]

range :: Int -> Int -> ArrayView Int
range f = fromArray <<< A.range f

infix 8 range as ..

replicate :: forall a. Int -> a -> ArrayView a
replicate i = fromArray <<< A.replicate i

null :: forall a. ArrayView a -> Boolean
null (View _ 0 _) = true
null _            = false

length :: forall a. ArrayView a -> Int
length (View _ len _) = len

cons :: forall a. a -> ArrayView a -> ArrayView a
cons a av = fromArray (A.cons a (toArray av))

infix 6 cons as :

snoc :: forall a. ArrayView a -> a -> ArrayView a
snoc av a = fromArray (A.snoc (toArray av) a)

head :: forall a. ArrayView a -> Maybe a
head (View _    0 _)   = Nothing
head (View from _ arr) = arr A.!! from

last :: forall a. ArrayView a -> Maybe a
last (View _    0   _)   = Nothing
last (View from len arr) = arr A.!! (from + len)

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
elemIndex e av = A.elemIndex e (toArray av)

-- internal

justNonEmpty :: forall a b. (ArrayView a -> b) -> ArrayView a -> Maybe b
justNonEmpty _ (View _ 0 _) = Nothing
justNonEmpty f av           = Just (f av)

unsafeTail :: forall a. ArrayView a -> ArrayView a
unsafeTail (View from len arr) = View (from + 1) (len - 1) arr

unsafeInit :: forall a. ArrayView a -> ArrayView a
unsafeInit (View from len arr) = View from (len - 1) arr
