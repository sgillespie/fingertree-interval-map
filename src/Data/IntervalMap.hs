{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.IntervalMap
  ( -- * Intervals
    Interval (..),
    point,

    -- * Interval maps
    IntervalMap (),
    empty,
    singleton,
    insert,
    union,

    -- * Searching
    intersections,
    dominators,
    search,

    -- * Extraction
    bounds,
    leastView,
    splitAfter,
  ) where

import Data.FingerTree (FingerTree (), ViewL (..), (<|), (><))
import Data.FingerTree qualified as FingerTree
import Data.Foldable (toList)
import GHC.Generics (Generic)

-- | A closed interval.  The lower bound should be less than or equal
-- to the upper bound.
data Interval v = Interval
  { -- | Lower bound of the interval
    low :: v,
    -- | Higher bound of the interval
    high :: v
  }
  deriving (Eq, Ord, Show, Generic)

-- | An interval in which the lower and upper bounds are equal
point :: v -> Interval v
point v = Interval v v

-- | Rightmost interval (including largest lower bound) and largest upper bound
data IntInterval v
  = NoInterval
  | IntInterval (Interval v) v

instance Ord v => Semigroup (IntInterval v) where
  NoInterval <> i = i
  i <> NoInterval = i
  IntInterval _ hi1 <> IntInterval int2 hi2 =
    IntInterval int2 (max hi1 hi2)

instance Ord v => Monoid (IntInterval v) where
  mempty = NoInterval

instance Ord v => FingerTree.Measured (IntInterval v) (Node v a) where
  measure (Node i _) = IntInterval i (high i)

-- | An interval paired with a value
data Node v a = Node (Interval v) a
  deriving (Eq, Ord, Show, Generic)

instance Functor (Node v) where
  fmap f (Node i x) = Node i (f x)

instance Foldable (Node v) where
  foldMap f (Node _ x) = f x

instance Traversable (Node v) where
  traverse f (Node i x) = fmap (Node i) (f x)

-- | Map of closed intervals, possibly with duplicates, ordered lexicographically
-- by interval
newtype IntervalMap v a
  = IntervalMap (FingerTree (IntInterval v) (Node v a))
  deriving (Generic, Show)

instance Functor (IntervalMap v) where
  fmap f (IntervalMap t) = IntervalMap $ FingerTree.unsafeFmap (fmap f) t

instance Foldable (IntervalMap v) where
  foldMap f (IntervalMap t) = foldMap (foldMap f) t
  null (IntervalMap t) = FingerTree.null t

instance Traversable (IntervalMap v) where
  traverse f (IntervalMap t) = IntervalMap <$> FingerTree.unsafeTraverse (traverse f) t

instance (Eq v, Eq a) => Eq (IntervalMap v a) where
  IntervalMap xs == IntervalMap ys = toList xs == toList ys

-- | Lexicographical ordering
instance (Ord v, Ord a) => Ord (IntervalMap v a) where
  compare (IntervalMap xs) (IntervalMap ys) = compare (toList xs) (toList ys)

instance Ord v => Semigroup (IntervalMap v a) where
  (<>) = union

-- | /O(m log (n/\//m))/.  Merge two interval maps.
-- The map may contain duplicate intervals; entries with equal intervals
-- are kept in the original order.
union :: Ord v => IntervalMap v a -> IntervalMap v a -> IntervalMap v a
union (IntervalMap xs) (IntervalMap ys) = IntervalMap (merge1 xs ys)
  where
    merge1 xs' ys' =
      case FingerTree.viewl xs' of
        EmptyL -> ys'
        x :< xs'' ->
          let (l, r) = FingerTree.split (larger x) ys'
          in l >< (x <| merge2 xs'' r)

    merge2 xs' ys' =
      case FingerTree.viewl ys' of
        EmptyL -> xs'
        y :< ys'' ->
          let (l, r) = FingerTree.split (larger' y) xs'
          in l >< (y <| merge1 r ys'')

    larger (Node i _) (IntInterval k _) = k >= i
    larger _ NoInterval = error "larger NoInterval"

    larger' (Node i _) (IntInterval k _) = k > i
    larger' _ NoInterval = error "larger NoInterval"

instance Ord v => Monoid (IntervalMap v a) where
  mempty = empty

empty :: Ord v => IntervalMap v a
empty = IntervalMap FingerTree.empty

singleton :: Ord v => Interval v -> a -> IntervalMap v a
singleton i x = IntervalMap $ FingerTree.singleton (Node i x)

insert :: Ord v => Interval v -> a -> IntervalMap v a -> IntervalMap v a
insert (Interval lo hi) _ m | lo > hi = m
insert i x (IntervalMap t) = IntervalMap $ l >< (Node i x <| r)
  where
    (l, r) = FingerTree.split (larger i) t

    larger i' (IntInterval k _) = k >= i'
    larger _ NoInterval = error "larger NoInterval"

-- | /O(k log (n/\//k))/.  All intervals that intersect with the given
-- interval, in lexicographical order.
intersections :: Ord v => Interval v -> IntervalMap v a -> [(Interval v, a)]
intersections i = inRange (low i) (high i)

-- | /O(k log (n/\//k))/.  All intervals that contain the given interval,
-- in lexicographical order.
dominators :: Ord v => Interval v -> IntervalMap v a -> [(Interval v, a)]
dominators i = inRange (high i) (low i)

-- | /O(k log (n/\//k))/.  All intervals that contain the given point,
-- in lexicographical order.
search :: Ord v => v -> IntervalMap v a -> [(Interval v, a)]
search p = inRange p p

-- | /O(k log (n/\//k))/.  All intervals that intersect with the given
-- interval, in lexicographical order.
inRange :: Ord v => v -> v -> IntervalMap v a -> [(Interval v, a)]
inRange lo hi (IntervalMap t) = matches (FingerTree.takeUntil (greater hi) t)
  where
    matches xs =
      case FingerTree.viewl (FingerTree.dropUntil (atleast lo) xs) of
        EmptyL -> []
        Node i x :< xs' -> (i, x) : matches xs'

atleast :: Ord v => v -> IntInterval v -> Bool
atleast k (IntInterval _ hi) = k <= hi
atleast _ NoInterval = error "atleast NoInterval"

greater :: Ord v => v -> IntInterval v -> Bool
greater k (IntInterval i _) = low i < k
greater _ NoInterval = error "greater NoInterval"

-- | /O(1)/.  @'bounds' m@ returns @'Nothing'@ if @m@ is empty, and
-- otherwise @'Just' i@, where @i@ is the smallest interval containing
-- all the intervals in the map.
bounds :: Ord v => IntervalMap v a -> Maybe (Interval v)
bounds (IntervalMap t) =
  case FingerTree.measure t of
    NoInterval -> Nothing
    IntInterval _ hi ->
      case FingerTree.viewl t of
        EmptyL -> Nothing
        Node (Interval lo _) _ :< _ -> Just (Interval hi lo)

-- | /O(1)/.  @'leastView' m@ returns @'Nothing'@ if @m@ is empty, and
-- otherwise @'Just' ((i, x), m')@, where @i@ is the least interval,
-- @x@ is the associated value, and @m'@ is the rest of the map.
leastView :: Ord v => IntervalMap v a -> Maybe ((Interval v, a), IntervalMap v a)
leastView (IntervalMap t) =
  case FingerTree.viewl t of
    EmptyL -> Nothing
    Node i a :< t' -> Just ((i, a), IntervalMap t')

-- | /O(log(min(i,n-i)))/.  @'splitAfter' k m@ returns a pair of submaps,
-- one consisting of intervals whose lower bound is less than or equal
-- to @k@, and the other of those whose lower bound is greater.
splitAfter :: Ord v => v -> IntervalMap v a -> (IntervalMap v a, IntervalMap v a)
splitAfter k (IntervalMap t) = (IntervalMap before, IntervalMap after)
  where
    (before, after) = FingerTree.split (greater k) t
