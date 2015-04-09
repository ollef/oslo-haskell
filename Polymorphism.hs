-- | Let's redefine some of the standard typeclasses and make instances for
-- practice!
--
-- Exercise: Replace the 'undefined's with actual definitions.
module Polymorphism where

-- | Hide (by selectively importing) some stuff from the standard prelude so we
-- can redefine them.
--
-- This is non-standard and just to be able to redefine stuff on our own.
import Prelude
  (Eq((==)), Ord((<=)), Show, Bool, Int, Integer, Maybe(Just, Nothing),
  Num((+), (-)), (++), (&&), fromInteger, undefined, (.))

-------------------------------------------------------------------------------
-- * Ordinary *-kinded polymorphism

{- You've seen some generic algebraic datatypes before, but let's recap:

The standard library list is defined as:

> data [a] = [] | a : [a]

The type [a] is sugar for [] a, and at the term level, [] and (:) are
constructors, the latter being 'infix'.

Without special syntactic sugar we might define

data List a = Nil | Cons a (List a)
-}

-- | From the talk, a function that doesn't care about the elements of the list
length :: [a] -> Int
length []     = 0
length (_:xs) = 1 + length xs

-- | This is a binary tree with data in the nodes similar to a previous exercise
data Tree a = Leaf | Fork (Tree a) a (Tree a)
  deriving (Eq, Ord, Show)

-- | A slightly different binary tree with data at the leaves
data Tree' a = Leaf' a | Fork' (Tree' a) (Tree' a)
  deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------
-- * Ad hoc polymorphism (i.e. typeclasses) at kind *

-- | Normally imported from Data.Semigroups (provided the semigroups package is
--   installed)
class Semigroup a where
  (<>) :: a -> a -> a

instance Semigroup [a] where
  (<>) = (++)

-- | Here we define a Monoid as something that is a Semigroup and additionally
-- has an identity element.
--
-- The 'class ...' line can be read as 'A type a that is already a Semigroup is
-- a Monoid provided that it implements the following functions'
--
-- Note that this differs slightly from the Haskell standard library's
-- definition, which does not include Semigroup, but instead defines both
-- mempty and 'mappend' in Monoid. Our version showcases more features of
-- typeclasses and is more general.
class Semigroup a => Monoid a where
  mempty  :: a

instance Monoid [a] where
  mempty  = []

instance Semigroup (Tree a) where
  t <> t' = undefined

prop_tree_append_assoc :: Tree Int -> Tree Int -> Tree Int -> Bool
prop_tree_append_assoc a b c = a <> (b <> c) == (a <> b) <> c

instance Semigroup (Tree' a) where
  t <> t' = undefined

prop_tree'_append_assoc :: Tree' Int -> Tree' Int -> Tree' Int -> Bool
prop_tree'_append_assoc a b c = a <> (b <> c) == (a <> b) <> c

instance Monoid (Tree a) where
  mempty  = undefined

-- EXERCISE: Is (Tree' a) a Monoid? Why (not)?

-- | mempty is the left and right identity
prop_mempty :: Tree Int -> Bool
prop_mempty t = t == mempty <> t
             && t == t <> mempty

-- | Read this as "if b has a Semigroup instance, then (a -> b) has a Semigroup
--   instance, whose definition is ..."
instance Semigroup b => Semigroup (a -> b) where
  -- (<>) :: Semigroup b => (a -> b) -> (a -> b) -> (a -> b)
  f <> g = undefined

instance Monoid b => Monoid (a -> b) where
  -- mempty :: Monoid b => a -> b
  mempty = undefined

{- EXERCISE:

  There are several possible Monoid/Semigroup instances for Maybe.

  One has the following instance head:

  > instance Monoid (Maybe a) where
  >   ...

  Another has this:

  > instance Monoid a => Monoid (Maybe a) where
  >   ...

  What's the difference between the two?

  The standard library uses the latter, so the next exercise is to define it:
-}

instance Semigroup a => Semigroup (Maybe a) where
  (<>) = undefined

instance Monoid a => Monoid (Maybe a) where
  -- | You have two possible choices for mempty. The following is what the
  --   standard library uses. What's the other one?
  mempty = Nothing

-------------------------------------------------------------------------------
-- * Ad hoc polymorphism at kind * -> *

-- | Things that can be "mapped over"
class Functor f where
  fmap :: (a -> b) -> f a -> f b

-- | From the talk
instance Functor [] where
  fmap _ []     = []
  fmap f (x:xs) = f x : fmap f xs

-- | An infix synonym for fmap
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

instance Functor Maybe where
  fmap _ Nothing  = Nothing
  fmap f (Just x) = Just (f x)

instance Functor Tree where
  fmap f t = undefined

instance Functor Tree' where
  fmap f t = undefined

instance Functor ((->) x) where
  -- fmap :: (a -> b) -> (x -> a) -> x -> b
  fmap f g = undefined

-- | This is normally imported from Control.Applicative
class Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

instance Applicative [] where
  pure x        = [x]
  []     <*> _  = []
  (f:fs) <*> xs = fmap f xs <> (fs <*> xs)

-- If you think these instances are tricky, try writing down the type
-- first. Oftentimes there is a very limited set of things that you
-- can do with what you've got.

instance Applicative Tree where
  pure x = undefined
  (<*>)  = ap -- sometimes it's more straightforward to define Monad first and
              -- define the applicative in terms of it, even though Applicative
              -- is a superclass of Monad.

instance Applicative Tree' where
  pure x = undefined
  (<*>)  = ap

instance Applicative ((->) x) where
  -- pure :: a -> x -> a
  pure = undefined
  -- (<*>) :: (x -> a -> b) -> (x -> a) -> (x -> b)
  (<*>) = undefined

-- This last instance may seem a bit strange at first. How is it useful?
-- Consider:
data Config = Config { verbose :: Bool } -- Imagine other configuration options
                                         -- your program might have
-- Now, let's say you're in a situation where you have a couple of functions
-- of type 'Config -> something', and some pure values, i.e. just a 'something':
--
-- Now you might write e.g. this function using only (<*>) and pure, without
-- having to plumb Configs all over the place:
configFun :: (Config -> a)
          -> (Config -> a -> b)
          -> c
          -> (Config -> a -> b -> c -> d)
          -> Config -> d
configFun x y z f = undefined

-- Comparing this to how we write the following function may help:
noConfigFun :: a
            -> (a -> b)
            -> c
            -> (a -> b -> c -> d)
            -> d
noConfigFun x y z f = f x (y x) z

class Applicative m => Monad m where
  return :: a -> m a
  return = pure
  (>>=) :: m a -> (a -> m b) -> m b
  (>>)  :: m a -> m b -> m b
  m >> n = m >>= \_ -> n

ap :: Monad m => m (a -> b) -> m a -> m b
ap = undefined

-- This one's a little bit tricky. Try the one for Tree' first if you want.
instance Monad Tree where
  t >>= f = undefined

instance Monad Tree' where
  t >>= f = undefined

instance Monad ((->) x) where
  f >>= g = undefined
