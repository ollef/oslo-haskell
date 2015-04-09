{-# LANGUAGE DeriveFunctor #-}
-- | Let's fold and traverse some trees
module FoldAndTraverse where

import Control.Monad.State
import Data.Monoid
import Data.Foldable
import Data.Traversable

data Tree a = Leaf a | Fork (Tree a) (Tree a)
  deriving (Functor, Show)

instance Foldable Tree where
  -- foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f t = undefined

exampleTree, exampleTree2 :: Tree Int
exampleTree = Leaf 1 `Fork` ((Leaf 2 `Fork` Leaf 3) `Fork` Leaf 4)
exampleTree2 = Leaf 0 `Fork` (exampleTree `Fork` Leaf 0)

-- | Define the following function in terms of foldMap. Note that it
-- also exists in the Foldable class as 'toList'.
elements :: Tree a -> [a]
elements = undefined

test :: [Int]
test = elements exampleTree

instance Traversable Tree where
  -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse f t = undefined

{- What can we do with traverse?

A useful way of thinking about it is to think of the function (a -> f b) as an
effectful computation. Some examples:

(a -> Maybe b)   = A computation that might fail
(a -> [b])       = A non-deterministic (= having many possible answers)
                   computation
(a -> State s b) = A stateful computation
(a -> (x -> b))  = A computation with a configuration parameter

Thinking of it this way we can try to read the type of the above instantiations
of traverse:
  traverse :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
    = apply a possibly failing computation to all the leaves in a tree. If one
      of the functions fails, the whole thing fails.
  traverse :: (a -> [b]) -> Tree a -> [Tree b]
    = apply a non-deterministic computation to all the leaves in a tree.
      Return all possible outcomes.
   traverse :: (a -> State s b) -> Tree a -> State s (Tree b)
     = apply a stateful function to all the leaves in a tree.
   traverse :: (a -> (x -> b)) -> Tree a -> x -> Tree b
     = apply a function with a configuration to all the leaves in a tree.
-}

maybeTraversals :: (Maybe (Tree Int), Maybe (Tree Int))
maybeTraversals  = (traverse (safeDiv 100) exampleTree, traverse (safeDiv 100) exampleTree2)
  where
    safeDiv _ 0 = Nothing
    safeDiv x y = Just (x `div` y)

listTraversal :: [Tree Int]
listTraversal = traverse (\n -> [0..n]) exampleTree

stateTraversal :: (Tree Bool, [Int])
stateTraversal = runState (traverse f exampleTree2) []
  where
    -- | Add x to the state if it's not already there. Return True if
    -- it's a duplicate and False if not.
    f :: Int -> State [Int] Bool
    f x = do
      xs <- get
      if x `elem` xs then
        return True
      else do
        put (x:xs)
        return False
