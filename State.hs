-- | Now we'll another Monad instance, this time using the type classes defined
-- in the standard library.
--
module StateMonad where

import Control.Applicative
import Control.Monad

-- | This is a very useful type.
-- Think of 'State s a' as a computation that returns an 'a' that additionally
-- has access to a piece of state of type 's' that it may modify by returning a
-- different element of 's'.
newtype State s a = State (s -> (a, s))

-- Now define its instances
instance Functor (State s) where
  fmap = undefined

instance Applicative (State s) where
  pure  = undefined
  (<*>) = ap

instance Monad (State s) where
  return = pure
  (>>=) = undefined

-- We can use this type to write what looks like imperative programs.
-- First define the following functions:

get :: State s s
get = undefined

put :: s -> State s ()
put = undefined

modify :: (s -> s) -> State s ()
modify f = undefined

runState :: State s a -> s -> (a, s)
runState = undefined

-------------------------------------------------------------------------------
-- For instance we might write a little 'language' for describing the movement
-- of a character in a game, and additionally keep track of where it's been so
-- far.

type Pos = (Int, Int) -- ^ A 2D coordinate

data GameState = GameState
  { currentPos :: Pos
  , visited    :: [Pos]
  } deriving Show

type Game a = State GameState a

move :: (Pos -> Pos) -> Game ()
move f = do
  gameState <- get
  let newPos       = f (currentPos gameState)
      newGameState = GameState 
                      { currentPos = newPos
                      , visited    = newPos : visited gameState
                      }
  put newGameState

-- | Rewrite the do-notation above to explicitly use (>>=) and (>>)
move' :: (Pos -> Pos) -> Game ()
move' = undefined

-- | Define the following functions using move above
moveRight :: Game ()
moveRight = undefined

moveLeft :: Game ()
moveLeft = undefined

moveUp :: Game ()
moveUp = undefined

moveDown :: Game ()
moveDown = undefined

-- | Write a program that moves the character in a square with side 'n' in
-- steps of 1, returning to the initial position.
-- Hint: use move{Right,Left,Up,Down} and the following function (if you want):
-- replicateM_ :: Monad m => Int -> m a -> m ()
square :: Int -> Game ()
square n = undefined

-- | Test the square function using this:
test :: ((), GameState)
test = runState (square 4) initialGameState
  where
    initialGameState = GameState (0, 0) []
