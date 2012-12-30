{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}

-- | This module provides efficient and intuitive ways to build and manipulate random variables of all kinds.
--
--   The following is an example of generating combinations for a slot machine.
--
-- > import Control.Monad.Random
-- > import Control.Applicative
-- > import Control.Monad
-- >
-- > data Slot = Lemon | Cherry | Strawberry | Orange | Bar | Seven deriving (Enum, Show)
-- >
-- > data Combination = Combination Slot Slot Slot deriving Show
-- >
-- > fairSlot = oneOf [Lemon .. Seven]
-- > fairCombination = Combination <$> fairSlot <*> fairSlot <*> fairSlot
-- >
-- > biasedSlot = fromFreqs [Lemon `withFreq` 1, 
-- >                         Cherry `withFreq` 1, 
-- >                         Strawberry `withFreq` 1.2, 
-- >                         Orange `withFreq` 1.1,
-- >                         Bar `withFreq` 0.9,
-- >                         Seven `withFreq` 0.8]
-- >
-- > biasedCombination = Combination <$> biasedSlot <*> biasedSlot <*> biasedSlot
-- >
-- >
-- > aTripToAMachine = do
-- >           combination <- fromFreqs [fairCombination `withFreq` 10, 
-- >                                     biasedCombination `withFreq` 5]
-- >           rounds      <- inRange (5, 50)
-- >           replicateM rounds combination
-- >
-- > aTripToTheCasino = do
-- >           trips <- fmap (*3) $ inRange (1, 10)
-- >           fmap concat $ replicateM trips aTripToAMachine
-- >
-- > main = pick aTripToTheCasino >>= print
module Control.Monad.Random(
	-- * RandPicker class
	RandPicker(..),
	MonadRand,
	-- * Rand Monad
	Rand(..), 
	evalRand, execRand,
	-- * Creation of random variables
	rand, oneOf, inRange,
	fromFreqs, withFreq,
	-- * RandT Monad
	RandT(..),
	evalRandT, execRandT
	) where

import System.Random
import Data.Array.IArray ((!), listArray, Array)
import Control.Applicative
import Control.Monad
import Control.Arrow (first, second)
import Control.Monad.Trans
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Writer.Class
import qualified Data.IntervalMap as IM

-- | Class supporting the return of a random element.
class RandPicker m where
	pick :: Rand a -> m a

type MonadRand m = (Monad m, RandPicker m)

instance RandPicker IO where
	pick r = do
		g <- getStdGen
		let (x, g') = r `runRand` g
		setStdGen g'
		return x



-- | Random variable of @a@.
newtype Rand a = Rand { runRand :: RandomGen g => g -> (a, g) }

instance Monad Rand where
	return x = Rand (\g -> (x, g))
	r >>= f = Rand (\g -> let (x, g') = r `runRand` g in f x `runRand` g')

instance Functor Rand where
	fmap f r = Rand (\g -> let (x, g') = r `runRand` g in (f x, g'))

instance Applicative Rand where
	pure = return
	f <*> x = do
		h <- f
		a <- x
		return (h a)

instance RandPicker Rand where
	pick = id

-- | Run the random variable and returns only its value.
--   The new generator is lost.
evalRand :: RandomGen g => Rand a -> g -> a
evalRand v g = fst $ v `runRand` g 

-- | Run the random variable and returns only the new @RandomGen@.
execRand :: RandomGen g => Rand a -> g -> g
execRand v g = snd $ v `runRand` g



-- | Equiprobable distribution among the elements of the list.
oneOf :: [a] -> Rand a
oneOf xs = fmap (arr !) $ inRange range
	where 
		-- Creating an array for constant time lookup.
		range = (0, length xs - 1)
		arr = (listArray range :: [a] -> Array Int a) xs

-- | Distribution provided by 'random'.
rand :: Random a => Rand a
rand = Rand random

-- | Distribution within a given range, provided by 'randomR'.
inRange :: Random a => (a, a) -> Rand a
inRange r = Rand (randomR r)

-- | Distribution of elements proportionately to their indicated frequency.
fromFreqs :: Real b => [(a, b)] -> Rand a
fromFreqs fs = Rand (\g ->
	let (from, to) = genRange g in
	let range      = toRational (to - from) in
	let ratio      = freqSum / range in
	let (i, g')    = next g in
	let j          = (*) ratio $ toRational (i - from) in
	case IM.containing intervalMap j of
		[(_, x)] -> (x, g')
		_        -> error "Index not in the map."
	)
	where
		elems = preprocess fs

		freqSum = sum $ map snd elems

		intervalMap = IM.fromAscList $ computeIntervals 0 elems

		preprocess = map (second toRational) . filter ((>0) . snd)

		computeIntervals _ [] = undefined
		computeIntervals lower ((v, f):[]) = let upper = (lower + f) in 
			[(IM.ClosedInterval lower upper, v)]
		computeIntervals lower ((v, f):xs) = let upper = (lower + f) in 
			(IM.IntervalCO lower upper, v):computeIntervals upper xs

-- | Alias for @(,)@.
withFreq :: Real b => a -> b -> (a, b)
withFreq = (,)



newtype RandT m a = RandT { runRandT :: RandomGen g => g -> m (a, g) }

instance Functor m => Functor (RandT m) where
	fmap f r = RandT (\g -> fmap (first f) $ r `runRandT` g)

instance Applicative m => Applicative (RandT m) where
	pure x = RandT (\g -> pure (x, g))
	f <*> x = RandT (\g -> let (g', g'') = split g in 
		fmap (\(h, g3') x -> (h x, g3')) (f `runRandT` g') <*> 
		fmap fst (x `runRandT` g''))

instance Monad m => Monad (RandT m) where
	return x = RandT (\g -> return (x, g))
	r >>= f = RandT (runRandT r >=> (\(x, g) -> f x `runRandT` g))
	fail err = RandT (\_ -> fail err)

instance Monad m => RandPicker (RandT m) where
	pick r = RandT (\g -> return $ r `runRand` g)

instance MonadTrans RandT where
	lift m = RandT (\g -> m >>= (\x -> return (x, g)))

instance MonadReader r m => MonadReader r (RandT m) where
	ask = lift ask
	local f m = RandT (\g -> do
		(x, g') <- m `runRandT` g
		y <- local f (return x)
		return (y, g'))

instance MonadWriter w m => MonadWriter w (RandT m) where
	tell = lift . tell
	listen r = RandT (\g -> do
		((x, g'), w) <- listen $ r `runRandT` g
		return ((x, w), g'))
	pass r = RandT (\g -> pass $ do
		((x, f), g') <- r `runRandT` g
		return ((x, g'), f))

instance MonadState s m => MonadState s (RandT m) where
	get = lift get
	put = lift . put

instance MonadIO m => MonadIO (RandT m) where
    liftIO = lift . liftIO

instance MonadPlus m => MonadPlus (RandT m) where
	mzero = lift mzero
	mplus a b = RandT (\g -> let (g', g'') = split g in 
		(a `runRandT` g') `mplus` (b `runRandT` g'')) 

-- | Similar to 'evalRand'.
evalRandT :: (RandomGen g, Monad m) => RandT m a -> g -> m a
evalRandT r g = (r `runRandT` g) >>= (\(x, _) -> return x)

-- | Similar to 'execRand'.
execRandT :: (RandomGen g, Monad m) => RandT m a -> g -> m g
execRandT r g = (r `runRandT` g) >>= (\(_, g') -> return g')
