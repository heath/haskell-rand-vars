{-# LANGUAGE Rank2Types #-}

module Rand(
	Rand(..), 
	-- * Evaluation
	evalRand, execRand, 
	-- * Creation
	rand, oneOf, inRange,
	fromFreqs, withFreq
	) where

import System.Random
import Data.Array.IArray ((!), listArray, Array)
import Control.Applicative
import Data.List (foldl', find)
import qualified Data.IntervalMap as IM

-- | Random variable of @a@.
newtype Rand a = Rand { runRand :: RandomGen g => g -> (a, g) }

instance Monad Rand where
	return x = Rand (\g -> (x, g))
	r >>= f = Rand (\g -> let (x, g') = r `runRand` g in f x `runRand` g')

instance Functor Rand where
	fmap f r = Rand (\g -> let (x, g') = r `runRand` g in (f x, g'))

instance Applicative Rand where
	pure = return
	f <*> x = Rand (\g -> 
		let (h, g') = f `runRand` g in 
		let (a, g'') = x `runRand` g' in (h a, g''))


-- | Run the random variable and returns only its value.
--   The new generator is lost.
evalRand :: RandomGen g => Rand a -> g -> a
evalRand v g = fst $ v `runRand` g 

-- | Run the random variable and returns only the new @RandomGen@.
execRand :: RandomGen g => Rand a -> g -> g
execRand v g = snd $ v `runRand` g

-- | Equiprobable distribution among the elements of the list.
oneOf :: [a] -> Rand a
oneOf xs = fmap ((!)arr) $ inRange range
	where 
		-- Creating an array for constant time lookup.
		range = (0, length xs - 1)
		arr = (listArray range :: [a] -> Array Int a) xs

-- | Distribution provided by @random@.
rand :: Random a => Rand a
rand = Rand random

-- | Distribution within a given range, provided by @randomR@.
inRange :: Random a => (a, a) -> Rand a
inRange r = Rand (\g -> randomR r g)

-- | Distribution of elements proportionately to their indicated frequency.
fromFreqs :: Real b => [(a, b)] -> Rand a
fromFreqs fs = Rand (\g ->
	let (from, to) = genRange g in
	let range      = toRational (to - from) in
	let ratio      = freqSum / range in
	let (i, g')    = next g in
	let j          = (*) ratio $ toRational (i - from) in
	case (IM.containing) intervalMap j of
		[(_, x)] -> (x, g')
		_        -> error "Index not in the map."
	)
	where
		elems = preprocess fs

		freqSum = sum $ map snd elems

		intervalMap = IM.fromAscList $ computeIntervals 0 elems

		preprocess = map (\(x, f) -> (x, toRational f)) . filter ((>0) . snd)

		computeIntervals _ [] = undefined
		computeIntervals lower ((v, f):[]) = let upper = (lower + f) in (IM.ClosedInterval lower upper, v):[]
		computeIntervals lower ((v, f):xs) = let upper = (lower + f) in (IM.IntervalCO lower upper, v):(computeIntervals upper xs)

-- | Alias for @(,)@.
withFreq :: Real b => a -> b -> (a, b)
withFreq = (,) 