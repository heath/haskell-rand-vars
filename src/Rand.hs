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
	let range = toRational (to - from) in
	let s = toRational $ sum $ map snd fs in
	let ratio = range / s in
	let xs = computeThreshold ratio fs in
	let (i, g') = next g in
	let di = toRational (i - from) in
	let Just (x, _) = find (\(x, s) -> di <= s) xs in (x, g'))
	where
		computeThreshold :: Real b => Rational -> [(a, b)] -> [(a, Rational)]
		computeThreshold r fs = reverse $ foldl' (go ((*r) . toRational)) [] fs
	
		go :: (b -> Rational) -> [(a, Rational)] -> (a, b) -> [(a, Rational)]
		go steps [] (x, f) = let step = steps f in if step > 0 then [(x, step)] else []
		go steps xs@((_, d):_) (x, f) = let step = steps f in if step > 0 then (x, d + step):xs else xs

-- | Alias for @(,)@.
withFreq :: Real b => a -> b -> (a, b)
withFreq = (,) 