Haskell - rand-vars
===================

This library provides the `Rand` applicative monad. 
This monad represent computations that can return values at random.
The distribution of values can be specified using provided functions.

A monad transformer, `RandT`, is also available.

The class `MonadRand` allows to pick an element at random out of `Rand`. Instances for `Rand`, `RandT` and `IO` are provided.

Code Example
============

```haskell
 import Control.Monad.Random
 import Control.Applicative
 import Control.Monad

 data Slot = Lemon | Cherry | Strawberry | Orange | Bar | Seven deriving (Enum, Show)

 data Combination = Combination Slot Slot Slot deriving Show

 fairSlot = oneOf [Lemon .. Seven]
 fairCombination = Combination <$> fairSlot <*> fairSlot <*> fairSlot

 biasedSlot = fromFreqs [Lemon `withFreq` 1, 
                         Cherry `withFreq` 1, 
                         Strawberry `withFreq` 1.2, 
                         Orange `withFreq` 1.1,
                         Bar `withFreq` 0.9,
                         Seven `withFreq` 0.8]

 biasedCombination = Combination <$> biasedSlot <*> biasedSlot <*> biasedSlot


 aTripToAMachine = do
           combination <- fromFreqs [fairCombination `withFreq` 10, 
                                     biasedCombination `withFreq` 5]
           rounds      <- inRange (5, 50)
           replicateM rounds combination

 aTripToTheCasino = do
           trips <- fmap (*3) $ inRange (1, 10)
           fmap concat $ replicateM trips aTripToAMachine

 main = pick aTripToTheCasino >>= print
```
