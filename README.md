# Haskell - rand-vars

This library provides the `Rand` applicative monad. 
This monad represent computations that can return values at random.
The distribution of values can be specified using provided functions.

A monad transformer, `RandT`, is also available.

The class `MonadRand` allows to pick an element at random out of `Rand`. Instances for `Rand`, `RandT` and `IO` are provided.

# Creation of random variables

## `rand`

`rand` returns a random element, following distribution of underlying `Random` (from `System.Random`) instance.

```haskell
decision :: Rand Bool
decision = rand
```

## `inRange`

`inRange` returns an element at random from the specified range. The distribution follows the one of the underlying `Random` instance.

```haskell
die :: Rand Int
die = inRange (1, 6)
```

## `oneOf`

`oneOf` models a random variable that returns one with equal probabilities of the elements of the list.

```haskell
color :: Rand String
color = oneOf ["red", "green", "blue"]
```

If the list contains `n` elements, constructing the random variable takes `O(n)` time. Picking an element out of the random variable is constant time. 

## `fromFreqs`

`fromFreqs` allows to specify the frequency of each element. Note that the frequencies don't have to add up to `1`. Negative frequencies are considered null.

```haskell
biaisedDie :: Rand Int
biaisedDie = fromFreqs [1 `withFreq` 1.2, 
                        2 `withFreq` 1.1, 
                        3 `withFreq` 1, 
                        4 `withFreq` 1, 
                        5 `withFreq` 0.9, 
                        6 `withFreq` 0.8]
```

If the list contains `n` elements, constructing the random variable takes `O(n)` time. Picking an element out of the random variable takes `O(log(n))` time. 

If the sum of frequencies is `0`, the behavior is unspecified. 

Note that `withFreq` is simply an alias for `(,)`, the pair constructor.

# Picking value from random variables.

Any monad that has an instance of `MonadRand` can pick an element at random out of a random variable. To do so, simple use the `pick` function.

```haskell
pick :: MonadRand m => Rand a -> m a
```

`IO`, `Rand` and `RandT`, are instances of `MonadRand`.

# Code example

The following is a more complete example on the Casino theme!

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
