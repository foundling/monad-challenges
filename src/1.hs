{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

type Gen a = Seed -> (a, Seed)

-- 1.1

nextRand :: (a, Seed) -> (Integer, Seed)
nextRand = rand . snd 

nextRandLetter :: (Char, Seed) -> (Char, Seed)
nextRandLetter = randLetter . snd

fiveRands :: [Integer]
fiveRands = map fst . take 5 . iterate nextRand . rand . mkSeed $ 1

-- 1.2

lift1 :: (a -> b) -> (a,c) -> (b,c)
lift1 f (a,b) = (f a, b)  

lift2 :: (b -> c) -> (a,b) -> (a,c)
lift2 f (a,b) = (a, f b)  

randLetter :: Gen Char
randLetter = lift1 toLetter . rand

randString3 :: [Char]
randString3 = map fst . take 3 . iterate nextRandLetter . randLetter $ mkSeed 1

-- 1.3

generalA :: (a -> b) -> Gen a -> Gen b
generalA f g s = (a',s')
    where
        a' = f a
        (a,s') = g s

generalA' :: (a -> b) -> Gen a -> Gen b
generalA' f g = (\ (a,b) -> (f a, b)) . g

randEven :: Gen Integer
randEven = generalA (*2) rand

randOdd :: Gen Integer
randOdd = generalA (+1) randEven

randTen :: Gen Integer
randTen = generalA (*10) rand

-- 1.4

randPair :: Gen (Char,Integer)
randPair s = ((c,i),s'')
    where
        (i,s'') = rand s' 
        (c,s') = randLetter s

generalPair :: Gen a -> Gen b -> Gen (a,b) 
generalPair genA genB s = ((a,b), s2)
    where
        (a, s1) = genA s
        (b, s2) = genB s1

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB f genA genB s = (f a b, s'')
    where
        (a, s')  = genA s
        (b, s'') = genB s

generalPair2 :: Gen a -> Gen b -> Gen (a,b)
generalPair2 = generalB (,)

-- 1.5

-- takes a list of Gen a, returns a Gen of [a] 
