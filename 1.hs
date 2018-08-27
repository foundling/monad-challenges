{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

type Gen a = Seed -> (a, Seed)

-- 1.1

nextRand :: (Integer,Seed) -> (Integer,Seed)
nextRand = rand . snd 

nextRandLetter :: (Char,Seed) -> (Char,Seed)
nextRandLetter = randLetter . snd

-- abstract the gen
-- next :: Gen Integer -> (Integer,Seed) -> (Integer,Seed)

fiveRands :: [Integer]
fiveRands = heads . take n . iterate nextRand . rand $ seed
    where
        n = 5
        seed = mkSeed 1
        heads = map fst

-- 1.2

lift' :: (a -> b) -> (a,c) -> (b,c)
lift' f (a,b) = (f a, b)

randLetter :: Gen Char
randLetter = lift' toLetter . rand

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


-- note: we're passing in 's' after the two gen types, 
-- so that's the 'Seed' in: Seed -> ((a,b), Seed)
-- thus, we just need to return the resulting pair, 
-- not the function that produces it
generalPair :: Gen a -> Gen b -> Gen (a,b) 
generalPair ga gb s = ((a,b),s'')
    where
        (a,s')  = ga s 
        (b,s'') = gb s'

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB f ga gb s = (f a b,s)
  where
    (b, s'') = gb s'
    (a, s') = ga s

generalPair2 :: Gen a -> Gen b -> Gen (a,b)
generalPair2 = generalB (,)

-- 1.5
repRandom :: [Seed -> (a, Seed)] -> Seed -> ([a], Seed)
repRandom [] s = ([],s)
repRandom (gen:gens) s = (x:xs,s'')  
    where
        (x,s') = gen s
        (xs,s'') = repRandom gens s'


data Pair a b = Pair (a,b)
    deriving (Show, Eq)



