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
generalB f ga gb s = (c, s'')
    where   
        c = f a b
        (b, s'') = gb s'
        (a, s') = ga s

generalPair2 :: Gen a -> Gen b -> Gen (a,b)
generalPair2 = generalB (,)

-- 1.5

--repRandom :: [Gen a] -> Gen [a]
--repRandom :: [(Seed -> (a, Seed))] -> Seed -> [a]
--repRandom [] s = []
--repRandom [ga] s = [a] 
--    where
--        (a,_) = ga s

--repRandom :: [Gen a] -> Gen [a]
repRandom :: [Seed -> (a, Seed)] -> Seed -> [a]
repRandom [] s = []
repRandom (g:gs) s = [a] ++ repRandom gs s'
    where 
        (a,s') = g s 
