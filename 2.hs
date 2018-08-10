{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

data Maybe a = Nothing | Just a

instance Show a => Show (Maybe a) where
    show Nothing = "Nothing"
    show (Just a) = "Just " ++ show a 

data Shape = Circle (Integer, Integer) | Square (Integer, Integer)

instance Show Shape where 
    show (Circle (r,c)) = "Circle (" ++ show r ++ "," ++ show c ++ ")"

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:xs) = Just x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (x:xs) = Just xs

lookupMay :: Eq a => a -> [(a,b)] -> Maybe b
lookupMay k [] = Nothing
lookupMay k (x:xs) = 
    if (k == fst x) then Just $ snd x 
    else lookupMay k xs

divMay :: (Eq a, Integral a) => a -> a -> Maybe a
divMay a 0 = Nothing 
divMay a b = Just (a `div` b)

{--
maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay [x] = Just x
maximumMay (x:xs) = if x >= Just (maximumMay xs) then Just x else maximumMay xs  

--}

maximumMay :: (Ord a, Num a) => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay [x] = Just x
maximumMay xs = Just $ foldl max 0 xs

minimumMay :: (Ord a, Num a) => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay [x] = Just x
minimumMay xs = Just $ foldl min mb xs
    where 
        mb = maxBound :: Int
