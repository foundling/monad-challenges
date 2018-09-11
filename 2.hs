{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where
import MCPrelude

data Maybe a = Just a | Nothing

instance Show a => Show (Maybe a) where
  show (Just a) = "Just " ++ show a
  show Nothing = "Nothing"

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:xs) = Just x 

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (x:xs) = Just xs

lookupMay :: Eq a => a -> [(a,b)] -> Maybe b
lookupMay k [] = Nothing
lookupMay k (p:ps) = if (fst p) == k then Just (snd p) else lookupMay k ps 

divMay :: (Eq a, Integral a) => a -> a -> Maybe a
divMay x 0 = Nothing 
divMay x y = Just $ div x y

--
--    • In the expression: maximumMay y : xs
--
--Couldn't match expected type ‘Maybe a’
--                  with actual type ‘[Maybe a0]’
--
maximumMay :: (Ord a) => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay [x] = Just x
maximumMay [x,y] = Just $ min x y
maximumMay (x:y:xs) = if x >= y then maximumMay (x:xs) else maximumMay (y:xs)

minimumMay :: (Ord a) => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay [x] = Just x
minimumMay [x,y] = Just $ min x y
minimumMay (x:y:xs) = if x <= y then minimumMay (x:xs) else minimumMay (y:xs) 
