{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where
import MCPrelude

data Maybe a = Nothing | Just a 

instance Show a => Show (Maybe a) where
    show Nothing = "Nothing"
    show (Just a) = "Just " ++ show a

--
--data Person = String
--
--instance Show Person => Show Person where 
--    show Person = show 
--
--

data Expr = Val Int | Div Expr Expr

--eval :: Expr -> Int
--eval (Val a) = a
--eval safeDiv a b = div $ eval a $ eval b

safeDiv (Val m) (Val 0) = Nothing
safeDiv (Val m) (Val n) = Just $ div m n
