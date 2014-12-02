{-# LANGUAGE TypeOperators, RankNTypes, KindSignatures #-}

import Prelude hiding ((.), map)
import Control.Category
import Control.Comonad

-- DONE

type Endo a = a -> a
type f <~ g = g -> f
-- Natural transformation
type f :~> g = forall a. f a -> g a
type a >>- b = forall m. Monad m => a -> m b
type a ->> b = forall w. Comonad w => w a -> b

-- Natural Transformations!
f :: Either t :~> Maybe
f (Right x) = Just x
f (Left _)  = Nothing

g :: [] :~> Maybe
g []    = Nothing
g (x:_) = Just x

h :: [] :~> Either String
h []    = Left "empty list!"
h (x:_) = Right x

-- maybe one useful case, but still confusing as hell!
map :: b <~ a -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs 

-- confuse your friends!
addTwo :: Int <~ Int <~ Int
addTwo x y = x + y

apply :: Endo Int -> Int <~ Int
apply f x = f x

-- this is id
return' :: a >>- a
return' = return

-- Easy to see how this is composition
(<=<) :: (b >>- c) -> (a >>- b) -> a >>- c
f <=< g = \a -> g a >>= f

-- and how this is id
coreturn :: a ->> a
coreturn = extract

-- And this too!
(=<=) :: (b ->> c) -> (a ->> b) -> a ->> c
f =<= g = f . extend g
