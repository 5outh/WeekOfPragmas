{-# LANGUAGE RebindableSyntax, NoMonomorphismRestriction, DeriveFunctor #-}

import Prelude hiding ((>>=), (>>), return)
import Data.Monoid
import Data.Map as M
import Control.Monad hiding ((>>))
import Bang

-- Maybe talk about derives here.

--(>>)   = (+)
--return = 0

--check i = do
--    i
--    80
--    60
--    10

--(>>)   = mappend
--return = mempty

-- check :: String
--check = do
--    "a"
--    "b"

--check :: Sum Int
--check = do
--    Sum 1
--    Sum 2

--check :: Product Int
--check = do
--    Product 10
--    Product 30

--(>>)    = flip (.)
--return  = id

--check = do
--    (+1)
--    (*100)
--    (/300)
-- square root of 69 is 8 something, right?

--check = do
--    sum
--    sqrt
--    floor

-- Talk about why this makes no sense. (param is same as input (w a))
--(>>=) = (=>>)
--return = extract

-- (>>=)  = flip fmap
-- return = id

-- addOne :: (Num t, Functor f) => f t -> f t
-- addOne fx = do 
--     y <- fx
--     y + 1

-- addTwo :: (Num t, Functor f) => f t -> f t
-- addTwo fx = do
--     y <- addOne fx
--     y + 1

-- kleisliExample :: M.Map k a -> M.Map a b -> Maybe b

-- (>>) = (<=<)

-- kleisliExample :: (Ord a, Ord b) => Map b c -> Map a b -> a -> Maybe c
-- kleisliExample mp1 mp2 = do
--   flip M.lookup mp1
--   flip M.lookup mp2

-- Bang!
music1, music2 :: Music Dur PercussionSound
music1 = do
  m4 bd bd bd bd
  hc
  m4 bd qr bd qr >< m4 hc hc hc hc
  where (>>) = (<>)

music2 = do
  quintuplets $ 5 #> do 
    bd
    cc 
  where (>>) = (><)

music :: IO ()
music = bang $ 4 !> do
    music1
    music2
    where (>>) = (<>)

