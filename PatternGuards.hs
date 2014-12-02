{-# LANGUAGE TemplateHaskell, PatternGuards, NPlusKPatterns, RecordWildCards, PatternSynonyms, ViewPatterns, RankNTypes #-}

import qualified Data.Map as M
import Control.Comonad.Cofree
import Control.Applicative((<$>))
import Lens.Family2
import Lens.Family2.TH

-- About pattern matching

addLookup :: (Ord k, Num a) => M.Map k a -> k -> k -> Maybe a
addLookup env var1 var2
   | Just val1 <- M.lookup var1 env
   , Just val2 <- M.lookup var2 env
   = Just $ val1 + val2
   | otherwise = Nothing

-- λ> addLookup (M.fromList [(1, 1), (2, 2)]) 1 2

check :: (Floating a, RealFrac a) => a -> a -> Maybe Int
check a b
    | (n+6) <- floor $ sqrt a
    , (m+1) <- floor $ sqrt b
    = Just $ n + m
    | otherwise = Nothing 

-- λ> check 900 70

data Foo =
    Bar{ n :: Int }
  | Baz{ m :: Int }
    deriving (Show, Eq)

toFoo :: Bool -> Foo
toFoo True  = Bar 1
toFoo False = Baz 1 

check2 :: Bool -> Bool -> Int
check2 a b
  | Baz{..} <- toFoo a
  , Bar{..} <- toFoo b
  = n + m
  | otherwise = 0

-- λ> check2 False True

-- (a, h (Cofree h a))
-- (a, [Cofree [a]])
-- (a, Identity (Cofree (Identity a)))
-- (a, Maybe (Cofree (Maybe a)))

toTuple :: Cofree f a -> (a, f (Cofree f a))
toTuple (a :< b) = (a, b)

pattern RoseNode a xs = a :< xs
pattern RoseLeaf a    = a :< []

--point  (a :< _)  = a
--goto x (_ :< xs) = lookup x (map toTuple xs)

test = 10 :< [5 :< [], 7 :< []]

--goto x (RoseNode _ xs) = (x :<) <$> lookup x (map toTuple xs) 
goto x (RoseNode _ xs) = RoseNode x <$> lookup x (map toTuple xs) 

-- rename this "view" to begin with in blog post
point (RoseNode a _)  = a
point (RoseLeaf a)    = a

children (RoseNode _ xs) = xs
children (RoseLeaf _)    = []

isTen (point -> 10) = True
isTen _            = False

hasChildren (children -> []) = False
hasChildren _                = True

--λ> hasChildren test
--True
--(0.00 secs, 1547416 bytes)
--λ> hasChildren <$> goto 5 test
--Just False

data Vector3 a = Vector3{ _x :: a, _y :: a, _z :: a }
makeLenses ''Vector3

checkField :: Eq a => Lens' (Vector3 a) a -> a -> Vector3 a -> Bool
checkField field n (view field -> t) = n == t
