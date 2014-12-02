{-# LANGUAGE Arrows, TupleSections, LambdaCase #-}

{-
After LambdaCase
Missing:
TypeSynonymInstances

Needs work.

-}

import Prelude hiding ((.), id)
import Control.Category
import Control.Monad((<=<), join)
import Control.Comonad
import Control.Arrow hiding (Kleisli(..))
import Control.Applicative((<$>))

newtype Kleisli m a b   = K  { runKleisli :: a -> m b }
newtype CoKleisli w a b = CK { runCoKleisli :: w a -> b }

instance Monad m => Category (Kleisli m) where
  id            = K return
  (K f) . (K g) = K (f <=< g)

instance Comonad w => Category (CoKleisli w) where
  id              = CK extract
  (CK f) . (CK g) = CK (f =<= g)

instance (Monad m, Functor m) => Arrow (Kleisli m) where
  arr f        = K $ return . f
  first (K f)  = K (\(b, d) -> (,d) <$> f b)
  second (K f) = K (\(d, b) -> (d,) <$> f b)

addA :: Arrow a => a b Int -> a b Int -> a b Int
addA f g = proc x -> 
  do
    y <- f -< x
    z <- g -< x
    returnA -< y + z

f :: Kleisli Maybe String Int
f = K $ \case
  "" -> Nothing
  xs -> Just $ length xs

g :: Kleisli Maybe String Int
g = K $ \case
  "butt" -> Nothing 
  xs     -> Just $ length xs

something :: String -> Maybe Int
something = runKleisli (addA f g)

-- Unrelated but cool bits
-- RankNTypes
--newtype Nat f g = N { runNat :: forall a. f a -> g a }

--instance Category Nat where
--  id = N id
--  (N f) . (N g) = N (f . g)
