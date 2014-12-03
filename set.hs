{-# LANGUAGE RebindableSyntax #-}
import Data.Set as S
import Prelude hiding ((>>=))

main = print $ do
    x <- S.fromList [1, 2, 3]
    y <- S.fromList [4, 5, 6]
    return (x * y)
    where (>>=) = setBind
          return = S.singleton

setBind :: Ord b => Set a -> (a -> Set b) -> Set b
setBind s f = S.foldr S.union S.empty (S.map f s)