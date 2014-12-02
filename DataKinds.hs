{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}

-- Goes along with GADTSyntax, about making *Kinds*
-- Together, making data types and kinds
-- DONE

--λ> :t Just
--Just :: a -> Maybe a
--λ> Just 3
--Just 3

-- Nat can be a kind!
data Nat = Z | S Nat
    deriving (Show, Eq)

-- λ> :k Z
-- Z :: Nat

data Vector :: Nat -> * -> * where
 Nil :: Vector Z a
 Cons :: a -> Vector n a -> Vector (S n) a

toInt :: Nat -> Int
toInt Z     = 0
toInt (S x) = succ (toInt x)

fromInt :: Int -> Nat
fromInt n = foldr1 (.) (replicate n S) Z

--λ> fromInt 20
--S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S Z)))))))))))))))))))
--(0.01 secs, 1544552 bytes)
--λ> toInt $ fromInt 20
--20

-- Uses GADTs and KindSignatures

toList :: Vector n x -> [x]
toList Nil = []
toList (Cons a v) = a : toList v

test :: Vector (S Z) Int
test = Cons 1 Nil 

--λ> :t toList
--toList :: Vector n x -> [x]
--λ> :t toList test
--toList test :: [Int]
--λ> toList test
--[1]
