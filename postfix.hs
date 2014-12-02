{-# LANGUAGE PostfixOperators, LambdaCase, MultiWayIf, NamedFieldPuns, RecordWildCards, NumDecimals, InstanceSigs #-}

-- DONE

-- LambdaCase
-- desugars to \y -> case y of {...}
maybe' def = \case
    Just x  -> x
    Nothing -> def

--λ> maybe' 10 (Just 3)
--3
--λ> maybe' 10 Nothing
--10

-- PostFixOperators
-- Operator can take a single argument, but LHS still needs to be in this form.
(!) n = product [2..n]

-- Must be wrapped in parens
--λ> (10!)
--3628800
-- λ> map (!) [1..10]
-- [1,2,6,24,120,720,5040,40320,362880,3628800]

-- MultiWayIf
collatz :: Integer -> [Integer]
collatz n = 
    if | n == 1 -> []
       | even n -> (n `div` 2) : collatz (n `div` 2)
       | odd n  -> (3 * n + 1) : collatz (3 * n + 1)

--λ> collatz 99
--[298,149,448,224,112,56,28,14,7,22,11,34,17,52,26,13,40,20,10,5,16,8,4,2,1]

data Vector3 a = Vector3{ _x :: a, _y :: a, _z :: a } deriving Show

-- NamedFieldPuns
-- Usage in a pattern match
magnitude :: Floating a => Vector3 a -> a 
magnitude (Vector3 {_x, _y, _z})   = sqrt . sum $ map (^2) [_x, _y, _z]

-- RecordWildCards
-- Record Wild Cards version
magnitude' :: Floating a => Vector3 a -> a 
magnitude' (Vector3 {..}) = sqrt . sum $ map (^2) [_x, _y, _z]

-- NamedFieldPuns
-- Usage in an expression
normalize :: Floating a => Vector3 a -> Vector3 a
normalize v@(Vector3 x y z) = Vector3 {_x, _y, _z}
  where m = magnitude v
        [_x, _y, _z] = map (/m) [x, y, z]

--λ> normalize $ Vector3 2 3 4
--Vector3 {_x = 0.3713906763541037, _y = 0.5570860145311556, _z = 0.7427813527082074}

-- NumDecimals
bignumber :: Integer
bignumber = 173.9e97
