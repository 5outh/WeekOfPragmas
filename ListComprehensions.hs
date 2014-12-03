{-# LANGUAGE ParallelListComp, TransformListComp, MonadComprehensions, RecordWildCards #-}

import GHC.Exts
import qualified Data.Map as M
import Data.List
import Data.Ord

-- Just one more sql-like query

-- After RecordWildCards

-- Parallel List Comp: Zips lists together instead of pulling all of the elt's out
arithmetic :: [Int]
arithmetic = [ x + y * z 
             | x <- [0..10]  
             | y <- [10..20] 
             | z <- [20..30]
             ]


fibs :: [Int]
fibs = 0 : 1 : [ x + y 
               | x <- fibs
               | y <- tail fibs
               ]

-- λ> take 10 fibs
-- [0,1,1,2,3,5,8,13,21,34]

fiblikes :: [Int]
fiblikes = 0 : 1 : 2 : [ x + y + z
                       | x <- fibs
                       | y <- tail fibs
                       | z <- tail (tail fibs)
                       ]

-- λ> take 10 fiblikes
-- [0,1,2,4,6,10,16,26,42,68]
-- (0.02 secs, 2644856 bytes)

-- SQL-like comprehensions
-- sortWith :: Ord b => (a -> b) -> [a] -> [a]
-- the :: Eq a => [a] -> a
-- groupWith :: Ord b => (a -> b) -> [a] -> [[a]]

data Character = Character{ firstName :: String, lastName :: String, birthYear :: Int }
  deriving (Show, Eq)

friends :: [Character]
friends = [ Character "Phoebe" "Buffay" 1963
          , Character "Chandler" "Bing" 1969
          , Character "Rachel" "Green" 1969
          , Character "Joey" "Tribbiani" 1967
          , Character "Ross" "Geller" 1966
          ]

oldest :: Int -> [Character] -> [String]
oldest k tbl = [ firstName ++ " " ++ lastName
               | Character{..} <- tbl
               , then sortWith by birthYear
               , then take k ]

groupByLargest :: Ord b => (a -> b) -> [a] -> [[a]]
groupByLargest f = sortBy (comparing (negate . length)) . groupWith f

bestBirthYears :: Int -> [Character] -> [(Int, [String])]
bestBirthYears k tbl = [ (the birthYear, firstName)
                       | Character{..} <- tbl
                       , then group by birthYear using groupByLargest
                       , then take k
                       ]

-- ONE more query.

employees = [ ("Simon", "MS", 80)
           , ("Erik", "MS", 100)
           , ("Phil", "Ed", 40)
           , ("Gordon", "Ed", 45)
           , ("Paul", "Yale", 60) ]

output = [ (the dept, sum salary)
        | (name, dept, salary) <- employees
        , then group by dept using groupWith
        -- , then sortWith by (sum salary)
        , then take 5 ]

-- keep in mind the original list comprehension:
-- [(a, b) | a <- xs, b <- ys]
-- == do a <- xs
--       b <- ys
--       return (a, b)

sqrts :: M.Map Int Int
sqrts = M.fromList $ [ (x, sx)
                     | x  <- map (^2) [1..100]
                     | sx <- [1..100]
                     ]

monadExample :: Maybe Int
monadExample = [ x+y | x <- Just 1, y <- Just 2 ]

sumIntSqrts :: Int -> Int -> Maybe Int
sumIntSqrts a b = [ x + y | x <- M.lookup a sqrts, y <- M.lookup b sqrts ]

greet :: IO String
greet = [ name 
        | name <- getLine
        , _ <- putStrLn $ unwords ["Hello, ", name, "!"]
        ]

-- couple more monad examples could be nice

-- Desugars to:
-- do x <- Just 1
--    y <- Just 2
--    return $ x + y

