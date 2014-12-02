## A Week of Pragmas: Day 3 (Pattern Matching)

Today we'll be talking about several syntactic extensions that extend the concept of pattern matching in Haskell. As it turns out, there are several of these! Well tackle each of them in turn, using the following imports.

```haskell
{-# LANGUAGE TemplateHaskell
			 , PatternGuards
			 , NPlusKPatterns
			 , RecordWildCards
			 , PatternSynonyms
			 , ViewPatterns
			 , RankNTypes #-}

import qualified Data.Map as M
import Control.Comonad.Cofree
import Control.Applicative((<$>))
import Lens.Family2
import Lens.Family2.TH
```

The first extension we'll look at is `PatternGuards`. Say you want to pattern match on multiple things at once, but don't want to nest pattern matching statements in order to match - really - a single pattern. This is best illustrated with an example, which I've borrowed from [Stephen Diehl's excellent post on what he wished he knew while learning Haskell](http://www.stephendiehl.com/what/):

```haskell
addLookup :: (Ord k, Num a) => M.Map k a -> k -> k -> Maybe a
addLookup env var1 var2
   | Just val1 <- M.lookup var1 env
   , Just val2 <- M.lookup var2 env
   = Just $ val1 + val2
   | otherwise = Nothing

```

The above code unpacks two calls to `M.lookup` in a `Map`, checks if *both* of them are `Just` values, and if they are, wraps them back in a `Just` and adds their internal contents. In all other cases, it returns `Nothing`. For instance:

```haskell
λ> addLookup (M.fromList [(1, 1), (2, 2)]) 1 2
Just 3
```

Try doing this without `PatternGuards`, and you'll find it much less clean!

We can combine `PatternGuards` with other useful extensions. For example, using `NPlusKPatterns`, we can express constraints on natural numbers. As an ad-hoc example, take a look at `check`:

```haskell
check :: (Floating a, RealFrac a) => a -> a -> Maybe Int
check a b
    | (n+6) <- floor $ sqrt a
    , (m+1) <- floor $ sqrt b
    = Just $ n + m
    | otherwise = Nothing
```

Here, we require that the floor of the square root of `a` is greater than or equal to 6, and similarly for `b`.

Another useful extension is `RecordWildCards`, which allows us to unpack a data type's member variables and use them as local variables in functions automatically. For example, consider the following program:

```haskell
data Foo =
    Bar{ n :: Int }
  | Baz{ m :: Int }
    deriving (Show, Eq)

toFoo :: Bool -> Foo
toFoo True  = Bar 1
toFoo False = Baz 1 

-- Record Wild Cards
check2 :: Bool -> Bool -> Int
check2 a b
  | Baz{..} <- toFoo a
  , Bar{..} <- toFoo b
  = n + m
  | otherwise = 0
```

The `{..}` notation is what `RecordWildCards` allows; we're able to directly access `n` and `m` from the `Foo` constructor without explicitly pattern matching on it. The example is a little contrived, but you can imagine that this is useful when dealing with types with many internal variables.

Now, we're going to switch gears a bit. We all know that `Cofree []` is a nonempty Rose Tree, right?!

If not, don't worry -- this is a much more complex sounding statement than it really is. The definition of `Cofree` is:

```haskell
data Cofree f a = a :< Cofree f a
```

Substituting `[]` in for `f`, we get:

```haskell
a :< Cofree [a]
```

which is isomorphic to:

```haskell
(a, Cofree [a])
```

Which basically says that we have an element of type `a`, along with a list of trees with roots of type `a`. Pattern matching on `Cofree` in this manner might be confusing, but perhaps we really do want to use this representation of a Rose Tree. What do we do?

Let me introduce `PatternSynonyms`. This extension allows us to refer to patterns by a different name. In our case, the following will work:

```haskell
pattern RoseNode a xs = a :< xs
pattern RoseLeaf a    = a :< []
```

Now we can pattern match on Rose Trees with our new patterns, like in this function, which traverses downwards from a `RoseNode` if possible:

```haskell
toTuple :: Cofree f a -> (a, f (Cofree f a))
toTuple (a :< b) = (a, b)

goto :: Eq a => a -> Cofree [] a -> Maybe (Cofree [] a)
goto x (RoseNode _ xs) = RoseNode x <$> lookup x (map toTuple xs)
```

Note that we use `RoseNode` in two places here: When deconstructing in the pattern match, and to actually construct a `RoseNode` in the body of the function. Cool!

Now, say we want to do something a little more complex. Perhaps we want to run our data through a legitimate function, and check its output to pattern match on. This is what `ViewPatterns` allows us to do. Consider the function `children`, for example, which provides a type of "view" for a Rose Tree:

```haskell
children (RoseNode _ xs) = xs
children (RoseLeaf _)    = []
```

Perhaps we want to check if a node has any children before processing it:

```haskell
hasChildren (children -> []) = False
hasChildren _                = True
```

Here, we run `children` on the input, and if it has none, we return `False`. A basic example, but again, you can see how this can be powerful with an appropriately complex `view`!

The name `ViewPatterns` got me thinking: We use `view` in `Lens` to look at fields in records. Perhaps we can apply this here?

I'll leave this little bit of code for you to pore over; I think this is a really neat snippet. Try running/tuning it and see what happens!

```haskell
data Vector3 a = Vector3{ _x :: a, _y :: a, _z :: a }
makeLenses ''Vector3

checkField :: Eq a => Lens' (Vector3 a) a -> a -> Vector3 a -> Bool
checkField field n (view field -> t) = n == t

main = print $ checkField (Vector3 1 2 3) x 1
```

Thanks for reading, see you tomorrow!