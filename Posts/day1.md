# A Week of Pragmas: Day 1 (Pragmas Galore!)

## Intro

GHC pragmas are flags given to haskell programs to modify various syntactic and semantic rules. There are many of them, so my aim this week is to discuss as many as possible and give some simple examples of when/where each of them is useful. I will be writing one post detailing one or more pragma of some theme for seven days. This is a project I've been excited to work on for a while now; I hope you enjoy it! We'll kick off the week with a discussion of several small syntactic extensions.

## Pragmas Galore

We'll talk about these in order of inclusion; add this to the top of your source file if you'd like to follow along:

```haskell
{-# LANGUAGE PostfixOperators, LambdaCase, MultiWayIf, NamedFieldPuns, RecordWildCards, NumDecimals, InstanceSigs #-}
```

## PostfixOperators

The `PostfixOperators` extension allows you to define operators that operate on the value *before* their placement. A good example from mathematics is the factorial ($!$) operator, which is equivalent to $\product{k=1}{n}k$. We can use `PostFixOperators` to program this directly into Haskell as follows:

```haskell
(!) :: Integer -> Integer
(!) n = product [1..n]
```

Note that the operator can take a single argument, but the left-hand side still needs to be in the form above (i.e. `(n!) = ...` is still invalid syntax).

To use this operator, we must wrap the expression it appears in in parentheses:

```haskell
λ> (10!)
3628800
```

We can also use `(!)` like any ordinary function, and, for example, map over a list with it:

```haskell
λ> map (!) [1..10]
[1,2,6,24,120,720,5040,40320,362880,3628800]
```

## LambdaCase

Have you ever written something like like this:

```haskell
example :: Monoid a => Maybe a -> a
example foo = case foo of
  Nothing -> mempty
  Just x  -> x
```

and wanted to get rid of all redundancy? Maybe you'd consider breaking `example` into two separate pattern-matching cases:

```haskell
example Nothing = mempty
example (Just x) = x
```

but now we're duplicating `example`! `LambdaCase` simply allows you to get rid of the redundancy in the above expression:

```haskell
example = \case
    Just x  -> x
    Nothing -> mempty
```

The expression bound to `example` desugars to something of the form `\foo -> case foo of {...}`. For example:

```
λ> example Nothing :: [Int]
 []
λ> example (Just [1, 2, 3])
 [1, 2, 3]
```

## MultiWayIf

`MultiWayIf` allows you to refactor pesky if/else chains into one convenient, nicely formatted syntactic block. As an example, perhaps we want to find the collatz sequence starting from some number. There are three cases to consider, which we can consolidate into a single syntactic block:

```haskell
collatz :: Integer -> [Integer]
collatz n = 
    if | n == 1 -> []
       | even n -> (n `div` 2) : collatz (n `div` 2)
       | odd n  -> (3 * n + 1) : collatz (3 * n + 1)
```

This looks similar to putting three guards into `collatz`, but it doesn't need to actually come after a function call. If we wanted to declare `collatz` anonymously for some reason, we might have to do something like this:

```haskell
\n -> if n == 1 then [] else if even n then ((n `div` 2) : collatz (n `div` 2)) else ((3 * n + 1) : collatz (3 * n + 1))
```

But using `MultiWayIf`, this becomes a lot cleaner:

```haskell
\n -> if | n == 1 -> []
         | even n -> (n `div` 2) : collatz (n `div` 2)
         | odd n  -> (3 * n + 1) : collatz (3 * n + 1)
```

## NamedFieldPuns

`NamedFieldPuns` automatically binds named record fields to variables with the same name in expressions. That sentence might be confusing, so let's look at some examples:

```haskell
data Vector3 a = Vector3{ _x :: a, _y :: a, _z :: a } deriving Show
```

We can pattern match on `Vector3`s individual fields and use them explicitly in a function:

```haskell
magnitude :: Floating a => Vector3 a -> a 
magnitude (Vector3 {_x, _y, _z})   = sqrt . sum $ map (^2) [_x, _y, _z]
```

Here, we avoid saying (`Vector3 {_x = _x, _y = _y, _z = _z}`) to save us some pain. If we only needed a subset of the record's fields, we also could have extracted them by writing a pattern that only references them, e.g. `Vector3 {_x, _y}`.

We can also use `NamedFieldPuns` in expressions. That is, instead of setting `Vector3 {_x = 9, ...}`, we can just bind a value to a variable `_x` in our expression and use it directly. For instance:

```haskell
normalize :: Floating a => Vector3 a -> Vector3 a
normalize v@(Vector3 x y z) = Vector3 {_x, _y, _z}
  where m = magnitude v
        [_x, _y, _z] = map (/m) [x, y, z]
```

This time, the right side of the expression sets a `Vector3`'s parameters based on the value of `_x`, `_y`, and `_z` in scope of the expression.

## RecordWildCards

`RecordWildCards` takes `NamedFieldPuns` one step further. By tacking on `{..}` to the end of a constructor, you can bind variables in an expression to all of the names of variables in that constructor and use them in the expression. For example, we can rewrite `magnitude` using `RecordWildCards` like so:

```haskell
magnitude' :: Floating a => Vector3 a -> a 
magnitude' (Vector3 {..}) = sqrt . sum $ map (^2) [_x, _y, _z]
```

Here, we don't explicitly grab `_x`, `_y`, and `_z`, but they are automatically bound and we can use them in the function.

## NumDecimals

This extension is tiny, but suppose you have some constant large number with lots of trailing 0's that you want to use. Using `NumDecimals`, you can define it using scientific notation. For instance: 

```haskell
bignumber :: Integer
bignumber = 173.9e97
```

```
λ> bignumber
1739000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
```

## InstanceSigs

Have you ever tried to define an instance of a typeclass and typed out the specific signature of an instance method in order to drive your implementation? For example, perhaps you're trying to redefine the `Monad` instance for `Maybe`, and you're new this weird functional programming stuff so you're having a little trouble:

```haskell
instance Monad Maybe where
  return = ???
  (>>=)  = ???
```

Well, a good first step would be to look at the types of `return` and `>>=` in the general form, and try to figure out what the appropriate type signature specific to `Maybe` would look like. We get:

```haskell
instance Monad Maybe where
  return :: a -> Maybe a
  return = Just
  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  Nothing  >>= f = Nothing
  (Just a) >>= f = f a 
```

Well, this doesn't compile by default, because GHC doesn't expect those type signatures to be there. However, with `InstanceSigs` enabled, it does. I often find myself writing out type signatures above typeclass function implementations in exactly this manner in a comment. With `InstanceSigs`, the comment isn't necessary.

See you tomorrow!

Ben