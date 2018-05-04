
## Haskell for Clojure Programmers



#### Why might we be interested in Haskell? (A few random reasons)

* It represents an extension to the lambda calculus, which Lisp is based on, and therefore Clojure as well. <!-- .element: class="fragment" data-fragment-index="1" -->
* A Clojure programmer may find the variety of persistent data structures provided natural and fun to use, if appearing verbose at first. <!-- .element: class="fragment" data-fragment-index="2" -->
* The flexibity provided in leveraging the type system to generate structure can be akin to how macros are used in Lisp, but with more logical rigor. <!-- .element: class="fragment" data-fragment-index="3" -->


#### Most importantly?

Thinking about the types of values (and types of types!) can help provide a strong foundation for structuring programs in a safe, rigorous, consistent, and _simple_ way.



### What is Haskell's lineage?

Committee-driven, created to provide a lazy, functional language as a basis for academic research, as the widely-used language Miranda was proprietary.


### What is Haskell's lineage?

* Lisp (1958) - lambda calculus <!-- .element: class="fragment" data-fragment-index="1" -->
* ML - "Meta Language" (1973): (Damas) Hindley Milner type inference, garbage collection, pattern matching, currying <!-- .element: class="fragment" data-fragment-index="2" -->
* NPL (-> Hope) - algebraic data types, list comprehension <!-- .element: class="fragment" data-fragment-index="3" -->
* Miranda - laziness and purity, syntax <!-- .element: class="fragment" data-fragment-index="4" -->

Unique to Haskell: monadic effects, typeclasses for ad-hoc polymorphism <!-- .element: class="fragment" data-fragment-index="5" -->



#### What is already familiar to us?

* Functions are first-class <!-- .element: class="fragment" data-fragment-index="1" -->
* Immutable Data (maps, lists, vectors, arrays, etc.) <!-- .element: class="fragment" data-fragment-index="2" -->
* A lot of thought around managing state <!-- .element: class="fragment" data-fragment-index="3" -->
* "How is data shaped" <!-- .element: class="fragment" data-fragment-index="4" -->
* Mapping, Reducing, etc. (Generic operations/Parametric Polymorphism) <!-- .element: class="fragment" data-fragment-index="5" -->
* Type Classes are reasonably similar to Protocols (Ad-hoc Polymorphism) <!-- .element: class="fragment" data-fragment-index="6" -->
* Laziness* <!-- .element: class="fragment" data-fragment-index="7" -->

(*Kinda sorta) <!-- .element: class="fragment haskell-talk-small" data-fragment-index="8" -->




#### What is new to us?

* The type system! <!-- .element: class="fragment" data-fragment-index="1" -->
* Tons of algebraic structures/category theoretic concepts in core libraries <!-- .element: class="fragment" data-fragment-index="2" -->
* "Built-in" Recursion <!-- .element: class="fragment" data-fragment-index="3" -->
* Pattern Matching* <!-- .element: class="fragment" data-fragment-index="4" -->
* Laziness* <!-- .element: class="fragment" data-fragment-index="5" -->

(*Kinda sorta) <!-- .element: class="fragment haskell-talk-small" data-fragment-index="6" -->



### Types

> The fundamental purpose of a type system is to prevent the occurrence of execution errors during the running of a program.

Luca Cardelli, Microsoft Research <!-- .element: class="haskell-talk-small" -->


> The purpose of the type system is to rule out bad programs without ruling out too many good programs.

Edward Kmett, Extremely Prolific Haskell Programmer and Haskell Core Libraries Committee Chair <!-- .element: class="haskell-talk-small" -->


#### Static vs. Dynamic

> Types are an alternative semantics to runtime evaluation. Thus, asking for types is asking for greater power to analyze and comes at a cost of power to construct.
>
> From here, an argument must move on to tradeoffs between power to analyze and power to construct. Why wouldn’t you want to just side entirely with power to construct?
>
> ...


> ...
> Most arguments of the static typing apologist end up being arguments for the value of power to analyze.

Joseph Abrahamson, <!-- .element: class="haskell-talk-small" --> https://lobste.rs/s/l9foze/clojure_vs_static_typing_world#c_hjenxv <!-- .element: class="haskell-talk-small" -->



### What sucks about Haskell? (That I know about)

* Strings <!-- .element: class="fragment" data-fragment-index="1" -->
* Records <!-- .element: class="fragment" data-fragment-index="2" -->
* (Consistency of) Documentation <!-- .element: class="fragment" data-fragment-index="3" -->
* Laziness* <!-- .element: class="fragment" data-fragment-index="5" -->

(*Kinda sorta) <!-- .element: class="fragment" data-fragment-index="6" -->



### OMG Just show me some code already

![judge judy says hurry it up](https://i.imgur.com/XQzGQr5.gif)




### Values and Their Types

```Haskell
-- a typical type function definition with a type signature
foo :: (a -> b) -> a -> b
foo = id

-- 'a' and 'b' are type variables
-- '->' represents function application

-- a function from 'a' to 'b'
(a -> b)

-- and given an 'a', will return a 'b'
-> a -> b
```


### Values and Their Types

```Haskell
λ :type show
show :: Show a => a -> String
λ :t foo
foo :: (a -> b) -> a -> b
λ :t foo show
foo show :: Show a => a -> String
λ :t foo show 1
foo show 1 :: String
λ foo show 1
"1"
```


### Values and Their Types

```Haskell
-- The default strings are lists ([]) of Chars
λ :t "hi" :: [Char]
"hi" :: [Char] :: [Char]
λ :t "hi" ++ " world"
"hi" ++ " world" :: [Char]

-- We can use an infix operator in the prefix position using parens
-- (Note how we get partial application for free):
λ :t (++) "hi"
(++) "hi" :: [Char] -> [Char]
```


### Values and Their Types

```Haskell
λ :t (++)
-- a is a type variable - so this is a list ([]) of 'a'
(++) :: [a] -> [a] -> [a]

-- But look--once we apply a value, we force our 'a' to be a String!
λ :t (++) "hi"
(++) "hi" :: [Char] -> [Char]

-- ...but we can also do this, because all we need is a list of 'a':
λ :t (++) [1,2]
(++) [1,2] :: Num a => [a] -> [a]
λ (++) [1,2] [3]
[1,2,3]
λ
```



### Numbers

```Haskell
λ :type 1
1 :: Num p => p
λ :type 1 :: Int
1 :: Int :: Int
λ :t 2.45 :: Int

<interactive>:1:1: warning: [-Wdeferred-type-errors]
    • No instance for (Fractional Int) arising from the literal ‘2.45’
    • In the expression: 2.45 :: Int
2.45 :: Int :: Int
λ :t 2.45 :: Double
2.45 :: Double :: Double
λ 
```


### Strings

```Haskell
λ :t "foo"
"foo" :: [Char]
λ :set -XOverloadedStrings
λ :t "foo"
"foo" :: Data.String.IsString p => p
λ :t "foo" :: String
"foo" :: String :: String
λ :m +Data.Text
λ :t "foo" :: Text
"foo" :: Text :: Text
λ
```


### Lists and Tuples

```Haskell
λ :t [1,2,3]
[1,2,3] :: Num a => [a]
λ [1,2,3] !! 0
1
λ [1,2,3] !! 3
*** Exception: Prelude.!!: index too large
λ
λ :t (1,2)
(1,2) :: (Num b, Num a) => (a, b)
λ fst (1,2)
1
λ snd (1,2)
2
λ
```


### Algebraic Data Types

A Sum type (think "or")

```Haskell
data PossiblyAn a = ThisHereIsAn a | GotNada
  deriving (Show)

checkIt (ThisHereIsAn n) = "We got a " ++ show n
checkIt GotNada = "Oh sorry we got no values today"

λ checkIt $ ThisHereIsAn "Foo"
"We got a \"Foo\""
λ checkIt $ GotNada
"Oh sorry we got no values today"
```


### Algebraic Data Types

A Product type (think "and")

```Haskell
data Pair a b = MakePair a b deriving Show

firstInPair (MakePair x y) = x
secondInPair (MakePair x y) = y

λ firstInPair $ MakePair 1 2
1
λ secondInPair $ MakePair 1 2
2
λ
```


### Algebraic Data Types

Records

```Haskell
data Color = Red | Blue | Yellow | White | Black
  deriving (Show)

data Person = Person { name :: String
                     , age :: Int
                     , favoriteColor :: Color
                     }
  deriving (Show)

λ Person "Bob" 35 Blue
Person {name = "Bob", age = 35, favoriteColor = Blue}
λ
```



### Parametric Polymorphism

```Haskell
λ :t (++)
(++) :: [a] -> [a] -> [a]
λ :t id
id :: a -> a
λ :t const 
const :: a -> b -> a
λ 
```


### Ad-hoc Polymorphism: Type Classes

```Haskell
λ :t (+)
(+) :: Num a => a -> a -> a
λ 1.0 + 1.0
2.0
λ 1 + 1
2
λ :t 1.0 + 1.0
1.0 + 1.0 :: Fractional a => a
λ :t 1 + 1
1 + 1 :: Num a => a
λ
```


### Another Ad-hoc Polymorphism Example

```Haskell
λ :t (>)
(>) :: Ord a => a -> a -> Bool
λ :t (==)
(==) :: Eq a => a -> a -> Bool
λ data Food = Sushi | Pizza | TurkeySandwich | JustSomeCrackers
    | Natto | LiterallyJustPaper deriving (Show, Eq, Ord)
λ Pizza > LiterallyJustPaper
True
λ Pizza > JustSomeCrackers
True
λ Pizza > Sushi
False
```


### Another Ad-hoc Polymorphism Example

```Haskell
λ :t fmap
fmap :: Functor f => (a -> b) -> f a -> f b
λ fmap (+1) [1,2,3]
[2,3,4]
λ fmap (+1) $ Just 1
Just 2
λ fmap (+1) (1,2)
(1,3)
λ 
```


### Let's make our own typeclass

```Haskell
data PossiblyAn a = ThisHereIsAn a | GotNada

class ShowGoofy a where
  showGoofy :: a -> String

instance (Show a) => ShowGoofy (PossiblyAn a) where
  showGoofy (ThisHereIsAn a) = "Gwarsh you got an " ++ (show a) ++ "!"
  showGoofy GotNada = "Well shucks you got nothin'"

```


## Okay but what's all this monad stuff about!?


### Be patient, we're getting there

![patience](https://i.imgur.com/XSZ7kCr.gif)


### First of all, a quote

> In the end, however, a Functor is simply what it is defined to be; doubtless there are many examples of Functor instances that don’t exactly fit either of the above intuitions. The wise student will focus their attention on definitions and examples, without leaning too heavily on any particular metaphor. Intuition will come, in time, on its own. <!-- .element: class="haskell-talk-small" -->

-- Prof. Brent Yorgey, from the Typeclassopedia <!-- .element: class="haskell-talk-small" -->
https://wiki.haskell.org/Typeclassopedia




#### Functor, Monad, Applicative, Monad...

Haskell structures a lot of its core libraries around some basic algebraic structures. And to a large extent typeclasses are how these structures are defined and extended.

Typeclassopedia is the definitive reference: https://wiki.haskell.org/Typeclassopedia


#### Functor

Lifts a function into a computational context. Think `map`--the only reason that name wasn't used in Haskell is because it was already taken, for lists. Whoops.

```Haskell
λ :t fmap
fmap :: Functor f => (a -> b) -> f a -> f b
λ fmap (+1) [1,2,3]
[2,3,4]
λ fmap (+ 1) $ Just 1
Just 2
λ (fmap.fmap) (+1) [[1],[2],[3]]
[[2],[3],[4]]
λ (fmap.fmap.fmap) (+1) [[Just 1],[Just 2],[Just 3]]
[[Just 2],[Just 3],[Just 4]]
λ 
```


#### Applicative Functor

Adds some more structure on top of `Functor`. Apply functions _in a context_ to values _in a context_. `<*>` ("ap") and `pure` need to be defined for `Applicative`.

```Haskell
λ :t (<*>)
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
λ :t pure
pure :: Applicative f => a -> f a
λ [(+1)] <*> [1,2,3]
[2,3,4]
λ :t pure (+1) :: [Integer -> Integer]
pure (+1) :: [Integer -> Integer] :: [Integer -> Integer]
λ (pure (+1)) <*> [1,2,3]
[2,3,4]
λ pure 1 :: [Integer]
[1]
λ
```


#### Monad

Even more structure on top of `Functor` and `Applicative`--so still useful to think about a computational context here. For `Monad`, we must define `return` and `>>=` ("bind").

```Haskell
λ :t (>>=)
(>>=) :: Monad m => m a -> (a -> m b) -> m b
λ :t return
return :: Monad m => a -> m a
λ (Just 1) >>= (\x -> return (x + 1))
Just 2
λ [1,2,3] >>= (\x -> return (x + 1))
[2,3,4]
```


(...)

```Haskell
λ :t (\x -> return (x + 1))
(\x -> return (x + 1)) :: (Num a, Monad m) => a -> m a
λ :t (flip (>>=)) (\x -> return (x + 1))
(flip (>>=)) (\x -> return (x + 1))
  :: (Num b, Monad m) => m b -> m b
λ (flip (>>=)) (\x -> return (x + 1)) $ Just 1
Just 2
λ doIt v = do { x <- v; return $ x + 1 }
λ :t doIt
doIt :: (Monad m, Num b) => m b -> m b
λ doIt $ Just 1
Just 2
λ doIt $ [1,2,3]
[2,3,4]
```


Monad is famous 'cause it is the main mechanism Haskell provides for interacting with effects.

```Haskell
main :: IO ()
main = do
  putStrLn "What's your name?"
  name <- getLine
  putStrLn $ "hi " ++ name

-- equivalent to
altMain =
  putStrLn "What's your Name" >> getLine >>= \n -> putStrLn $ "hi " ++ n
```


#### Monad Transformers

A.k.a. "How do I compose these things?"

```Haskell
type MyMonad = StateT Integer IO

doStuff :: MyMonad String
doStuff = do
  liftIO $ putStrLn "What's your name?"
  name <- liftIO getLine
  modify (+1)
  liftIO $ putStrLn $ "hi " ++ name ++ ", what's your favorite color?"
  color <- liftIO getLine
  modify (+1)
  return $ "name: " ++ name ++ ", color: " ++ color
```



### Laziness in Haskell vs. Clojure

```Clojure
(defn test-lazy
  [b]
  (let [x (do (println "Calculating.") (+ 1 1))]
    (if b x 1)))

> (test-lazy true)
Calculating.
2
> (test-lazy false)
Calculating.
1
> 
```


```Haskell
testLazy b = let x = trace "Calculating." 1 + 1
             in if b then x else 1

λ testLazy True
Calculating.
2
λ testLazy False
1
λ
```



### Data Structures!

Built-ins:

```Haskell
-- Lists!
λ take 10 [1..]
[1,2,3,4,5,6,7,8,9,10]

-- Tuples!
λ true = fst (True,False)
λ false = snd (True,False)
```


### Data Structures!

https://hackage.haskell.org/package/containers

Graphs, sets, trees, maps, some specific variations for efficiency.

https://hackage.haskell.org/package/array
https://hackage.haskell.org/package/vector

Efficient mutable and immutable indexed arrays



### Kinds: Types of Types!

```Haskell
λ data Foo a b = Foo { runFoo :: a -> b }
λ :t Foo
Foo :: (a -> b) -> Foo a b
λ :k Foo
Foo :: * -> * -> *
λ :t Foo id
Foo id :: Foo b b
λ :t Foo const
Foo const :: Foo a (b -> a)
λ (runFoo $ Foo const) 1 2
1
λ 
```
