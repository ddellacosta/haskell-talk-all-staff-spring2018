{-# LANGUAGE DeriveFunctor #-}

module BasicTypes where

foo :: (a -> b) -> a -> b
foo = id

-- Sum type

data PossiblyAn a = ThisHereIsAn a | GotNada
  deriving (Show, Functor)

checkIt (ThisHereIsAn n) = "We got a " ++ show n
checkIt GotNada = "Oh sorry we got no values today"

-- Product type

data Pair a b = MakePair a b deriving Show

firstInPair (MakePair x y) = x
secondInPair (MakePair x y) = y

-- Record

data Color = Red | Blue | Yellow | White | Black
  deriving (Show)

data Person = Person { name :: String
                     , age :: Int
                     , favoriteColor :: Color
                     }
  deriving (Show)
