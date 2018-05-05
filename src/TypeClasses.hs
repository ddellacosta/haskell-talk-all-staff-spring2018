{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}

module TypeClasses where

import Data.Char


-- Example 1

data Circle = Circle { radius :: Double }
  deriving Show

data Rect = Rect { height :: Double
                 , width :: Double
                 }
  deriving Show

data Triangle = Triangle { base :: Double
                         , height :: Double
                         }
  deriving Show

class HasArea e where
  area :: e -> Double

instance HasArea Circle where
   area (Circle r) = (r ^ 2) * pi
 
instance HasArea Rect where
   area (Rect h w) = h * w

instance HasArea Triangle where
   area (Triangle b h) = 0.5 * b * h


-- Example 2

class Add a where
  add :: a -> a -> a

instance Add Int where
  add = (+)

instance Add Char where
  add n m = chr $ (ord n) + (ord m)

-- Doesn't work without FlexibleInstances extension
instance Add [Char] where
  add = (++)
