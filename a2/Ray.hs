{- Ray.hs
 - Ray type
 -
 - Timothy A. Chagnon
 - CS 636 - Spring 2009
 -}
module Ray where

import Math
import Material

data Ray = Ray Vec3f Vec3f
    deriving Show

--                      t     Normal Material
data Intersection = Inx RealT Material
    deriving (Show, Eq)
instance Ord Intersection where
    compare (Inx t0 _) (Inx t1 _) = compare t0 t1

