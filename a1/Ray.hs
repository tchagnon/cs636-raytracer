{- Ray.hs
 - Ray type
 -
 - Timothy A. Chagnon
 - CS 636 - Spring 2009
 -}
module Ray where

import Math

data Ray = Ray !Vec3f !Vec3f
    deriving Show

type Intersection = RealT

-- Transform the ray to object space
transformR :: Mat4f -> Ray -> Ray
transformR m (Ray o d) =
    (Ray (point3f (m `mvMul` (point4f o)))
         (norm (direction3f (m `mvMul` (direction4f d)))))

