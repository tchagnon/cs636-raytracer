{- Primitive.hs
 - Primitive shapes.
 -
 - Timothy A. Chagnon
 - CS 636 - Spring 2009
 -}
module Primitive where

import Math
import Ray

data Primitive  = Sphere RealT
    deriving (Show, Eq)

-- Intersect a ray with primitives
intersectP :: Ray -> Primitive -> [Intersection]
intersectP (Ray o d) (Sphere r) =
    let b           = 2 * (d `dot` o) in
    let c           = (magSq o) - r^2 in
    let discrim     = b^2 - 4*c in
    if discrim < 0
        then []
        else
            let t0 = (-b - (sqrt discrim))/2 in
            let t1 = (-b + (sqrt discrim))/2 in
            if t0 < 0
                then if t1 < 0
                    then []
                    else [t1]
                else [t0, t1]


