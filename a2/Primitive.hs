{- Primitive.hs
 - Primitive shapes.
 -
 - Timothy A. Chagnon
 - CS 636 - Spring 2009
 -}
module Primitive where

import Math
import Ray
import Material

data Primitive  = Sphere RealT Vec3f
    deriving (Show, Eq)

-- Sphere centered at the origin
sphere :: RealT -> Primitive
sphere r = Sphere r zeroVec3f

-- Intersect a ray with primitives
intersectP :: Ray -> Material -> Primitive -> [Intersection]
intersectP (Ray o d) mat (Sphere r ctr) =
    let b           = 2 * (d `dot` (o-ctr)) in
    let c           = (magSq (o-ctr)) - r*r in
    let discrim     = b*b - 4*c in
    let normal t    = (1/r) `svMul` ((o-ctr) + (t `svMul` d)) in
    if discrim < 0
        then []
        else
            let t0 = (-b - (sqrt discrim))/2 in
            let t1 = (-b + (sqrt discrim))/2 in
            if t0 < 0
                then if t1 < 0
                    then []
                    else [(Inx t1 (normal t1) mat)]
                else [(Inx t0 (normal t0) mat), (Inx t1 (normal t1) mat)]

-- Transform Primitives
transformP :: Mat4f -> Primitive -> Primitive
transformP t (Sphere r c) =
    let r' = r * (t!|0!.0) in
    let c' = transformPt t c in
    Sphere r' c'
