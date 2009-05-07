{- Bounding.hs
 - Bounding Volume Heirarchy
 -
 - Timothy A. Chagnon
 - CS 636 - Spring 2009
 -}
module Bounding where

import Primitive
import Mesh
import Math
import Ray

data Bounding = BoundingBox !Vec3f !Vec3f
    deriving (Show, Eq)

-- Construct a bounding volume from a primitive
boundingP :: Primitive -> Bounding
boundingP (Sphere r c) =
    let vecR = Vec3f r r r in
    BoundingBox (c-vecR) (c+vecR)
    
-- Construct a bounding volume from a Mesh
boundingM :: Mesh -> Bounding
boundingM (TriMesh faces _) = boundingF faces

-- Construct a bounding volume from a list of faces
boundingF :: [(Face,Face)] -> Bounding
boundingF faces =
    let (minV, maxV) = minMaxFaces faces in
    BoundingBox minV maxV

-- Get a bounding volume from the union of 2 other bounding volumes
boundUnion :: Bounding -> Bounding -> Bounding
boundUnion (BoundingBox minA maxA) (BoundingBox minB maxB) =
    let minV = zipWith3f min minA minB in
    let maxV = zipWith3f max maxA maxB in
    BoundingBox minV maxV

-- Infinity
inf = 1/0

-- Min and Max vectors from all faces
minMaxFaces :: [(Face,Face)] -> (Vec3f, Vec3f)
minMaxFaces = foldl f ((Vec3f inf inf inf),(Vec3f (-inf) (-inf) (-inf))) where
    f (minV,maxV) ((Face a amb amc),_) =
        let b       = a - amb in
        let c       = a - amc in
        let minV'   = foldl1 (zipWith3f min) [minV, a, b, c] in
        let maxV'   = foldl1 (zipWith3f max) [maxV, a, b, c] in
        (minV',maxV')

-- Determine if there is an intersection with the bounding box
intersectB :: Ray -> Bounding -> Bool
intersectB (Ray o d) (BoundingBox minV maxV) =
    let (Vec3f roX roY roZ) = o in
    let (Vec3f rdX rdY rdZ) = d in
    let (Vec3f x1  y1  z1)  = minV in
    let (Vec3f x2  y2  z2)  = maxV in
    let xs = (roX, rdX, x1, x2) in
    let ys = (roY, rdY, y1, y2) in
    let zs = (roZ, rdZ, z1, z2) in
    let dims = [xs, ys, zs] in
    if (or (map isParallelNonIsect dims)) then False
        else
            let (t1s, t2s) = unzip (map getTs dims) in
            let tNear = maximum t1s in
            let tFar = minimum t2s in
            if (tNear > tFar) || (tFar < 0)
                then False
                else True

isParallelNonIsect :: (RealT, RealT, RealT, RealT) -> Bool
isParallelNonIsect (roX, rdX, x1, x2) = (rdX == 0) && ((roX < x1 || roX > x2))

getTs :: (RealT, RealT, RealT, RealT) -> (RealT, RealT)
getTs (roX, rdX, x1, x2) =
    let t1 = (x1-roX)/rdX in
    let t2 = (x2-roX)/rdX in
    if t1 > t2
        then (t2, t1)
        else (t1, t2)

