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
boundingM (TriMesh faces _) =
    let (minV, maxV) = minMaxFaces faces in
    BoundingBox minV maxV

-- Infinity
inf = 1/0

-- Min and Max vectors from all faces
minMaxFaces :: [(Face,Face)] -> (Vec3f, Vec3f)
minMaxFaces = foldl f ((Vec3f inf inf inf),(Vec3f (-inf) (-inf) (-inf))) where
    f (minV,maxV) ((Face a amb amc),_) =
        let (Vec3f minX minY minZ) = minV in
        let (Vec3f maxX maxY maxZ) = maxV in
        let (Vec3f ax ay az) = a in
        let (Vec3f bx by bz) = a-amb in
        let (Vec3f cx cy cz) = a-amc in
        let minX' = minimum [minX, ax, bx, cx] in
        let minY' = minimum [minY, ay, by, cy] in
        let minZ' = minimum [minZ, az, bz, cz] in
        let maxX' = maximum [maxX, ax, bx, cx] in
        let maxY' = maximum [maxY, ay, by, cy] in
        let maxZ' = maximum [maxZ, az, bz, cz] in
        ((Vec3f minX' minY' minZ'),(Vec3f maxX' maxY' maxZ'))

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
