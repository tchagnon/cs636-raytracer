{- Object.hs
 - Heirarchical object tree
 -
 - Timothy A. Chagnon
 - CS 636 - Spring 2009
 -}
module Object where

import Data.List(sortBy)
import Data.Function(on)
import Math
import Ray
import Primitive
import Mesh
import Material
import Bounding

facesThreshold = 50

data ObjectTree = Group [ObjectTree]
                | Transform Mat4f ObjectTree
                | Material Material ObjectTree
                | Primitive Primitive
                | Mesh Mesh
                | LoadMesh String MeshShading
                | Bounding Bounding ObjectTree
                | Empty
    deriving Show

-- Load object tree
loadObjs :: ObjectTree -> IO ObjectTree
loadObjs (Group objs)      = do
    objs' <- mapM loadObjs objs
    return (Group objs')
loadObjs (Transform t obj) = do
    obj' <- loadObjs obj
    return (Transform t obj')
loadObjs (Material m obj)  = do
    obj' <- loadObjs obj
    return (Material m obj')
loadObjs (Primitive p)   = return (Primitive p)
loadObjs (LoadMesh file shading) = do
    mesh <- loadSMF file shading
    return (Mesh mesh)
loadObjs (Mesh m)      = error "loadObjs undexpected Mesh"

-- Push transforms down to primitives
prepObjs :: Mat4f -> ObjectTree -> ObjectTree
prepObjs t (Group objs)        = Group (map (prepObjs t) objs)
prepObjs t (Transform t2 objs) = prepObjs (t `mmMul` t2) objs
prepObjs t (Primitive p)       = Primitive (transformP t p)
prepObjs t (Mesh m)            = Mesh (transformM t m)
prepObjs t (Material m objs)   = Material m (prepObjs t objs)
prepObjs _ o                   = error $ "Undefined prepObjs on object: " ++ (show o)

-- Traverse the Object tree looking for intersections
intersect :: Ray -> Material -> ObjectTree -> [Intersection]
intersect r mat (Group objs)        = concatMap (intersect r mat) objs
intersect r mat (Primitive p)       = intersectP r mat p
intersect r mat (Mesh m)            = intersectM r mat m
intersect r mat (Material mat' obj) = intersect r mat' obj
intersect r mat (Bounding b obj)    = if (intersectB r b) then intersect r mat obj else []
intersect r mat Empty               = []
intersect _ _   o                   = error $ "Undefined intersect on object: " ++ (show o)

-- Compute Bounding Volume Heirarchy
constructBVH :: Int -> ObjectTree -> ObjectTree
constructBVH t (Group objs)        = Group (map (constructBVH t) objs)
constructBVH t (Primitive p)       = Bounding (boundingP p) (Primitive p)
constructBVH t (Mesh m)            = dissectMesh t m
constructBVH t (Material mat' obj) = Material mat' (constructBVH t obj)
constructBVH t o                   = error $ "Undefined constructBVH on object: " ++ (show o)

-- Break the mesh up into a tree of bounding boxes
dissectMesh :: Int -> Mesh -> ObjectTree
dissectMesh faceThreshold (TriMesh faces shading) = dissect (cycle [v3x, v3y, v3z]) faces where
    dissect (dimF:dims) faces =
        if (length faces) < faceThreshold
            then Bounding (boundingF faces) (Mesh (TriMesh faces shading))
            else
                let aFace (Face a amb amc) = a in
                let sorted           = sortBy (compare `on` (dimF.aFace.fst)) faces in
                let n                = length sorted in
                let (lo, hi)         = splitAt (n `div` 2) sorted in
                let loBounding       = dissect dims lo in
                let hiBounding       = dissect dims hi in
                let (Bounding loB _) = loBounding in
                let (Bounding hiB _) = hiBounding in
                let uB               = boundUnion loB hiB in
                (Bounding uB (Group [loBounding, hiBounding]))
