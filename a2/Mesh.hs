{- Mesh.hs
 - Triangle Mesh model.
 -
 - Timothy A. Chagnon
 - CS 636 - Spring 2009
 -}
module Mesh where

import Data.Array
import Math
import Ray

data Mesh = TriMesh ![Face]
    deriving (Show, Eq)

-- Face has       a     (a-b)  (a-c)
data Face = Face !Vec3f !Vec3f !Vec3f
    deriving (Show, Eq)

type VecArray = Array Int Vec3f

-- Load mesh
loadSMF :: String -> IO Mesh
loadSMF file = do
    smfString <- readFile file
    let mesh = readSMF smfString
    return mesh

-- Parse an SMF string to Mesh
readSMF :: String -> Mesh
readSMF smfString =
    let ls                      = lines smfString in
    let ls'                     = filter ((/=) []) ls in
    let vs                      = filter ((==) 'v' . head) ls' in
    let fs                      = filter ((==) 'f' . head) ls' in
    let vertVecs                = map readVertex vs in
    let vertArray               = listArray (0, (length vertVecs)) vertVecs in
    let faces                   = map (readFace vertArray) fs in
    TriMesh faces

-- Convert an SMF vertex string to Vec3f
readVertex :: String -> Vec3f
readVertex =
    let readVert                = (map read) . (tail . words) in
    let list2vec3f (x:y:z:_)    = vec3f x y z in
    list2vec3f . readVert

-- Convert an SMF face string to Face
--readFace :: String -> Face
readFace :: VecArray -> String -> Face
readFace verts =
    let readFaceList            = (map ((-1 +) . read)) . (tail . words) in
    (list2face verts) . readFaceList

-- Take a list of Vertex indices and produce a Face
list2face :: VecArray -> [Int] -> Face
list2face verts (ai:bi:ci:_)    =
    let a = verts ! ai in
    let b = verts ! bi in
    let c = verts ! ci in
    Face a (a-b) (a-c)

-- Intersect a ray with a mesh model
intersectM :: Ray -> Mesh -> [Intersection]
intersectM (Ray r d) (TriMesh []) = []
intersectM (Ray r d) (TriMesh (face:rest)) =
    let (Face a amb amc) = face in
    let amr              = a-r in
    let mtxA             = colMat3f amb amc d in
    let detA             = detMat3f mtxA in
    let beta             = (detMat3f (colMat3f amr amc d))   / detA in
    let gamma            = (detMat3f (colMat3f amb amr d))   / detA in
    let t                = (detMat3f (colMat3f amb amc amr)) / detA in
    let recurse          = intersectM (Ray r d) (TriMesh rest) in
    if beta >= 0 && gamma >= 0 && (beta + gamma) <= 1 && t >= 0
        then t:recurse
        else recurse

