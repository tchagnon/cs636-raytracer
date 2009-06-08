{- Mesh.hs
 - Triangle Mesh model.
 -
 - Timothy A. Chagnon
 - CS 636 - Spring 2009
 -}
module Mesh where

import Data.Array
import Data.List
import Data.Function
import Math
import Ray
import Material

import Debug.Trace

data Mesh = TriMesh ![(Face, Face)] MeshShading
    deriving (Show, Eq)

-- Face has       a      (a-b)  (a-c)
data Face = Face !Vec3f !Vec3f !Vec3f
    deriving (Show, Eq)

type VecArray = Array Int Vec3f

data MeshShading = FlatShade | SmoothShade
    deriving (Show, Eq)

-- Load mesh
loadSMF :: String -> MeshShading -> IO Mesh
loadSMF file shading = do
    smfString <- readFile file
    let mesh = readSMF smfString shading
    return mesh

-- Parse an SMF string to Mesh
readSMF :: String -> MeshShading -> Mesh
readSMF smfString shading =
    let ls                      = lines smfString in
    let ls'                     = filter ((/=) []) ls in
    let vs                      = filter ((==) 'v' . head) ls' in
    let fs                      = filter ((==) 'f' . head) ls' in
    let vertVecs                = map readVertex vs in
    let numVerts                = length vertVecs in
    let vertArray               = listArray (0, numVerts) vertVecs in
    let faceIndices             = map readFaceIdx fs in
    let faces                   = map (lookupFace vertArray) faceIndices in
    let faceNorms               = map faceNormal faces in
    let normLists               = collectNorms numVerts faceIndices faceNorms in
    let normArray               = listArray (0, numVerts) (map avgVecs normLists) in
    let normals                 = map (lookupNormal normArray) faceIndices in
    TriMesh (zip faces normals) shading

-- Convert an SMF vertex string to Vec3f
readVertex :: String -> Vec3f
readVertex =
    let readVert                = (map read) . (tail . words) in
    let list2vec3f (x:y:z:_)    = vec3f x y z in
    list2vec3f . readVert

-- Convert an SMF face string to Face
readFaceIdx :: String -> (Int,Int,Int)
readFaceIdx s =
    let readFaceList    = (map ((-1 +) . read)) . (tail . words) in
    let (ai:bi:ci:_)    = readFaceList s in
    (ai, bi, ci)

-- Take a list of Vertex indices and produce a Face
lookupFace :: VecArray -> (Int,Int,Int) -> Face
lookupFace verts (ai, bi, ci)    =
    let a      = verts ! ai in
    let b      = verts ! bi in
    let c      = verts ! ci in
    let amb    = (a-b) in
    let amc    = (a-c) in
    Face a amb amc

-- Get the normal for the whole face
faceNormal :: Face -> Vec3f
faceNormal (Face a amb amc) = norm (amb `cross` amc)

-- Take a list of Vertex indices and produce a Normal Face (normal for each vertex)
lookupNormal :: VecArray -> (Int,Int,Int) -> Face
lookupNormal norms (ai, bi, ci)    =
    let a      = norms ! ai in
    let b      = norms ! bi in
    let c      = norms ! ci in
    Face a b c

-- Gather Normals into lists associated with each of n verticesj
collectNorms :: Int -> [(Int,Int,Int)] -> [Vec3f] -> [[Vec3f]]
collectNorms numVerts indices norms =
    let splitIdx (a,b,c) n      = [(a,n),(b,n),(c,n)] in
    let sortByIdx               = sortBy (compare `on` fst) in
    let groupByIdx              = groupBy ((==) `on` fst) in
    let removeIdx               = map (map snd) in
    let fillIn                  = fillGaps [0..(numVerts-1)] in
    let idxNorms                = concat (zipWith splitIdx indices norms) in
    let norms                   = removeIdx $ groupByIdx $ fillIn $ sortByIdx $ idxNorms in
    norms

-- Fill gaps where there is no norm associated with a vertex
fillGaps :: [Int] -> [(Int,Vec3f)] -> [(Int,Vec3f)]
fillGaps [] _ = []
fillGaps (x:xs) [] = (x,(Vec3f 1 0 0)):(fillGaps xs [])
fillGaps (x:xs) ((i,v):vs)
    | x < i     = (x,(Vec3f 1 0 0)):(fillGaps xs ((i,v):vs))
    | x == i    = (i,v):(fillGaps xs vs)
    | x > i     = (i,v):(fillGaps (x:xs) vs)

-- Average vectors
avgVecs :: [Vec3f] -> Vec3f
avgVecs vs = norm ((1/(fromIntegral $ length vs)) `svMul` (foldl1 (+) vs))

-- Intersect a ray with a mesh model
intersectM :: Ray -> Material -> Mesh -> [Intersection]
intersectM (Ray r d eta) mat (TriMesh [] _) = []
intersectM (Ray r d eta) mat (TriMesh ((face,norms):rest) shading) =
    let (Face a amb amc)   = face in
    let (Face na nb nc)    = norms in
    let amr                = a-r in
    let mtxA               = colMat3f amb amc d in
    let detA               = detMat3f mtxA in
    let beta               = (detMat3f (colMat3f amr amc d))   / detA in
    let gamma              = (detMat3f (colMat3f amb amr d))   / detA in
    let alpha              = 1 - beta - gamma in
    let t                  = (detMat3f (colMat3f amb amc amr)) / detA in
    let normal             = interpNorm shading face norms alpha beta gamma in
    let recurse            = intersectM (Ray r d eta) mat (TriMesh rest shading) in
    if beta >= 0 && gamma >= 0 && (beta + gamma) <= 1 && t >= 0
        then (Inx t normal mat):recurse
        else recurse

-- Interpolate Normal
interpNorm :: MeshShading -> Face -> Face -> RealT -> RealT -> RealT -> Vec3f
interpNorm FlatShade   (Face a amb amc) (Face na nb nc) alpha beta gamma =
    --norm (amb `cross` amc)
    avgVecs [na, nb, nc]
interpNorm SmoothShade (Face a amb amc) (Face na nb nc) alpha beta gamma =
    norm ((alpha `svMul` na) + (beta `svMul` nb) + (gamma `svMul` nc))

-- Transform the mesh
transformM :: Mat4f -> Mesh -> Mesh
transformM t (TriMesh faces s) = TriMesh (map (transformF t) faces) s

-- Transform a face
transformF :: Mat4f -> (Face, Face) -> (Face, Face)
transformF t ((Face a amb amc), (Face na nb nc)) =
    let tPt  = transformPt  t in
    let tDir = transformDir t in
    (Face (tPt a)   (tDir amb) (tDir amc),
     Face (norm (tDir na)) (norm (tDir nb))  (norm (tDir nc)))
