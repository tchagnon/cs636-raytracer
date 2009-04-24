{- MathHmatrix.hs
 - Mapping of Linear algebra routines via the hMatrix library.
 - hMatrix uses GSL and a BLAS implementation such as ATLAS.
 -
 - Timothy A. Chagnon
 - CS 636 - Spring 2009
 -}
module MathHmatrix where

import Numeric.LinearAlgebra

deg2rad :: RealT -> RealT
deg2rad d = d * pi / 180

type RealT = Double
type Vec3f = Vector Double

vec3f :: Double -> Double -> Double -> Vector Double
vec3f x y z = (3 |>) [x,y,z]

zeroVec3f :: Vec3f
zeroVec3f = vec3f 0 0 0
svMul   :: Double -> Vec3f -> Vec3f
svMul = scale
dot     :: Vec3f -> Vec3f -> Double
dot  = Numeric.LinearAlgebra.dot
cross   :: Vec3f -> Vec3f -> Vec3f
cross v1 v2 =
    let [a, b, c] = toList v1 in
    let [d, e, f] = toList v2 in
    vec3f (b*f-c*e) (c*d-a*f) (a*e-b*d)

mag     :: Vec3f -> Double
mag v   = sqrt (magSq v)
magSq   :: Vec3f -> Double
magSq v = v `Numeric.LinearAlgebra.dot` v
norm    :: Vec3f -> Vec3f
norm v  = (1/(mag v)) `svMul` v

type Mat4f = Matrix Double
mat4f :: Vec4f -> Vec4f -> Vec4f -> Vec4f -> Mat4f
mat4f a b c d = fromRows [a,b,c,d]

type Vec4f = Vector Double
vec4f :: Double -> Double -> Double -> Double -> Vector Double
vec4f x y z w = (4 |>) [x,y,z,w]

mvMul     :: Mat4f -> Vec4f -> Vec4f
mvMul  = (<>)
mmMul     :: Mat4f -> Mat4f -> Mat4f
mmMul  = (<>)

id4f :: Mat4f
id4f = ident 4

point4f :: Vec3f -> Vec4f
point4f v = 4 |> ((toList v) ++ [1.0])

point3f :: Vec4f -> Vec3f
point3f v =
    let [x, y, z, w] = toList v in
    if w == 0
        then error "point3f divide by zero"
        else vec3f (x/w) (y/w) (z/w)

direction4f :: Vec3f -> Vec4f
direction4f v = 4 |> ((toList v) ++ [0.0])

direction3f :: Vec4f -> Vec3f
direction3f = subVector 0 3

vec3fElts :: Vec3f -> (RealT, RealT, RealT)
vec3fElts v =
    let [x,y,z] = toList v in
    (x,y,z)

type ColMat3f = Matrix Double
colMat3f :: Vec3f -> Vec3f -> Vec3f -> ColMat3f
colMat3f a b c = fromColumns [a,b,c]

detMat3f :: ColMat3f -> RealT
detMat3f = det
