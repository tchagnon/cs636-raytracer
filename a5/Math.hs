{- Math.hs
 - Direct implementation of Linear algebra routines.
 - Specific sizes (3x1,4x1,3x3,4x4) allow us to do fast analytical implementations.
 -
 - Timothy A. Chagnon
 - CS 636 - Spring 2009
 -}
module Math where

import Control.Parallel.Strategies hiding (dot)
import Control.DeepSeq
import Debug.Trace
tshow :: Show a => String -> a -> a
tshow x y = trace (x ++ " " ++ (show y)) y

-- Synonym for Real number type
type RealT = Double

-- Degrees to Radians
deg2rad :: RealT -> RealT
deg2rad d = d * pi / 180

epsilon :: RealT
epsilon = 1e-12

--------------------------------------------------------------------------------
-- 3x1 Vector
--------------------------------------------------------------------------------
data Vec3f = Vec3f !RealT !RealT !RealT
    deriving (Show, Eq)

-- Required for parListChunk
instance NFData Vec3f where
    rnf (Vec3f x y z) = rnf x `seq` rnf y `seq` rnf z

instance Num Vec3f where
    Vec3f a b c + Vec3f d e f   = Vec3f (a+d) (b+e) (c+f)
    Vec3f a b c - Vec3f d e f   = Vec3f (a-d) (b-e) (c-f)
    (*)                         = cross
    abs (Vec3f a b c)           = Vec3f (abs a) (abs b) (abs c)
    signum (Vec3f 0 0 0)        = zeroVec3f
    signum v                    = norm v
    fromInteger i               = Vec3f (fromInteger i) (fromInteger i) (fromInteger i)

vec3f = Vec3f
zeroVec3f :: Vec3f
zeroVec3f = Vec3f 0 0 0
svMul   :: RealT -> Vec3f -> Vec3f
svMul s (Vec3f a b c)             = Vec3f (s*a) (s*b) (s*c)
dot     :: Vec3f -> Vec3f -> RealT
dot (Vec3f a b c) (Vec3f d e f)   = a*d + b*e + c*f
cross   :: Vec3f -> Vec3f -> Vec3f
cross (Vec3f a b c) (Vec3f d e f) = Vec3f (b*f-c*e) (c*d-a*f) (a*e-b*d)
mag     :: Vec3f -> RealT
mag v                             = sqrt (magSq v)
magSq   :: Vec3f -> RealT
magSq (Vec3f a b c)               = (a*a + b*b + c*c)
norm    :: Vec3f -> Vec3f
norm v                            = (1/(mag v)) `svMul` v
(.*) :: Vec3f -> Vec3f -> Vec3f
Vec3f a b c .* Vec3f d e f        = Vec3f (a*d) (b*e) (c*f)

-- Reduce Vec4f to Vec3f, divide by w
point3f :: Vec4f -> Vec3f
point3f (Vec4f _ _ _ 0) = error "point3f divide by zero"
point3f (Vec4f x y z w) = Vec3f (x/w) (y/w) (z/w)

-- Reduce Vec4f to Vec3f, ignore w
direction3f :: Vec4f -> Vec3f
direction3f (Vec4f x y z _) = Vec3f x y z

-- Return elements as a 3-tuple
vec3fElts :: Vec3f -> (RealT, RealT, RealT)
vec3fElts (Vec3f x y z) = (x,y,z)

-- Dot product clamped to 0
dot0 :: Vec3f -> Vec3f -> RealT
dot0 v1 v2 = max 0 (dot v1 v2)

-- zipWith for Vec3f
zipWith3f :: (RealT -> RealT -> RealT) -> Vec3f -> Vec3f -> Vec3f
zipWith3f g (Vec3f a b c) (Vec3f d e f) = Vec3f (g a d) (g b e) (g c f)

-- Element extraction
v3x, v3y, v3z :: Vec3f -> RealT
v3x (Vec3f x _ _) = x
v3y (Vec3f _ y _) = y
v3z (Vec3f _ _ z) = z

--------------------------------------------------------------------------------
-- 4x1 Vector
--------------------------------------------------------------------------------
data Vec4f = Vec4f !RealT !RealT !RealT !RealT
    deriving (Show, Eq)
instance Num Vec4f where
    Vec4f a b c d + Vec4f e f g h   = Vec4f (a+e) (b+f) (c+g) (d+h)
    Vec4f a b c d - Vec4f e f g h   = Vec4f (a-e) (b-f) (c-g) (d-h)
    (*)                         = error "Vec4f multiplication not implemented"
    abs                         = norm4f
    signum (Vec4f 0 0 0 0)      = zeroVec4f
    signum v                    = norm4f v
    fromInteger i               = Vec4f (fromInteger i) (fromInteger i) (fromInteger i) (fromInteger i)

vec4f = Vec4f
zeroVec4f :: Vec4f
zeroVec4f = Vec4f 0 0 0 0
svMul4f   :: RealT -> Vec4f -> Vec4f
svMul4f s (Vec4f a b c d) = Vec4f (s*a) (s*b) (s*c) (s*d)
dot4f     :: Vec4f -> Vec4f -> RealT
dot4f (Vec4f a b c d) (Vec4f e f g h) = a*e + b*f + c*g + d*h
mag4f     :: Vec4f -> RealT
mag4f v = sqrt (magSq4f v)
magSq4f   :: Vec4f -> RealT
magSq4f (Vec4f a b c d) = (a*a + b*b + c*c + d*d)
norm4f    :: Vec4f -> Vec4f
norm4f v  = (1/(mag4f v)) `svMul4f` v

-- Raise Vec3f to Vec4f with w=1
point4f :: Vec3f -> Vec4f
point4f (Vec3f x y z) = Vec4f x y z 1

-- Raise Vec3f to Vec4f with w=0
direction4f :: Vec3f -> Vec4f
direction4f (Vec3f x y z) = Vec4f x y z 0

-- Indexing
(!.) :: Vec4f -> Int -> RealT
(Vec4f e0 e1 e2 e3) !. 0 = e0
(Vec4f e0 e1 e2 e3) !. 1 = e1
(Vec4f e0 e1 e2 e3) !. 2 = e2
(Vec4f e0 e1 e2 e3) !. 3 = e3
(Vec4f e0 e1 e2 e3) !. _ = error "Invalid index to (!) Mat4f"

-- Transform a Vec3f point using a Mat4f tranformation matrix
transformPt :: Mat4f -> Vec3f -> Vec3f
transformPt t v = point3f (t `mvMul` (point4f v))

-- Transform a Vec3f direction using a Mat4f tranformation matrix
transformDir :: Mat4f -> Vec3f -> Vec3f
transformDir t v = direction3f (t `mvMul` (direction4f v))

--------------------------------------------------------------------------------
-- 4x4 Matrix
--------------------------------------------------------------------------------
data Mat4f = Mat4f !Vec4f !Vec4f !Vec4f !Vec4f
    deriving (Show, Eq)
mat4f = Mat4f

-- Matrix-Vector Multiplication
mvMul     :: Mat4f -> Vec4f -> Vec4f
mvMul (Mat4f r1 r2 r3 r4) v = Vec4f (r1 `dot4f` v) (r2 `dot4f` v) (r3 `dot4f` v) (r4 `dot4f` v)

-- Matrix-Matrix Multiplication
mmMul     :: Mat4f -> Mat4f -> Mat4f
mmMul (Mat4f r1 r2 r3 r4) m2 = 
    let (Mat4f t1 t2 t3 t4) = transpose4f m2 in
    Mat4f (Vec4f (r1 `dot4f` t1) (r1 `dot4f` t2) (r1 `dot4f` t3) (r1 `dot4f` t4))
          (Vec4f (r2 `dot4f` t1) (r2 `dot4f` t2) (r2 `dot4f` t3) (r2 `dot4f` t4))
          (Vec4f (r3 `dot4f` t1) (r3 `dot4f` t2) (r3 `dot4f` t3) (r3 `dot4f` t4))
          (Vec4f (r4 `dot4f` t1) (r4 `dot4f` t2) (r4 `dot4f` t3) (r4 `dot4f` t4))

-- Matrix Transpose
transpose4f :: Mat4f -> Mat4f
transpose4f (Mat4f  (Vec4f r11 r12 r13 r14)
                    (Vec4f r21 r22 r23 r24)
                    (Vec4f r31 r32 r33 r34)
                    (Vec4f r41 r42 r43 r44)) =
            Mat4f   (Vec4f r11 r21 r31 r41)
                    (Vec4f r12 r22 r32 r42)
                    (Vec4f r13 r23 r33 r43)
                    (Vec4f r14 r24 r34 r44)

-- Identity Matrix
id4f :: Mat4f
id4f = Mat4f    (Vec4f 1 0 0 0)
                (Vec4f 0 1 0 0)
                (Vec4f 0 0 1 0)
                (Vec4f 0 0 0 1)

-- Indexing
(!|) :: Mat4f -> Int -> Vec4f
(Mat4f r0 r1 r2 r3) !| 0 = r0
(Mat4f r0 r1 r2 r3) !| 1 = r1
(Mat4f r0 r1 r2 r3) !| 2 = r2
(Mat4f r0 r1 r2 r3) !| 3 = r3
(Mat4f r0 r1 r2 r3) !| _ = error "Invalid index to (!!) Mat4f"

--------------------------------------------------------------------------------
-- 3x3 Column Matrix
--------------------------------------------------------------------------------
data ColMat3f = ColMat3f !Vec3f !Vec3f !Vec3f
    deriving (Show, Eq)
colMat3f = ColMat3f

-- Determinant of a 3x3 column matrix
detMat3f :: ColMat3f -> RealT
detMat3f (ColMat3f a b c) =
    let (Vec3f ax ay az) = a in
    let (Vec3f bx by bz) = b in
    let (Vec3f cx cy cz) = c in
    ax*(by*cz - cy*bz) + bx*(cy*az - ay*cz) + cx*(ay*bz - by*az)
