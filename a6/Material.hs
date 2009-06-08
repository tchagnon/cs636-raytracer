{- Material.hs
 - Materials
 -
 - Timothy A. Chagnon
 - CS 636 - Spring 2009
 -}
module Material where

import Math
import Color

data Material =
    PhongMaterial {
        kd      :: RealT,
        ks      :: RealT,
        ka      :: RealT,
        kt      :: RealT,
        n       :: RealT,
        m       :: RealT,
        cs      :: Color,
        cd      :: Vec3f -> Color,
        ct      :: Color,
        eta     :: RealT
    }
instance Show Material where
    show mat = "PhongMaterial" ++ (concatMap show (map ($mat) [kd, ks, ka, kt, n, m, eta]))
instance Eq Material where
    x == y =
        let vars = [kd, ks, ka, kt, n, m, eta] in
        and (zipWith (==) (map ($x) vars) (map ($y) vars))
