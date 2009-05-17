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
        n       :: RealT,
        c       :: Color
    }
    deriving (Show, Eq)
