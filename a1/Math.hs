{-# LANGUAGE CPP #-}
{- Math.hs
 - Point of switching between MathDirect and MathHmatrix
 -
 - Timothy A. Chagnon
 - CS 636 - Spring 2009
 -}
module Math (
#ifdef USING_HMATRIX
    module MathHmatrix,
#else
    module MathDirect,
#endif

    module MatInv
    ) where

#ifdef USING_HMATRIX
import MathHmatrix
#else
import MathDirect
#endif

import MatInv

