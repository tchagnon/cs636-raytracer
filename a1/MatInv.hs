{-# LANGUAGE CPP #-}
{- MatInv.hs
 - 4x4 Matrix invert function.
 - Direct implementation obtained from Maple's CodeGeneration
 -
 - Timothy A. Chagnon
 - CS 636 - Spring 2009
 -}
module MatInv where

#ifdef USING_HMATRIX
import MathHmatrix
import Numeric.LinearAlgebra
inverse4f :: Mat4f -> Mat4f
inverse4f = inv

#else
import MathDirect

-- Analytical solution via Maple (see minv.mw)
inverse4f :: Mat4f -> Mat4f
inverse4f   (Mat4f  (Vec4f m11 m12 m13 m14)
                    (Vec4f m21 m22 m23 m24)
                    (Vec4f m31 m32 m33 m34)
                    (Vec4f m41 m42 m43 m44)) =
    let t1 = m22 * m43 in
    let t3 = m22 * m44 in
    let t5 = m42 * m24 in
    let t7 = m44 * m32 in
    let t9 = m43 * m32 in
    let t11 = m42 * m23 in
    let t14 = m11 * m22 in
    let t15 = m43 * m34 in
    let t17 = m44 * m33 in
    let t19 = m11 * m42 in
    let t20 = m24 * m33 in
    let t22 = m11 * m44 in
    let t23 = m32 * m23 in
    let t25 = m11 * m43 in
    let t26 = m32 * m24 in
    let t28 = m23 * m34 in
    let t30 = m31 * m14 in
    let t32 = m22 * m41 in
    let t33 = m13 * m34 in
    let t35 = m14 * m33 in
    let t37 = m31 * m13 in
    let t39 = m43 * m31 in
    let t40 = m12 * m24 in
    let t42 = m21 * m12 in
    let t44 = -t14 * t15 + t14 * t17 - t19 * t20 - t22 * t23 + t25 * t26 + t19 * t28 + t1 * t30 + t32 * t33 - t32 * t35 - t3 * t37 - t39 * t40 - t17 * t42 in
    let t46 = m42 * m21 in
    let t50 = m41 * m12 in
    let t52 = m21 * m13 in
    let t54 = m41 * m14 in
    let t57 = m44 * m31 in
    let t58 = m12 * m23 in
    let t60 = m41 * m13 in
    let t62 = m21 * m14 in
    let t65 = t5 * t37 - t46 * t33 + t46 * t35 + t15 * t42 + t50 * t20 + t7 * t52 + t54 * t23 - t50 * t28 + t57 * t58 - t60 * t26 - t9 * t62 - t11 * t30 in
    let t67 = 1.0 / (t44 + t65) in
    let t121 = m44 * m21 in
    let t125 = m43 * m21 in
    let t146 = m41 * m32 in
    let t152 = m42 * m31 in
    let t172 = m32 * m11 in
    let t174 = m32 * m21 in
    let t176 = m31 * m12 in
    let r00 = (-t1 * m34 + t3 * m33 - t5 * m33 - t7 * m23 + t9 * m24 + t11 * m34) * t67 in
    let r01 = -(t17 * m12 + m42 * m13 * m34 - m42 * m14 * m33 - t15 * m12 - t7 * m13 + t9 * m14) * t67 in
    let r02 = (t1 * m14 - t3 * m13 - m43 * m12 * m24 + t5 * m13 + m44 * m12 * m23 - t11 * m14) * t67 in
    let r03 = -(-m22 * m13 * m34 + m22 * m14 * m33 - t40 * m33 - m14 * m32 * m23 + t58 * m34 + m13 * m32 * m24) * t67 in
    let r10 = -(t17 * m21 + m41 * m23 * m34 - t15 * m21 - t57 * m23 + t39 * m24 - m41 * m24 * m33) * t67 in
    let r11 = (-t25 * m34 + t22 * m33 + t39 * m14 + t60 * m34 - t54 * m33 - t57 * m13) * t67 in
    let r12 = -(t22 * m23 - t25 * m24 - t121 * m13 - t54 * m23 + t60 * m24 + t125 * m14) * t67 in
    let r13 = (-m11 * m24 * m33 + m11 * m23 * m34 + m24 * m31 * m13 - t52 * m34 + t62 * m33 - m23 * m31 * m14) * t67 in
    let r20 = (t32 * m34 - t3 * m31 + t5 * m31 - t46 * m34 + t7 * m21 - t146 * m24) * t67 in
    let r21 = -(t22 * m32 - t19 * m34 + t152 * m14 + t50 * m34 - t54 * m32 - t57 * m12) * t67 in
    let r22 = (t22 * m22 - t121 * m12 - t54 * m22 - t19 * m24 + t46 * m14 + t50 * m24) * t67 in
    let r23 = -(m34 * m11 * m22 - m34 * m21 * m12 - t30 * m22 - t172 * m24 + t174 * m14 + t176 * m24) * t67 in
    let r30 = -(-t1 * m31 + t32 * m33 - t46 * m33 + t11 * m31 - t146 * m23 + t9 * m21) * t67 in
    let r31 = (-t19 * m33 + t25 * m32 - t39 * m12 + t152 * m13 + t50 * m33 - t60 * m32) * t67 in
    let r32 = -(t25 * m22 - t125 * m12 - t60 * m22 - t19 * m23 + t46 * m13 + t50 * m23) * t67 in
    let r33 = t67 * (m33 * m11 * m22 - m33 * m21 * m12 - t37 * m22 - t172 * m23 + t174 * m13 + t176 * m23) in
    Mat4f   (Vec4f r00 r01 r02 r03)
            (Vec4f r10 r11 r12 r13)
            (Vec4f r20 r21 r22 r23)
            (Vec4f r30 r31 r32 r33)

#endif
