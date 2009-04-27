{- scene1.hs
 - ...
 -
 - Timothy A. Chagnon
 - CS 636 - Spring 2009
 -}

import RayTracer

main = rayTracer scene

scene =
    Scene {
        outfile         = "scene1.ppm",
        width           = 100,
        height          = 100,
--        width           = 512,
--        height          = 512,
        superSample     = 1,
        background      = black,
        ambientLight    = white,
        defaultMaterial = mat0,
        camera  =
            Camera {
                dist        = 5,
                fovAngle    = 45,
                location    = vec3f 0 3 5,
                direction   = norm (vec3f 0 (-0.5) (-1)),
                up          = norm (vec3f 0 1 (-0.5))
            },
        lights = [lightFire, light1],
        objects =
            Group [
                Group [
                    Transform (translate (vec3f x 0 z)) (Primitive (sphere 0.05))
                    | x <- [-5..5], z <- [-20..0]],
                Material matBlue (
                    Transform (translate (vec3f 1.5 0.5 1.5)) (
                    Transform (rotate (-45) (vec3f 0 1 0)) (
                        LoadMesh "../models/bound-bunny_200.smf" SmoothShade
                    ))
                ),
                Material matFire (
                    Transform (translate (vec3f 0.3 0 0)) (
                    Transform (scale (vec3f 0.007 0.007 0.007)) (
                    Transform (rotate (-90) (vec3f 1 0 0)) (
                        LoadMesh "../models/campfire.smf" SmoothShade
                    )))
                )
            ]
    }

mat0 =
    PhongMaterial {
        kd = 0.7,
        ks = 0.3,
        ka = 0.1,
        n  = 30,
        c  = white
    }

matBlue     = mat0 {c = cornflowerBlue}
matFire     =
    PhongMaterial {
        kd = 0.8,
        ks = 0.2,
        ka = 0.5,
        n  = 5,
        c  = orange
    }

lightFire =
    PointLight {
        color = white, --1.4 `svMul` orange,
        position = vec3f 0 2 0
    }

light1 =
    PointLight {
        color = 0.1 `svMul` white,
        position = vec3f 4 4 9
    }
