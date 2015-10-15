{- scene0.hs
 - Scene given on Assignment 1 webpage, 2 spheres and bunny
 -
 - Timothy A. Chagnon
 - CS 636 - Spring 2009
 -}

import RayTracer

main = rayTracer scene0

scene0 =
    Scene {
        outfile         = "scene0.ppm",
        width           = 512,
        height          = 512,
        superSample     = 2,
        outputSampleMap = False,
        sampleThreshold = vec3f 0.05 0.05 0.05,
        reflectionDepth = 8,
        background      = black,
        ambientLight    = white,
        defaultMaterial = mat0,
        camera  =
            Camera {
                dist        = 8,
                fovAngle    = 45,
                location    = vec3f 0 3.0 8,
                direction   = vec3f 0 0 (-1),
                up          = vec3f 0 1 0
            },
        lights = [light0],
        objects =
            Group [
                Material mat0 (
                    Primitive (Plane (vec3f 0 0 0) (vec3f 0 0 1) (vec3f 1 0 0))
                ),
                Material matTransparent (
                    Transform (translate (vec3f (-1.0) 1.5 (-0.5))) (
                        Primitive (sphere 1.5)
                    )
                ),
                Material matTeapot (
                    Transform (translate (vec3f (1.5) 0 (-2.0))) (
                    Transform (scale (vec3f 0.75 0.75 0.75)) (
                    Transform (rotate (-45) (vec3f 0 1 0)) (
                        LoadMesh "models/teapot.smf" SmoothShade
                    )))
                ),
                Material matCow (
                    Transform (translate (vec3f (0.0) 0.0 (-10.0))) (
                    Transform (scale (vec3f 15 15 15)) (
                    Transform (rotate (-45) (vec3f 0 1 0)) (
                        LoadMesh "models/bound-cow.smf" SmoothShade
                    )))
                )
            ]
    }

ball = Primitive (sphere 0.4)
trefoilPos :: RealT -> Vec3f
trefoilPos t = vec3f ((sin t) + 2*(sin (2*t))) (-((cos t) - 2 * (cos (2*t)))) (sin (3*t))
trefoil = Group [ Transform (translate (trefoilPos t)) ball
                | t <- [0, pi/20  .. 2*pi]
                ]

mat0 =
    PhongMaterial {
        kd = 0.6,
        ks = 0.3,
        ka = 0.1,
        kt = 0.0,
        n  = 30,
        m  = 30,
        cs = white,
        cd = \(Vec3f x y z) -> if ((floor x)+(floor z)) `rem` 2 == 0 then cornflowerBlue else white,
        ct = white,
        eta = etaGlass
    }

matTransparent =
    PhongMaterial {
        kd = 0.01,
        ks = 0.1,
        ka = 0.0,
        kt = 0.9,
        n  = 30,
        m  = 10,
        cs = white,
        cd = const white,
        ct = white,
        eta = etaGlass
    }


matTeapot =
    PhongMaterial {
        kd = 0.2,
        ks = 0.1,
        ka = 0.0,
        kt = 0.7,
        n  = 30,
        m  = 10,
        cs = white,
        cd = const orange,
        ct = white,
        eta = etaGlass
    }

matCow     =
    PhongMaterial {
        kd = 0.4,
        ks = 0.2,
        ka = 0.4,
        kt = 0.0,
        n  = 5,
        m  = 5,
        cs = darkBrown,
        cd = const darkBrown,
        ct = white,
        eta = etaGlass
    }

light0 =
    PointLight {
        color = white,
        position = vec3f 4 4 9
    }
