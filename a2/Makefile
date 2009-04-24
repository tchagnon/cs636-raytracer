SCENES = scene0 teapot
IMAGES = $(addsuffix .ppm,$(SCENES))
DEPENDS = Color Image Material Math Mesh Primitive Ray RayTracer Scene
GHCARGS = -threaded #-prof -auto-all #-DUSING_HMATRIX
GHCOPT = -O2 -funbox-strict-fields -fexcess-precision -funfolding-keeness-factor=10 $(VIAC)
VIAC = -fvia-C -optc-O3 -optc-msse3 -optc-ffast-math
THREADS = 2

all: $(SCENES)

%: %.hs  $(addsuffix .hs,$(DEPENDS))
	ghc $(GHCARGS) $(GHCOPT) --make $@

%.ppm: %
	time -p ./$< $(THREADS) +RTS -N$(THREADS)

test: $(addsuffix .test,$(IMAGES))

%.test: %
	display $<

clean:
	-rm -f *.hi *.o $(SCENES)
