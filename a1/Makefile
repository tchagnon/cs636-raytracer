SCENES = scene0 teapot
IMAGES = $(addsuffix .ppm,$(SCENES))
DEPENDS = MathHmatrix MathDirect MatInv RayTracer
GHCARGS = -threaded #-prof -auto-all #-DUSING_HMATRIX
GHCOPT = -O2 -funbox-strict-fields -fexcess-precision -funfolding-keeness-factor=10 # -optc-O3 -optc-msse3 -optc-ffast-math
THREADS = 4

all: $(SCENES)

%: %.hs  $(addsuffix .hs,$(DEPENDS))
	ghc -cpp $(GHCARGS) $(GHCOPT) --make $@

%.ppm: %
	time -p ./$< $(THREADS) +RTS -N$(THREADS)

test: $(addsuffix .test,$(IMAGES))

%.test: %
	display $<

clean:
	-rm -f *.hi *.o $(SCENES)
