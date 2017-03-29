OPTIONS = -disable-interrupts\
          -debug-level 0\
          -block\
          -lambda-lift\
          -optimize-leaf-routines\
          -unsafe\
          -uses posix\
          -O3

map-closure: map-closure.sc QobiScheme-chicken.sc
	csc ${OPTIONS} map-closure.sc

install: map-closure
	cp map-closure ../bin/.
