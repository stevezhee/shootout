UNAME := $(shell uname)

CC = clang -O3 -lm

EXE =
ifeq ($(UNAME), Linux)
	EXE = ~/.local/bin/shootout
else
	EXE = ~/AppData/Roaming/local/bin/shootout.exe
endif
ifeq ($(UNAME), Linux)
	FFI-EXE = ~/.local/bin/shootout-ffi
else
	FFI-EXE = ~/AppData/Roaming/local/bin/shootout-ffi.exe
endif

all : ffi

shoot : shootout.cabal
	stack install
#	$(EXE)

ffi : shootout-ffi.cabal
	stack install
	$(FFI-EXE)
	mv SDL.hs src
	# shootout > t.ll
	# llc t.ll
	# clang t.s -lSDL2
	# ./a.out

shootout-ffi.cabal :
	mv shootout.cabal shootout.cabal.keep
	mv shootout-ffi.cabal.keep shootout-ffi.cabal

shootout.cabal :
	mv shootout-ffi.cabal shootout-ffi.cabal.keep
	mv shootout.cabal.keep shootout.cabal

# all : a.exe nbody.exe fannkuch.exe spectral.exe
# 	ls -al $^
# 	# ./a.exe 2000
# 	# ./spectral.exe 5500 # spectral:(5500)=1.274224153
# 	# ./fannkuch.exe 7
# 	# ./nbody.exe 1
# 	./a.exe 2
# 	./spectral.exe 2


t.ll : Main.hs Shoot.hs Untyped.hs
	stack install
	shoot

%.s : %.ll
	llc $^

a.exe : t.s main.c
	${CC} -o $@ main.c t.s

# GCC = gcc -O2 -o $@ $< -lm

# all : nbody.out main.out
# 	diff $^
# 	ls -al *.exe

# %.out : %.exe
# 	./$< 100 > $@

%.exe : %.c main.c
	${CC} -o $@ $^

# gen.c : NBody.hs
# 	runghc -Wall $< > $@

# main.exe : main.c gen.c
# 	${GCC}

# clean :
# 	rm -f *.out *.exe *.stackdump *.o *.hi gen.c

# C:\Users\letner\AppData\Local\Programs\stack\i386-windows\ghc-7.8.4\bin:C:\Users\letner\AppData\Local\Programs\stack\i386-windows\ghc-7.8.4\mingw\bin:C:\Users\letner\AppData\Local\Programs\stack\i386-windows\git-2.4.5.1\cmd:C:\Users\letner\AppData\Local\Programs\stack\i386-windows\git-2.4.5.1\usr\bin:
