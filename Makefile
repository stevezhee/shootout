GCC = gcc -O2 -o $@ $< -lm

all : nbody.out main.out
	diff $^
	ls -al *.exe

%.out : %.exe
	./$< 100 > $@

nbody.exe : nbody.c
	${GCC}

gen.c : NBody.hs
	runghc $<

main.exe : main.c gen.c
	${GCC}
