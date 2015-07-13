all : a.exe

t.ll : Main.hs Shoot.hs Untyped.hs
	stack install
	shoot

%.s : %.ll
	llc $^

a.exe : t.s main.c
	clang main.c t.s

# GCC = gcc -O2 -o $@ $< -lm

# all : nbody.out main.out
# 	diff $^
# 	ls -al *.exe

# %.out : %.exe
# 	./$< 100 > $@

# nbody.exe : nbody.c
# 	${GCC}

# gen.c : NBody.hs
# 	runghc -Wall $< > $@

# main.exe : main.c gen.c
# 	${GCC}

# clean :
# 	rm -f *.out *.exe *.stackdump *.o *.hi gen.c

# C:\Users\letner\AppData\Local\Programs\stack\i386-windows\ghc-7.8.4\bin:C:\Users\letner\AppData\Local\Programs\stack\i386-windows\ghc-7.8.4\mingw\bin:C:\Users\letner\AppData\Local\Programs\stack\i386-windows\git-2.4.5.1\cmd:C:\Users\letner\AppData\Local\Programs\stack\i386-windows\git-2.4.5.1\usr\bin:
