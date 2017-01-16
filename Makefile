
all: excel1 excel2 excel3 excel4

excel4: excel4.hs
	ghc -o excel4 excel4.hs

excel3: excel3.hs
	ghc -o excel3 excel3.hs

excel2: excel2.hs
	ghc -o excel2 excel2.hs

excel2.1: excel2.1.hs
	ghc -o excel2.1 excel2.1.hs

excel1: excel1.hs
	ghc -o excel1 excel1.hs

clean:
	rm -f excel3.hi excel3.o excel3 excel4.hi excel4.o excel4

