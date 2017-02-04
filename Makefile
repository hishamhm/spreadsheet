
EXCEL=0

.PHONY: all test_consistency_hs_lhs

all: diff_ok excel4.pdf

excel4.pdf: excel4.tex
	pdflatex excel4.tex

excel4.tex: excel4.lhs
	~/.cabal/bin/lhs2TeX -lexcel=$(EXCEL) excel4.lhs > excel4.tex

EXCEL:
	make clean all EXCEL=1

excel4: excel4.lhs
	rm -f excel4.o excel4.hi
	ghc -o excel4 -cpp -DEXCEL=$(EXCEL) excel4.lhs

lhs_lines: excel4
	./excel4 | tr ',' '\n' | tr -d '[]' > lhs_lines

hs_lines: excel4_hs
	./excel4_hs | tr ',' '\n' | tr -d '[]' > hs_lines

diff_ok: hs_lines lhs_lines
	diff hs_lines lhs_lines > diff_ok
	[ `stat -c '%s' diff_ok` = 0 ]

excel4_hs: excel4_hs.hs
	rm -f excel4_hs.o excel4_hs.hi
	ghc -o excel4_hs excel4_hs.hs

excel3: excel3.hs
	ghc -o excel3 excel3.hs

excel2: excel2.hs
	ghc -o excel2 excel2.hs

excel2.1: excel2.1.hs
	ghc -o excel2.1 excel2.1.hs

excel1: excel1.hs
	ghc -o excel1 excel1.hs

clean:
	rm -f excel3 excel4 excel4_hs *.o *.hi excel4.tex excel4.pdf

