
.PHONY: all test_consistency_hs_lhs

all: diff_ok XlInterpreter.pdf XlInterpreterDemo.pdf

XlInterpreterDemo.pdf: XlInterpreterDemo.tex
	pdflatex XlInterpreterDemo.tex

XlInterpreterDemo.tex: XlInterpreterDemo.lhs
	~/.cabal/bin/lhs2TeX XlInterpreterDemo.lhs > XlInterpreterDemo.tex

XlInterpreter.pdf: XlInterpreter.tex
	pdflatex XlInterpreter.tex

XlInterpreter.tex: XlInterpreter.lhs
	~/.cabal/bin/lhs2TeX XlInterpreter.lhs > XlInterpreter.tex

XlInterpreter: XlInterpreter.lhs
	rm -f XlInterpreter.o XlInterpreter.hi XlInterpreterDemo.o XlInterpreterDemo.hi
	ghc -o XlInterpreter -cpp XlInterpreterDemo.lhs

lhs_lines: XlInterpreter
	./XlInterpreter | tr ',' '\n' | tr -d '[]' > lhs_lines

hs_lines: XlInterpreter_hs
	./XlInterpreter_hs | tr ',' '\n' | tr -d '[]' > hs_lines

diff_ok: hs_lines lhs_lines
	diff hs_lines lhs_lines > diff_ok
	[ `stat -c '%s' diff_ok` = 0 ]

XlInterpreter_hs: XlInterpreter_hs.hs
	rm -f XlInterpreter_hs.o XlInterpreter_hs.hi
	ghc -o XlInterpreter_hs XlInterpreter_hs.hs

excel3: excel3.hs
	ghc -o excel3 excel3.hs

excel2: excel2.hs
	ghc -o excel2 excel2.hs

excel2.1: excel2.1.hs
	ghc -o excel2.1 excel2.1.hs

excel1: excel1.hs
	ghc -o excel1 excel1.hs

clean:
	rm -f excel3 XlInterpreter XlInterpreter_hs *.o *.hi XlInterpreter.tex XlInterpreter.pdf

