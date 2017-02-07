
.PHONY: all test_consistency_hs_lhs

all: diff_ok XlInterpreter.pdf DemoXlInterpreter.pdf

DemoXlInterpreter.pdf: DemoXlInterpreter.tex
	pdflatex DemoXlInterpreter.tex

DemoXlInterpreter.tex: DemoXlInterpreter.lhs Xl_format.lhs
	~/.cabal/bin/lhs2TeX DemoXlInterpreter.lhs > DemoXlInterpreter.tex

XlInterpreter.pdf: XlInterpreter.tex
	pdflatex XlInterpreter.tex

XlInterpreter.tex: XlInterpreter.lhs Xl_format.lhs
	~/.cabal/bin/lhs2TeX XlInterpreter.lhs > XlInterpreter.tex

DemoXlInterpreter: DemoXlInterpreter.lhs XlInterpreter.lhs
	rm -f XlInterpreter.o XlInterpreter.hi DemoXlInterpreter.o DemoXlInterpreter.hi
	ghc -o DemoXlInterpreter -cpp DemoXlInterpreter.lhs

lhs_lines: DemoXlInterpreter
	./DemoXlInterpreter > lhs_lines

hs_lines: XlInterpreter_hs
	./XlInterpreter_hs > hs_lines

diff_ok: works.txt lhs_lines
	diff works.txt lhs_lines > diff_ok
	[ `stat -c '%s' diff_ok` = 0 ]
#diff_ok: hs_lines lhs_lines
#	diff hs_lines lhs_lines > diff_ok
#	[ `stat -c '%s' diff_ok` = 0 ]

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

