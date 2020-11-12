
all: LLVMMain TestInstant
	
TestInstant: ParInstant.hs LexInstant.hs TestInstant.hs
	ghc --make TestInstant.hs -o TestInstant

LLVMMain: ParInstant.hs LexInstant.hs LLVMMain.hs
	ghc --make LLVMMain.hs -o LLVMMain

debug: info.txt

ParInstant.y LexInstant.x: Instant.cf
	bnfc $<

LexInstant.hs: LexInstant.x
	alex -g $<
ParInstant.hs: ParInstant.y
	happy -gca $<

info.txt: ParInstant.y
	happy -gca ParInstant.y
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocInstant.ps
distclean: clean
	-rm -f DocInstant.* LexInstant.* ParInstant.* LayoutInstant.* SkelInstant.* PrintInstant.* TestInstant.* AbsInstant.* TestInstant ErrM.* SharedString.* Instant.dtd XMLInstant.* info.txt

%.ll: %.ins LLVMMain
	./LLVMMain $< > $@

%.e: %.ll
	clang $< printint.c -o $@

test: examples/test01.e examples/test02.e examples/test03.e examples/test04.e examples/test05.e examples/test06.e examples/test07.e
	examples/test01.e | cmp - examples/test01.output
	examples/test02.e | cmp - examples/test02.output
	examples/test03.e | cmp - examples/test03.output
	examples/test04.e | cmp - examples/test04.output
	examples/test05.e | cmp - examples/test05.output
	examples/test06.e | cmp - examples/test06.output
	examples/test07.e | cmp - examples/test07.output
