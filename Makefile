
all: insc_llvm insc_jvm TestInstant
	
TestInstant: ParInstant.hs LexInstant.hs TestInstant.hs
	ghc --make TestInstant.hs -o TestInstant

insc_llvm: ParInstant.hs LexInstant.hs LLVMMain.hs LLVMCompiler.hs
	ghc --make LLVMMain.hs -o insc_llvm
insc_jvm:  ParInstant.hs LexInstant.hs JVMMain.hs JVMCompiler.hs
	ghc --make JVMMain.hs -o insc_jvm


ParInstant.y LexInstant.x: Instant.cf
	bnfc $<

LexInstant.hs: LexInstant.x
	alex -g $<
ParInstant.hs: ParInstant.y
	happy -gca $<

clean:
	-rm -f *.bak *.log *.aux *.hi *.o *.dvi insc_llvm insc_jvm TestInstant
	-rm -f DocInstant.ps DocInstant.txt
	-rm -f AbsInstant.hs LexInstant.hs ParInstant.hs PrintInstant.hs TestInstant.hs SkelInstant.hs ErrM.hs ParInstant.y LexInstant.x
	-rm -f examples/*.{bc,ll,e,j,class}

%.ll: %.ins insc_llvm
	./insc_llvm $< 

%.e: %.ll
	clang $< printint.c -o $@

%.j: %.ins insc_jvm
	./insc_jvm $<


test-llvm: examples/test01.e examples/test02.e examples/test03.e examples/test04.e examples/test05.e examples/test06.e examples/test07.e
	examples/test01.e | cmp - examples/test01.output
	examples/test02.e | cmp - examples/test02.output
	examples/test03.e | cmp - examples/test03.output
	examples/test04.e | cmp - examples/test04.output
	examples/test05.e | cmp - examples/test05.output
	examples/test06.e | cmp - examples/test06.output
	examples/test07.e | cmp - examples/test07.output

test-jvm: examples/test01.j examples/test02.j examples/test03.j examples/test04.j examples/test05.j examples/test06.j examples/test07.j
	(cd examples; java test01) | cmp - examples/test01.output
	(cd examples; java test02) | cmp - examples/test02.output
	(cd examples; java test03) | cmp - examples/test03.output
	(cd examples; java test04) | cmp - examples/test04.output
	(cd examples; java test05) | cmp - examples/test05.output
	(cd examples; java test06) | cmp - examples/test06.output
	(cd examples; java test07) | cmp - examples/test07.output
