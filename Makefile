
all: insc_llvm insc_jvm
	
TestInstant: src/ParInstant.hs src/LexInstant.hs src/TestInstant.hs
	ghc -isrc --make src/TestInstant.hs -o TestInstant

insc_llvm: src/ParInstant.hs src/LexInstant.hs src/LLVMMain.hs src/LLVMCompiler.hs
	ghc -isrc --make src/LLVMMain.hs -o insc_llvm
insc_jvm: src/ParInstant.hs src/LexInstant.hs src/JVMMain.hs src/JVMCompiler.hs
	ghc -isrc --make src/JVMMain.hs -o insc_jvm



src/LexInstant.hs: src/LexInstant.x
	alex -g $<
src/ParInstant.hs: src/ParInstant.y
	happy -gca $<

clean:
	-rm -f src/*.bak src/*.log src/*.aux src/*.hi src/*.o src/*.dvi insc_llvm insc_jvm TestInstant
	-rm -f src/LexInstant.hs src/ParInstant.hs
	-rm -f examples/*.bc examples/*.ll  examples/*.j examples/*.class

%.ll: %.ins insc_llvm
	./insc_llvm $< 


%.j: %.ins insc_jvm
	./insc_jvm $<


test-llvm: examples/test01.ll examples/test02.ll examples/test03.ll examples/test04.ll examples/test05.ll examples/test06.ll examples/test07.ll
	lli examples/test01.bc | cmp - examples/test01.output
	lli examples/test02.bc | cmp - examples/test02.output
	lli examples/test03.bc | cmp - examples/test03.output
	lli examples/test04.bc | cmp - examples/test04.output
	lli examples/test05.bc | cmp - examples/test05.output
	lli examples/test06.bc | cmp - examples/test06.output
	lli examples/test07.bc | cmp - examples/test07.output

test-jvm: examples/test01.j examples/test02.j examples/test03.j examples/test04.j examples/test05.j examples/test06.j examples/test07.j
	(cd examples; java test01) | cmp - examples/test01.output
	(cd examples; java test02) | cmp - examples/test02.output
	(cd examples; java test03) | cmp - examples/test03.output
	(cd examples; java test04) | cmp - examples/test04.output
	(cd examples; java test05) | cmp - examples/test05.output
	(cd examples; java test06) | cmp - examples/test06.output
	(cd examples; java test07) | cmp - examples/test07.output

dist: 
	tar -czf ks386105.tar.gz Makefile README src/ lib/ examples/
