
all: insc_llvm insc_jvm TestInstant
	
TestInstant: src/ParInstant.hs src/LexInstant.hs src/TestInstant.hs
	ghc -isrc --make src/TestInstant.hs -o TestInstant

insc_llvm: src/ParInstant.hs src/LexInstant.hs src/LLVMMain.hs src/LLVMCompiler.hs
	ghc -isrc --make src/LLVMMain.hs -o insc_llvm
insc_jvm: src/ParInstant.hs src/LexInstant.hs src/JVMMain.hs src/JVMCompiler.hs
	ghc -isrc --make src/JVMMain.hs -o insc_jvm


src/ParInstant.y src/LexInstant.x: src/Instant.cf
	bnfc -o src/  $<

src/LexInstant.hs: src/LexInstant.x
	alex -g $<
src/ParInstant.hs: src/ParInstant.y
	happy -gca $<

clean:
	-rm -f src/*.bak src/*.log src/*.aux src/*.hi src/*.o src/*.dvi insc_llvm insc_jvm TestInstant
	-rm -f src/DocInstant.ps src/DocInstant.txt
	-rm -f src/AbsInstant.hs src/LexInstant.hs src/ParInstant.hs src/PrintInstant.hs src/TestInstant.hs src/SkelInstant.hs src/ErrM.hs src/ParInstant.y src/LexInstant.x
	-rm -f examples/*.bc examples/*.ll examples/*.e examples/*.j examples/*.class

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
