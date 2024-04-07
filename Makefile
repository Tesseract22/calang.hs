CACHE_DIR := cache
ASM_DIR := asm
LD_FLAGS := -dynamic-linker /lib64/ld-linux-x86-64.so.2
NASM_FLAGS := -f elf64 -g -F dwarf
all: main test
bin_dir:
	mkdir -p bin
cache_dir:
	mkdir -p cache
asm_dir:
	mkdir -p asm


bin/Parser: Parser.hs bin_dir
	ghc $< -odir ${CACHE_DIR} -o $@
bin/main: bin/Parser main.cal bin_dir asm_dir cache_dir
	./$< main.cal ${ASM_DIR}/main.asm
	nasm ${NASM_FLAGS} -o ${CACHE_DIR}/main.o ${ASM_DIR}/main.asm
	ld ${LD_FLAGS} -lc ${CACHE_DIR}/main.o -o $@
bin/test: ${ASM_DIR}/test.asm bin_dir cache asm_dir
	nasm $< ${NASM_FLAGS} -o ${CACHE_DIR}/test.o 
	ld ${LD_FLAGS} -lc ${CACHE_DIR}/test.o -o $@
	./$@

Parser: bin/Parser
main: bin/main
test: bin/test
clean:
	rm -r asm bin cache