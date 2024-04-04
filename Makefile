all: Parser test
Parser: Parser.hs
	ghc Parser.hs -odir cache
main: Parser main.cal
	./Parser main.cal main.asm
	nasm -f elf64 -o cache/main.o main.asm
	ld -dynamic-linker /lib64/ld-linux-x86-64.so.2 -lc cache/main.o -o main
test: test.asm
	nasm -f elf64 -o cache/test.o test.asm
	ld -dynamic-linker /lib64/ld-linux-x86-64.so.2 -lc cache/test.o -o test
	./test