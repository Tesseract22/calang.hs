# Calang
### Explanation
#### `Calang` (as in calculator language) is a educational and experimental language written in haskell. 
#### It supports basic mathematical operations and defining (nested) functions. It does not have `for` or `if` and does not support recursion. Calang is NOT turing complete.  
#### It can be compiled to `x86-64` nasm and then compiled to machine code with `nasm` with linkage to `libc`. It can also be interpreted directly.

### Example 
This is the example from `showcase.cal`:

```
a := 10
b := 5

f := fn (a, b) {
    g := fn (a, b) a + b
    ret g(a,b)*g(a,b)
}


g := fn (a) a*a

f(10, 5) = 
g(a) + 2*a*b + g(b) =
```

### Usage
Make sure you have the the `haskell` compiler `ghc` installed.

run `ghc Parser.hs -o Parser` to compile the compiler/interpreter

run `Parser <path_to_source_code.cal> <output_nasm_path.asm>` to compile `.cal` file.

