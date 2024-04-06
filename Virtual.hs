
module Virtual (Inst(..), Program, compileInst, compileProgram) where
import Text.Printf (printf)

data Inst = 
    InstPush    Int     |
    InstPushf   Float   |
    InstDupBase Int     |
    InstPop     Int     | 

    InstAdd | 
    InstSub | 
    InstDiv | 
    InstMod | 
    InstMul |

    InstAddf | 
    InstSubf | 
    InstDivf | 
    InstModf | 
    InstMulf |

    InstJump    String | 
    InstJumpEq  String |

    InstCall String |
    InstShow        |
    InstShowf

compileInst :: Inst -> String
compileInst inst = case inst of
    InstPush    i -> "\tpush " ++ show i ++ "\n"
    InstPushf   f -> "\tadd rsp, 8\n" ++ printf "\tmov rax, __float64(%f)__\n" f ++ "\tmov [rsp], rax\n"
    InstPop     i -> "\tsub rsp, " ++ show (i * 8) ++ "\n"
    InstDupBase i -> printf "\tpush qword [rbp - %d]\n" (i * 8)

    InstAdd -> "\tpop rax\n" ++ "\tadd   qword [rsp], rax\n"
    InstSub -> "\tpop rax\n" ++ "\tsub   qword [rsp], rax\n"
    InstMul -> "\tpop rax\n" ++ "\timul  qword rax, [rsp]\n" ++ "\tmov [rsp], rax\n"
    InstDiv -> "\tpop rbx\n" ++ "\tpop rax\n" ++ "\txor rdx, rdx,\n" ++ "\tdiv rbx\n" ++ "\tpush rax\n"
    InstMod -> "\tpop rbx\n" ++ "\tpop rax\n" ++ "\txor rdx, rdx,\n" ++ "\tdiv rbx\n" ++ "\tpush rdx\n"

    InstAddf -> "\tmovsd xmm0, [rsp]\n" ++ "\tmovsd xmm1, [rsp - 8]\n" ++ "\taddsd xmm0, xmm1\n" ++ "\tmovsd [rsp - 8], xmm0\b" ++ "\tsub rsp, 8\b"
    InstSubf -> "\tmovsd xmm0, [rsp]\n" ++ "\tmovsd xmm1, [rsp - 8]\n" ++ "\tsubsd xmm0, xmm1\n" ++ "\tmovsd [rsp - 8], xmm0\b" ++ "\tsub rsp, 8\b"
    InstMulf -> "\tmovsd xmm0, [rsp]\n" ++ "\tmovsd xmm1, [rsp - 8]\n" ++ "\tmulsd xmm0, xmm1\n" ++ "\tmovsd [rsp - 8], xmm0\b" ++ "\tsub rsp, 8\b"
    InstDivf -> "\tmovsd xmm0, [rsp]\n" ++ "\tmovsd xmm1, [rsp - 8]\n" ++ "\tdivsd xmm0, xmm1\n" ++ "\tmovsd [rsp - 8], xmm0\b" ++ "\tsub rsp, 8\b"
    
    InstJump    lable -> "\tjmp " ++ lable ++ "\n" 
    InstJumpEq  lable -> "\tcmp [rsp - 8], [rsp]\n" ++ compileInst (InstPop 2)

    InstCall    lable -> "\tcall " ++ lable ++ "\n"
    InstShow          -> "\tpop rsi\n" ++ "\tmov rdi, format\n" ++  "\tmov rax, 0\n" ++ compileInst (InstCall "align_printf")
    InstShowf         -> "\tmovsd xmm0, [rsp]\n" ++ "\tsub rsp, 8\n" ++ "\tmov rdi, formatf\n" ++ "\tmov rax, 1\n" ++ compileInst (InstCall "align_printf")

type Program = [Inst]


builtinData = "section        .data   \n\     
\    format        db \"= %i\", 0xa, 0x0    \n\
\    formatf        db \"= %f\", 0xa, 0x0    \n\
\    aligned       db 0                     \n"

builtinText = "section        .text    \n\
\extern printf           \n\
\extern exit             \n\
\global         _start   \n"
builtinShow = "align_printf:            \n\
\    mov rbx, rsp        \n\
\    and rbx, 0x000000000000000f\n\
\    cmp rbx, 0             \n\
\    je .align_end          \n\
\    .align:                \n\
\        push rbx           \n\
\        mov byte [aligned], 1  \n\
\    .align_end:                \n\
\    call printf                \n\
\    cmp byte [aligned], 1      \n\
\    jne .unalign_end           \n\
\    .unalign:                  \n\
\        pop rbx                \n\
\        mov byte [aligned], 0  \n\
\    .unalign_end:              \n\
\    ret                \n"
builtinStart = "_start:\n\tpush rbp\n\tmov rbp, rsp\n"
builtinExit = "\tmov rdi, 0\n\tcall exit\n"
compileProgram :: Program -> String 
compileProgram prog = builtinData ++ builtinText ++ builtinShow ++ builtinStart ++ foldl1 (++) (compileInst <$> (prog ++ [InstPop 1])) ++ builtinExit        