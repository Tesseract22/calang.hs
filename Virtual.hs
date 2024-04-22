
module Virtual (Inst(..), Program, compileInst, compileProgram, freshIn) where

import Text.Printf (printf)
import Data.Map (Map, insert, lookup, empty, size, singleton, foldlWithKey, toList, member)

data Inst =
    InstPush    Int     |
    InstPushf   Float   |
    InstDupBase Int     |
    InstPop     Int     |
    InstReserve Int     |
    InstAssign  Int     |

    InstSaveRet         |
    InstRet             |

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

    Insti2f |
    Instf2i |

    InstJump    String |
    InstJumpEq  String |

    InstSaveStk        |
    InstLoadStk        |
    InstCall String Int |
    InstShow        |
    InstShowf
    deriving (Show)
compileInst :: Inst -> String
debugInst :: Inst -> String
debugInst inst = "\t; " ++ case inst of
    InstDupBase _ -> "dup base"
    InstRet -> "ret"

    InstAdd -> "add"
    InstSub -> "sub"
    InstMul -> "mul"
    InstDiv -> "div"
    InstMod -> "mod"

    InstAddf -> "addf"
    InstSubf -> "subf"
    InstMulf -> "mulf"
    InstDivf -> "divf"
    InstModf -> "modf"

    Insti2f -> "i2f"
    Instf2i -> "f2i"

    InstCall f _ -> "call " ++ f
    InstSaveStk -> "save stk top"
    InstLoadStk -> "load stk top"
    InstShow -> "show"
    InstShowf -> "showf"
    _ -> ""
    ++ "\n"

compileInst inst = debugInst inst ++ case inst of
    InstPush    i -> printf "\tpush %i\n" i
    InstPushf   f -> "\tsub rsp, 8\n" ++ printf "\tmov rax, __float64__(%f)\n" f ++ "\tmov [rsp], rax\n"
    InstPop     i -> "\tadd rsp, " ++ show (i * 8) ++ "\n"
    InstReserve i -> "\tsub rsp, " ++ show (i * 8) ++ "\n"
    InstAssign  i -> "\tpop rax\n" ++ printf "\tmov [rbp - %d], rax\n" (i * 8)
    InstDupBase i -> if i > 0 then printf "\tpush qword [rbp - %d]\n" (i * 8) else printf "\tpush qword [rbp + %d]\n" (-i * 8)


    InstAdd -> "\tpop rax\n" ++ "\tadd   qword [rsp], rax\n"
    InstSub -> "\tpop rax\n" ++ "\tsub   qword [rsp], rax\n"
    InstMul -> "\tpop rax\n" ++ "\timul  qword rax, [rsp]\n" ++ "\tmov [rsp], rax\n"
    InstDiv -> "\tpop rbx\n" ++ "\tpop rax\n" ++ "\txor rdx, rdx,\n" ++ "\tdiv rbx\n" ++ "\tpush rax\n"
    InstMod -> "\tpop rbx\n" ++ "\tpop rax\n" ++ "\txor rdx, rdx,\n" ++ "\tdiv rbx\n" ++ "\tpush rdx\n"

    InstAddf -> "\tmovsd xmm0, [rsp]\n" ++ "\tmovsd xmm1, [rsp + 8]\n" ++ "\taddsd xmm0, xmm1\n" ++ "\tmovsd [rsp + 8], xmm0\n" ++ "\tadd rsp, 8\n"
    InstSubf -> "\tmovsd xmm0, [rsp]\n" ++ "\tmovsd xmm1, [rsp + 8]\n" ++ "\tsubsd xmm0, xmm1\n" ++ "\tmovsd [rsp + 8], xmm0\n" ++ "\tadd rsp, 8\n"
    InstMulf -> "\tmovsd xmm0, [rsp]\n" ++ "\tmovsd xmm1, [rsp + 8]\n" ++ "\tmulsd xmm0, xmm1\n" ++ "\tmovsd [rsp + 8], xmm0\n" ++ "\tadd rsp, 8\n"
    InstDivf -> "\tmovsd xmm0, [rsp]\n" ++ "\tmovsd xmm1, [rsp + 8]\n" ++ "\tdivsd xmm0, xmm1\n" ++ "\tmovsd [rsp + 8], xmm0\n" ++ "\tadd rsp, 8\n"
    -- InstModf -> "\tmovsd xmm0, [rsp]\n" ++ "\tmovsd xmm1, [rsp - 8]\n" ++ "\tcvttss2si rax, xmm0\n" ++ "\tcvtsi2ss xmm2, rax\n"
    --     ++ "\tmulsd, xmm2, xmm1\n" ++ "\tmovsd xmm0, [rsp]\n" ++ "\tsubsd xmm0, xmm2\n" ++          "\tmovsd [rsp - 8], xmm0\n" ++ "\tsub rsp, 8\n"
    InstModf -> undefined

    Insti2f -> "\tmov rax, [rsp]\n" ++ "\tcvtsi2sd xmm0, rax\n" ++ "\tmovsd [rsp], xmm0\n"
    Instf2i -> undefined

    InstJump    lable -> "\tjmp " ++ lable ++ "\n"
    InstJumpEq  lable -> "\tcmp [rsp - 8], [rsp]\n" ++ compileInst (InstPop 2)

    InstSaveStk          -> "\tpush rbp\n\tmov rbp, rsp\n" 
    InstLoadStk          -> "\tpop rbp\n"
    InstCall    lable i  -> printf "\tcall %s\n" lable ++ compileInst (InstPop i) ++ "\tpush rax\n"
    InstRet -> compileInst InstLoadStk ++ "\tret\n"
    InstSaveRet -> "\tpop rax\n"

    InstShow          -> "\tpop rsi\n" ++ "\tmov rdi, format\n" ++  "\tmov rax, 0\n" ++ "\tcall align_printf\n"
    InstShowf         -> "\tmovsd xmm0, [rsp]\n" ++ "\tadd rsp, 8\n" ++ "\tmov rdi, formatf\n" ++ "\tmov rax, 1\n" ++ "\tcall align_printf\n"

type Program = Map String [Inst]


freshIn :: Program -> String -> String
freshIn prog hint 
    | member hint prog =  
        let freshIn' prog ct 
                | member ( "anony" ++ show ct) prog = freshIn' prog (ct + 1)
                | otherwise = "annoy" ++ show ct in 
        freshIn' prog 0

    | otherwise = hint



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



fnStart = "\tpush rbp\n\tmov rbp, rsp\n"
-- fnRet   = "\tret\n"

compileFn :: (String, [Inst]) -> String
compileFn (lable, insts) = lable ++ ":\n" ++ fnStart ++ foldl1 (++) (compileInst <$> insts)

builtinStart = "_start:\n\tcall main\n\tmov rdi, rax\n\tcall exit\n"
-- builtinExit = "\tpop rbp\n\tmov rdi, 0\n\tcall exit\n"
compileProgram :: Program -> String
compileProgram prog = builtinData ++ builtinText ++ builtinShow ++ builtinStart ++ foldl1 (++) (compileFn <$> toList prog)