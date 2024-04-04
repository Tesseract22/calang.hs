section        .data          
    format        db "The result is: %i!", 0xa, 0x0   
section        .text    
extern printf           
extern exit             
global         _start   
_start:                 
    push rbp            
    mov rbp, rsp        
                        
	push 5
	push 10
	push 0
	push qword [rbp - 8]
    pop rsi
    mov rdi, format
    mov rax, 0
    call printf
	push qword [rbp - 8]
    pop rsi
    mov rdi, format
    mov rax, 0
    call printf
	pop rax
	pop rax
	pop rbp
	mov rdi, 0
	call exit
