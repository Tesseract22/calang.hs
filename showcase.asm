section        .data          
    format        db "The result is: %i!", 0xa, 0x0   
section        .text    
extern printf           
extern exit             
global         _start   
_start:                 
                        
	push 1
	push 2
	push 10
	pop rax
	pop rcx
	imul rcx, rax
	push rcx
	pop rax
	pop rcx
	add rcx, rax
	push rcx
	push 5
	push 2
	pop rax
	pop rcx
	sub rcx, rax
	push rcx
	pop rax
	pop rcx
	add rcx, rax
	push rcx
    pop rsi
    mov rdi, format
    mov rax, 0
    call printf
	mov rdi, 0
	call exit
