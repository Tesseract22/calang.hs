section        .data          
    format        db "= %i", 0xa, 0x0   
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
	push qword [rbp - 8]
	pop rax
	pop rcx
	imul rcx, rax
	push rcx
	push 2
	push qword [rbp - 8]
	pop rax
	pop rcx
	imul rcx, rax
	push rcx
	push qword [rbp - 16]
	pop rax
	pop rcx
	imul rcx, rax
	push rcx
	pop rax
	pop rcx
	add rcx, rax
	push rcx
	push qword [rbp - 16]
	push qword [rbp - 16]
	pop rax
	pop rcx
	imul rcx, rax
	push rcx
	pop rax
	pop rcx
	add rcx, rax
	push rcx
    pop rsi
    mov rdi, format
    mov rax, 0
    call printf
	push qword [rbp - 8]
	push qword [rbp - 16]
	pop rax
	pop rcx
	add rcx, rax
	push rcx
	push qword [rbp - 8]
	push qword [rbp - 16]
	pop rax
	pop rcx
	add rcx, rax
	push rcx
	pop rax
	pop rcx
	imul rcx, rax
	push rcx
    pop rsi
    mov rdi, format
    mov rax, 0
    call printf
	push 10
	push 3
	pop rbx
	pop rax
	xor rdx, rdx,
	div rbx
	push rdx
	push 9
	pop rax
	pop rcx
	imul rcx, rax
	push rcx
	push 3
	pop rbx
	pop rax
	xor rdx, rdx,
	div rbx
	push rax
    pop rsi
    mov rdi, format
    mov rax, 0
    call printf
	pop rax
	pop rax
	pop rbp
	mov rdi, 0
	call exit
