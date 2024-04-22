section        .data   
    format        db "= %i", 0xa, 0x0    
    formatf        db "= %f", 0xa, 0x0    
    aligned       db 0                     
section        .text    
extern printf           
extern exit             
global         _start   
align_printf:            
    mov rbx, rsp        
    and rbx, 0x000000000000000f
    cmp rbx, 0             
    je .align_end          
    .align:                
        push rbx           
        mov byte [aligned], 1  
    .align_end:                
    call printf                
    cmp byte [aligned], 1      
    jne .unalign_end           
    .unalign:                  
        pop rbx                
        mov byte [aligned], 0  
    .unalign_end:              
    ret                
_start:
	call main
	mov rdi, rax
	call exit
f:
	push rbp
	mov rbp, rsp
	; 
	sub rsp, 0
	; 
	push 1024
	; 
	pop rax
	; 
	add rsp, 0
	; ret
	; load stk top
	pop rbp
	ret
g:
	push rbp
	mov rbp, rsp
	; 
	sub rsp, 0
	; dup base
	push qword [rbp + 24]
	; dup base
	push qword [rbp + 16]
	; sub
	pop rax
	sub   qword [rsp], rax
	; 
	pop rax
	; 
	add rsp, 0
	; ret
	; load stk top
	pop rbp
	ret
h:
	push rbp
	mov rbp, rsp
	; 
	sub rsp, 0
	; dup base
	push qword [rbp + 24]
	; dup base
	push qword [rbp + 16]
	; add
	pop rax
	add   qword [rsp], rax
	; show
	pop rsi
	mov rdi, format
	mov rax, 0
	call align_printf
	; dup base
	push qword [rbp + 24]
	; dup base
	push qword [rbp + 16]
	; add
	pop rax
	add   qword [rsp], rax
	; 
	pop rax
	; 
	add rsp, 0
	; ret
	; load stk top
	pop rbp
	ret
main:
	push rbp
	mov rbp, rsp
	; 
	sub rsp, 40
	; 
	push 10
	; 
	pop rax
	mov [rbp - 8], rax
	; 
	push 5
	; 
	pop rax
	mov [rbp - 16], rax
	; 
	push 1024
	; 
	pop rax
	mov [rbp - 24], rax
	; call f
	call f
	; 
	add rsp, 0
	push rax
	; 
	pop rax
	mov [rbp - 32], rax
	; dup base
	push qword [rbp - 8]
	; dup base
	push qword [rbp - 16]
	; call h
	call h
	; 
	add rsp, 16
	push rax
	; show
	pop rsi
	mov rdi, format
	mov rax, 0
	call align_printf
	; 
	push 1024
	; 
	pop rax
	mov [rbp - 40], rax
	; dup base
	push qword [rbp - 8]
	; dup base
	push qword [rbp - 16]
	; call g
	call g
	; 
	add rsp, 16
	push rax
	; show
	pop rsi
	mov rdi, format
	mov rax, 0
	call align_printf
	; 
	push 0
	; 
	pop rax
	; 
	add rsp, 40
	; ret
	; load stk top
	pop rbp
	ret
