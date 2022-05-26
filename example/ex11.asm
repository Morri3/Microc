
extern printi
extern printc
extern checkargc
global asm_main
default rel
section .data
glovars: dq 0
section .text
asm_main:
	push rbp
	mov qword [glovars], rsp
	sub qword [glovars], 8
	push rdx ;save asm_main args
	push rcx
	;check arg count:
	sub rsp, 24
	mov rdx, rcx
	mov rcx, 1
	call checkargc
	add rsp, 24
	pop rcx
	pop rdx ;pop asm_main args
	; allocate globals:
	
ldargs:           ;set up command line arguments on stack:
	mov rcx, rcx
	mov rsi, rdx
_args_next:
	cmp rcx, 0
	jz _args_end
	push qword [rsi]
	add rsi, 8
	sub rcx, 1
	jmp _args_next      ;repeat until --ecx == 0
_args_end:
	lea rbp, [rsp-0*8]  ; make rbp point to first arg
	;CALL 1,L1_main
	push rbp 
	call near L1_main
	push rbx
	;STOP
	mov rsp, qword [glovars]
	add rsp, 8          ; restore rsp
	pop rbp
	ret
	
L1_main:
	pop rax			; retaddr
	pop r10			; oldbp  
	sub rsp, 16     ; make space for svm r,bp 
	mov rsi, rsp 
	mov rbp, rsp 
	add rbp, 8	   ; 8*arity 

_L1_main_pro_1:	  ; slide 2 stack slot
	cmp rbp, rsi      
	jz _L1_main_pro_2    
	mov rcx, [rsi+16] 
	mov [rsi], rcx    
	add rsi, 8        
	jmp _L1_main_pro_1    

_L1_main_pro_2: 
	sub rbp, 8 ; rbp pointer to first arg 
	mov [rbp+16], rax ; set retaddr 
	mov [rbp+8], r10  ; set oldbp
	;INCSP 1
	lea rsp, [rsp-8*(1)]
	;INCSP 1
	lea rsp, [rsp-8*(1)]
	;INCSP 100
	lea rsp, [rsp-8*(100)]
	;GETSP
	push rsp
	;OFFSET 99
	push -792
	;SUB
	pop r10
	pop rax
	sub rax,r10
	push rax
	;INCSP 100
	lea rsp, [rsp-8*(100)]
	;GETSP
	push rsp
	;OFFSET 99
	push -792
	;SUB
	pop r10
	pop rax
	sub rax,r10
	push rax
	;INCSP 100
	lea rsp, [rsp-8*(100)]
	;GETSP
	push rsp
	;OFFSET 99
	push -792
	;SUB
	pop r10
	pop rax
	sub rax,r10
	push rax
	;INCSP 100
	lea rsp, [rsp-8*(100)]
	;GETSP
	push rsp
	;OFFSET 99
	push -792
	;SUB
	pop r10
	pop rax
	sub rax,r10
	push rax
	;GETBP
	push rbp
	;OFFSET 2
	push -16
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;CSTI 1
	push 1
	;STI
	pop r10
	pop rax
	mov [rax],r10
	push r10
	;INCSP -1
	lea rsp, [rsp-8*(-1)]
	;GOTO L3
	jmp L3
	
L2:
	;GETBP
	push rbp
	;OFFSET 103
	push -824
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;GETBP
	push rbp
	;OFFSET 2
	push -16
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;CSTI -8
	push -8
	;MUL
	pop rax
	pop r10
	imul r10
	push rax
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;CSTI 0
	push 0
	;STI
	pop r10
	pop rax
	mov [rax],r10
	push r10
	;INCSP -1
	lea rsp, [rsp-8*(-1)]
	;GETBP
	push rbp
	;OFFSET 2
	push -16
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;GETBP
	push rbp
	;OFFSET 2
	push -16
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;CSTI 1
	push 1
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;STI
	pop r10
	pop rax
	mov [rax],r10
	push r10
	;INCSP -1
	lea rsp, [rsp-8*(-1)]
	;INCSP 0
	lea rsp, [rsp-8*(0)]
	
L3:
	;GETBP
	push rbp
	;OFFSET 2
	push -16
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;GETBP
	push rbp
	;OFFSET 0
	push -0
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;SWAP
	pop rax
	pop r10
	push rax
	push r10
	;LT
	pop rax
	pop r10
	cmp r10, rax
	jl .Lasm0
	push 0
	jmp .Lasm1
.Lasm0:
	push 1
.Lasm1:
	;NOT
	pop rax
	xor rax, 1
	push rax
	;IFNZRO L2
	pop rax
	cmp rax,0
	jne L2
	;GETBP
	push rbp
	;OFFSET 2
	push -16
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;CSTI 1
	push 1
	;STI
	pop r10
	pop rax
	mov [rax],r10
	push r10
	;INCSP -1
	lea rsp, [rsp-8*(-1)]
	;GOTO L5
	jmp L5
	
L4:
	;GETBP
	push rbp
	;OFFSET 204
	push -1632
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;GETBP
	push rbp
	;OFFSET 2
	push -16
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;CSTI -8
	push -8
	;MUL
	pop rax
	pop r10
	imul r10
	push rax
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;GETBP
	push rbp
	;OFFSET 305
	push -2440
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;GETBP
	push rbp
	;OFFSET 2
	push -16
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;CSTI -8
	push -8
	;MUL
	pop rax
	pop r10
	imul r10
	push rax
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;CSTI 0
	push 0
	;STI
	pop r10
	pop rax
	mov [rax],r10
	push r10
	;STI
	pop r10
	pop rax
	mov [rax],r10
	push r10
	;INCSP -1
	lea rsp, [rsp-8*(-1)]
	;GETBP
	push rbp
	;OFFSET 2
	push -16
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;GETBP
	push rbp
	;OFFSET 2
	push -16
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;CSTI 1
	push 1
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;STI
	pop r10
	pop rax
	mov [rax],r10
	push r10
	;INCSP -1
	lea rsp, [rsp-8*(-1)]
	;INCSP 0
	lea rsp, [rsp-8*(0)]
	
L5:
	;GETBP
	push rbp
	;OFFSET 2
	push -16
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;CSTI 2
	push 2
	;GETBP
	push rbp
	;OFFSET 0
	push -0
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;MUL
	pop rax
	pop r10
	imul r10
	push rax
	;SWAP
	pop rax
	pop r10
	push rax
	push r10
	;LT
	pop rax
	pop r10
	cmp r10, rax
	jl .Lasm2
	push 0
	jmp .Lasm3
.Lasm2:
	push 1
.Lasm3:
	;NOT
	pop rax
	xor rax, 1
	push rax
	;IFNZRO L4
	pop rax
	cmp rax,0
	jne L4
	;GETBP
	push rbp
	;OFFSET 1
	push -8
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;GETBP
	push rbp
	;OFFSET 2
	push -16
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;CSTI 1
	push 1
	;STI
	pop r10
	pop rax
	mov [rax],r10
	push r10
	;STI
	pop r10
	pop rax
	mov [rax],r10
	push r10
	;INCSP -1
	lea rsp, [rsp-8*(-1)]
	;GOTO L7
	jmp L7
	
L6:
	;GOTO L9
	jmp L9
	
L8:
	;GOTO L11
	jmp L11
	
L10:
	;GETBP
	push rbp
	;OFFSET 2
	push -16
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;GETBP
	push rbp
	;OFFSET 2
	push -16
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;CSTI 1
	push 1
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;STI
	pop r10
	pop rax
	mov [rax],r10
	push r10
	;INCSP -1
	lea rsp, [rsp-8*(-1)]
	
L11:
	;GETBP
	push rbp
	;OFFSET 2
	push -16
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;GETBP
	push rbp
	;OFFSET 0
	push -0
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;SWAP
	pop rax
	pop r10
	push rax
	push r10
	;LT
	pop rax
	pop r10
	cmp r10, rax
	jl .Lasm4
	push 0
	jmp .Lasm5
.Lasm4:
	push 1
.Lasm5:
	;NOT
	pop rax
	xor rax, 1
	push rax
	;IFZERO L13
	pop rax
	cmp rax,0
	je L13
	;GETBP
	push rbp
	;OFFSET 103
	push -824
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;GETBP
	push rbp
	;OFFSET 2
	push -16
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;CSTI -8
	push -8
	;MUL
	pop rax
	pop r10
	imul r10
	push rax
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;IFNZRO L17
	pop rax
	cmp rax,0
	jne L17
	;GETBP
	push rbp
	;OFFSET 204
	push -1632
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;GETBP
	push rbp
	;OFFSET 2
	push -16
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;GETBP
	push rbp
	;OFFSET 1
	push -8
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;SUB
	pop r10
	pop rax
	sub rax,r10
	push rax
	;GETBP
	push rbp
	;OFFSET 0
	push -0
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;CSTI -8
	push -8
	;MUL
	pop rax
	pop r10
	imul r10
	push rax
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;GOTO L16
	jmp L16
	
L17:
	;CSTI 1
	push 1
	
L16:
	;IFNZRO L15
	pop rax
	cmp rax,0
	jne L15
	;GETBP
	push rbp
	;OFFSET 305
	push -2440
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;GETBP
	push rbp
	;OFFSET 2
	push -16
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;GETBP
	push rbp
	;OFFSET 1
	push -8
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;CSTI -8
	push -8
	;MUL
	pop rax
	pop r10
	imul r10
	push rax
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;GOTO L14
	jmp L14
	
L15:
	;CSTI 1
	push 1
	
L14:
	;GOTO L12
	jmp L12
	
L13:
	;CSTI 0
	push 0
	
L12:
	;IFNZRO L10
	pop rax
	cmp rax,0
	jne L10
	;GETBP
	push rbp
	;OFFSET 2
	push -16
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;GETBP
	push rbp
	;OFFSET 0
	push -0
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;SWAP
	pop rax
	pop r10
	push rax
	push r10
	;LT
	pop rax
	pop r10
	cmp r10, rax
	jl .Lasm6
	push 0
	jmp .Lasm7
.Lasm6:
	push 1
.Lasm7:
	;NOT
	pop rax
	xor rax, 1
	push rax
	;IFZERO L18
	pop rax
	cmp rax,0
	je L18
	;GETBP
	push rbp
	;OFFSET 406
	push -3248
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;GETBP
	push rbp
	;OFFSET 1
	push -8
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;CSTI -8
	push -8
	;MUL
	pop rax
	pop r10
	imul r10
	push rax
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;GETBP
	push rbp
	;OFFSET 2
	push -16
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;STI
	pop r10
	pop rax
	mov [rax],r10
	push r10
	;INCSP -1
	lea rsp, [rsp-8*(-1)]
	;GETBP
	push rbp
	;OFFSET 103
	push -824
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;GETBP
	push rbp
	;OFFSET 2
	push -16
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;CSTI -8
	push -8
	;MUL
	pop rax
	pop r10
	imul r10
	push rax
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;GETBP
	push rbp
	;OFFSET 204
	push -1632
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;GETBP
	push rbp
	;OFFSET 2
	push -16
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;GETBP
	push rbp
	;OFFSET 1
	push -8
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;SUB
	pop r10
	pop rax
	sub rax,r10
	push rax
	;GETBP
	push rbp
	;OFFSET 0
	push -0
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;CSTI -8
	push -8
	;MUL
	pop rax
	pop r10
	imul r10
	push rax
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;GETBP
	push rbp
	;OFFSET 305
	push -2440
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;GETBP
	push rbp
	;OFFSET 2
	push -16
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;GETBP
	push rbp
	;OFFSET 1
	push -8
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;CSTI -8
	push -8
	;MUL
	pop rax
	pop r10
	imul r10
	push rax
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;CSTI 1
	push 1
	;STI
	pop r10
	pop rax
	mov [rax],r10
	push r10
	;STI
	pop r10
	pop rax
	mov [rax],r10
	push r10
	;STI
	pop r10
	pop rax
	mov [rax],r10
	push r10
	;INCSP -1
	lea rsp, [rsp-8*(-1)]
	;GETBP
	push rbp
	;OFFSET 1
	push -8
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;GETBP
	push rbp
	;OFFSET 1
	push -8
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;CSTI 1
	push 1
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;STI
	pop r10
	pop rax
	mov [rax],r10
	push r10
	;INCSP -1
	lea rsp, [rsp-8*(-1)]
	;GETBP
	push rbp
	;OFFSET 2
	push -16
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;CSTI 1
	push 1
	;STI
	pop r10
	pop rax
	mov [rax],r10
	push r10
	;INCSP -1
	lea rsp, [rsp-8*(-1)]
	;INCSP 0
	lea rsp, [rsp-8*(0)]
	;GOTO L19
	jmp L19
	
L18:
	;GETBP
	push rbp
	;OFFSET 1
	push -8
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;GETBP
	push rbp
	;OFFSET 1
	push -8
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;CSTI 1
	push 1
	;SUB
	pop r10
	pop rax
	sub rax,r10
	push rax
	;STI
	pop r10
	pop rax
	mov [rax],r10
	push r10
	;INCSP -1
	lea rsp, [rsp-8*(-1)]
	;GETBP
	push rbp
	;OFFSET 1
	push -8
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;CSTI 0
	push 0
	;SWAP
	pop rax
	pop r10
	push rax
	push r10
	;LT
	pop rax
	pop r10
	cmp r10, rax
	jl .Lasm8
	push 0
	jmp .Lasm9
.Lasm8:
	push 1
.Lasm9:
	;IFZERO L20
	pop rax
	cmp rax,0
	je L20
	;GETBP
	push rbp
	;OFFSET 2
	push -16
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;GETBP
	push rbp
	;OFFSET 406
	push -3248
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;GETBP
	push rbp
	;OFFSET 1
	push -8
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;CSTI -8
	push -8
	;MUL
	pop rax
	pop r10
	imul r10
	push rax
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;STI
	pop r10
	pop rax
	mov [rax],r10
	push r10
	;INCSP -1
	lea rsp, [rsp-8*(-1)]
	;GETBP
	push rbp
	;OFFSET 103
	push -824
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;GETBP
	push rbp
	;OFFSET 2
	push -16
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;CSTI -8
	push -8
	;MUL
	pop rax
	pop r10
	imul r10
	push rax
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;GETBP
	push rbp
	;OFFSET 204
	push -1632
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;GETBP
	push rbp
	;OFFSET 2
	push -16
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;GETBP
	push rbp
	;OFFSET 1
	push -8
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;SUB
	pop r10
	pop rax
	sub rax,r10
	push rax
	;GETBP
	push rbp
	;OFFSET 0
	push -0
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;CSTI -8
	push -8
	;MUL
	pop rax
	pop r10
	imul r10
	push rax
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;GETBP
	push rbp
	;OFFSET 305
	push -2440
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;GETBP
	push rbp
	;OFFSET 2
	push -16
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;GETBP
	push rbp
	;OFFSET 1
	push -8
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;CSTI -8
	push -8
	;MUL
	pop rax
	pop r10
	imul r10
	push rax
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;CSTI 0
	push 0
	;STI
	pop r10
	pop rax
	mov [rax],r10
	push r10
	;STI
	pop r10
	pop rax
	mov [rax],r10
	push r10
	;STI
	pop r10
	pop rax
	mov [rax],r10
	push r10
	;INCSP -1
	lea rsp, [rsp-8*(-1)]
	;GETBP
	push rbp
	;OFFSET 2
	push -16
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;GETBP
	push rbp
	;OFFSET 2
	push -16
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;CSTI 1
	push 1
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;STI
	pop r10
	pop rax
	mov [rax],r10
	push r10
	;INCSP -1
	lea rsp, [rsp-8*(-1)]
	;INCSP 0
	lea rsp, [rsp-8*(0)]
	;GOTO L21
	jmp L21
	
L20:
	;INCSP 0
	lea rsp, [rsp-8*(0)]
	
L21:
	;INCSP 0
	lea rsp, [rsp-8*(0)]
	
L19:
	;INCSP 0
	lea rsp, [rsp-8*(0)]
	
L9:
	;GETBP
	push rbp
	;OFFSET 1
	push -8
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;GETBP
	push rbp
	;OFFSET 0
	push -0
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;SWAP
	pop rax
	pop r10
	push rax
	push r10
	;LT
	pop rax
	pop r10
	cmp r10, rax
	jl .Lasm10
	push 0
	jmp .Lasm11
.Lasm10:
	push 1
.Lasm11:
	;NOT
	pop rax
	xor rax, 1
	push rax
	;IFZERO L23
	pop rax
	cmp rax,0
	je L23
	;GETBP
	push rbp
	;OFFSET 1
	push -8
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;CSTI 0
	push 0
	;EQ
	pop rax
	pop r10
	cmp rax, r10
	jne .Lasm12
	push 1
	jmp .Lasm13
.Lasm12:
	push 0
.Lasm13:
	;NOT
	pop rax
	xor rax, 1
	push rax
	;GOTO L22
	jmp L22
	
L23:
	;CSTI 0
	push 0
	
L22:
	;IFNZRO L8
	pop rax
	cmp rax,0
	jne L8
	;GETBP
	push rbp
	;OFFSET 1
	push -8
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;GETBP
	push rbp
	;OFFSET 0
	push -0
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;SWAP
	pop rax
	pop r10
	push rax
	push r10
	;LT
	pop rax
	pop r10
	cmp r10, rax
	jl .Lasm14
	push 0
	jmp .Lasm15
.Lasm14:
	push 1
.Lasm15:
	;IFZERO L24
	pop rax
	cmp rax,0
	je L24
	;INCSP 1
	lea rsp, [rsp-8*(1)]
	;GETBP
	push rbp
	;OFFSET 407
	push -3256
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;CSTI 1
	push 1
	;STI
	pop r10
	pop rax
	mov [rax],r10
	push r10
	;INCSP -1
	lea rsp, [rsp-8*(-1)]
	;GOTO L27
	jmp L27
	
L26:
	;GETBP
	push rbp
	;OFFSET 406
	push -3248
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;GETBP
	push rbp
	;OFFSET 407
	push -3256
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;CSTI -8
	push -8
	;MUL
	pop rax
	pop r10
	imul r10
	push rax
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;PRINTI
	pop rcx
	push rcx
	sub rsp, 16
	call printi
	add rsp, 16
	;INCSP -1
	lea rsp, [rsp-8*(-1)]
	;GETBP
	push rbp
	;OFFSET 407
	push -3256
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;GETBP
	push rbp
	;OFFSET 407
	push -3256
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;CSTI 1
	push 1
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;STI
	pop r10
	pop rax
	mov [rax],r10
	push r10
	;INCSP -1
	lea rsp, [rsp-8*(-1)]
	;INCSP 0
	lea rsp, [rsp-8*(0)]
	
L27:
	;GETBP
	push rbp
	;OFFSET 407
	push -3256
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;GETBP
	push rbp
	;OFFSET 0
	push -0
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;SWAP
	pop rax
	pop r10
	push rax
	push r10
	;LT
	pop rax
	pop r10
	cmp r10, rax
	jl .Lasm16
	push 0
	jmp .Lasm17
.Lasm16:
	push 1
.Lasm17:
	;NOT
	pop rax
	xor rax, 1
	push rax
	;IFNZRO L26
	pop rax
	cmp rax,0
	jne L26
	;CSTI 10
	push 10
	;PRINTC
	
                    pop rcx
	push rcx
	sub rsp, 16
	call printc
	add rsp, 16
	;INCSP -1
	lea rsp, [rsp-8*(-1)]
	;GETBP
	push rbp
	;OFFSET 1
	push -8
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;GETBP
	push rbp
	;OFFSET 1
	push -8
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;CSTI 1
	push 1
	;SUB
	pop r10
	pop rax
	sub rax,r10
	push rax
	;STI
	pop r10
	pop rax
	mov [rax],r10
	push r10
	;INCSP -1
	lea rsp, [rsp-8*(-1)]
	;GETBP
	push rbp
	;OFFSET 1
	push -8
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;CSTI 0
	push 0
	;SWAP
	pop rax
	pop r10
	push rax
	push r10
	;LT
	pop rax
	pop r10
	cmp r10, rax
	jl .Lasm18
	push 0
	jmp .Lasm19
.Lasm18:
	push 1
.Lasm19:
	;IFZERO L28
	pop rax
	cmp rax,0
	je L28
	;GETBP
	push rbp
	;OFFSET 2
	push -16
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;GETBP
	push rbp
	;OFFSET 406
	push -3248
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;GETBP
	push rbp
	;OFFSET 1
	push -8
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;CSTI -8
	push -8
	;MUL
	pop rax
	pop r10
	imul r10
	push rax
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;STI
	pop r10
	pop rax
	mov [rax],r10
	push r10
	;INCSP -1
	lea rsp, [rsp-8*(-1)]
	;GETBP
	push rbp
	;OFFSET 103
	push -824
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;GETBP
	push rbp
	;OFFSET 2
	push -16
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;CSTI -8
	push -8
	;MUL
	pop rax
	pop r10
	imul r10
	push rax
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;GETBP
	push rbp
	;OFFSET 204
	push -1632
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;GETBP
	push rbp
	;OFFSET 2
	push -16
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;GETBP
	push rbp
	;OFFSET 1
	push -8
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;SUB
	pop r10
	pop rax
	sub rax,r10
	push rax
	;GETBP
	push rbp
	;OFFSET 0
	push -0
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;CSTI -8
	push -8
	;MUL
	pop rax
	pop r10
	imul r10
	push rax
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;GETBP
	push rbp
	;OFFSET 305
	push -2440
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;GETBP
	push rbp
	;OFFSET 2
	push -16
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;GETBP
	push rbp
	;OFFSET 1
	push -8
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;CSTI -8
	push -8
	;MUL
	pop rax
	pop r10
	imul r10
	push rax
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;CSTI 0
	push 0
	;STI
	pop r10
	pop rax
	mov [rax],r10
	push r10
	;STI
	pop r10
	pop rax
	mov [rax],r10
	push r10
	;STI
	pop r10
	pop rax
	mov [rax],r10
	push r10
	;INCSP -1
	lea rsp, [rsp-8*(-1)]
	;GETBP
	push rbp
	;OFFSET 2
	push -16
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;GETBP
	push rbp
	;OFFSET 2
	push -16
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;CSTI 1
	push 1
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;STI
	pop r10
	pop rax
	mov [rax],r10
	push r10
	;INCSP -1
	lea rsp, [rsp-8*(-1)]
	;INCSP 0
	lea rsp, [rsp-8*(0)]
	;GOTO L29
	jmp L29
	
L28:
	;INCSP 0
	lea rsp, [rsp-8*(0)]
	
L29:
	;INCSP -1
	lea rsp, [rsp-8*(-1)]
	;GOTO L25
	jmp L25
	
L24:
	;INCSP 0
	lea rsp, [rsp-8*(0)]
	
L25:
	;INCSP 0
	lea rsp, [rsp-8*(0)]
	
L7:
	;GETBP
	push rbp
	;OFFSET 1
	push -8
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;CSTI 0
	push 0
	;SWAP
	pop rax
	pop r10
	push rax
	push r10
	;LT
	pop rax
	pop r10
	cmp r10, rax
	jl .Lasm20
	push 0
	jmp .Lasm21
.Lasm20:
	push 1
.Lasm21:
	;IFNZRO L6
	pop rax
	cmp rax,0
	jne L6
	;INCSP -406
	lea rsp, [rsp-8*(-406)]
	;RET 0
	pop rbx
	add rsp, 8*0
	pop rbp
	ret
	