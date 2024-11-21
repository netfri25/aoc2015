section .bss

; int fd
fd: resb 4

; char buf
letter: resb 1

; int count
count: resb 4

%define SYS_READ 0
%define SYS_OPEN 2

%define O_RDONLY 0

section .text
global _start
_start:

   ; rax = open(path, O_RDONLY)
   mov rax, SYS_OPEN
   mov rdi, path
   mov rsi, O_RDONLY
   syscall

   ; handle error
   cmp rax, 0
   jl error

   ; fd = rax
   mov dword [fd], eax

   mov dword [count], 0
loop:
   ; rax = read(fd, &buf, 1)
   mov rax, SYS_READ
   xor rdi, rdi
   mov edi, dword [fd]
   mov rsi, letter
   mov rdx, 1
   syscall

   ; reached EOF
   cmp rax, 0
   je eof

   ; handle error
   jl error

   cmp byte [letter], '('
   je up
   cmp byte [letter], ')'
   je down
   jmp error

up:
   inc dword [count]
   jmp loop

down:
   dec dword [count]
   jmp loop

eof:
   jmp eof
   ret

error:
   ret


section .data
path db "./input.txt", 0
