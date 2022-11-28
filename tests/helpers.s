# These things can't be written in C yet.

.globl returns_argument
returns_argument:
    mov %rdi, %rax
    ret

.globl adds_arguments
adds_arguments:
    lea (%rdi, %rsi), %rax
    ret

.globl adds_six_arguments
adds_six_arguments:
    add %rsi, %rdi
    add %rdx, %rdi
    add %rcx, %rdi
    add %r8, %rdi
    lea (%rdi, %r9), %rax
    ret
