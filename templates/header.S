// kill-all-types header

__kat__putchar:
        push    %rbp
        mov     %rsp, %rbp

        mov     %dil, -1(%rbp)

        mov     $1, %rax
        mov     $1, %rdi
        mov     %rbp, %rsi
        sub     $1, %rsi
        mov     $1, %rdx
        syscall

        pop     %rbp
        ret

__kat__print_dec:
        push    %rbp
        mov     %rsp, %rbp
        sub     $64, %rsp
        mov     %rcx, -8(%rbp)

        // zero is a special case
        cmp     $0, %rdi
        jnz     __kat__print_dec_nonzero

        mov     $'0', %dil
        call    __kat__putchar

        jmp     __kat__print_dec_return

__kat__print_dec_nonzero:

        // store each decimal point in order on stack
        xor     %rcx, %rcx
        mov     %rdi, %rax
        mov     %rsp, %rdi
__kat__print_dec_extrude_loop:
        // divide by 10
        xor     %rdx, %rdx
        mov     $10, %rsi
        div     %rsi

        // store mod 10 + iterate
        movb    %dl, (%rdi)
        add     $1, %rdi
        add     $1, %rcx

        cmp     $0, %rax
        jnz     __kat__print_dec_extrude_loop

        // actually print the numbers
        mov     %rdi, %rax
__kat__print_dec_print_loop:
        sub     $1, %rax
        mov     (%rax), %dil
        add     $'0', %dil
        push    %rax
        push    %rcx
        call    __kat__putchar
        pop     %rcx
        pop     %rax
        loop    __kat__print_dec_print_loop

__kat__print_dec_return:
        mov     -8(%rbp), %rcx
        add     $64, %rsp
        pop     %rbp
        ret

// debug print a u64
__kat__debug:
        mov     +8(%rsp), %rdi
        call    __kat__print_dec
        mov     $'\n', %rdi
        call    __kat__putchar
        ret

.global _start
_start:
        // compiler generated from this point onwards