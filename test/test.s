        default rel
        section .text
        global $entry
        extern $peek_byte
        extern $read_byte
        extern $write_byte
        extern $raise_error
        extern $collect_garbage
$entry:
        push rbx
        push rbp
        push r15
        mov rbp, rsp
        mov rbx, rdi
        add rbx, 0
        mov rax, 0
        push rax
        mov rax, 0
        mov [(rbx + 0)], rax
        pop rax
        mov [(rbx + 8)], rax
        mov rax, rbx
        xor rax, 2
        add rbx, 16
        push rax
        mov rax, 128
        mov [(rbx + 0)], rax
        mov rax, rbx
        xor rax, 1
        add rbx, 8
        push rax
        mov rax, 32
        add rsp, 8
        add rsp, 8
        add rsp, 0
        pop r15
        pop rbp
        pop rbx
        ret
$err:
        mov r15, rsp
        and r15, 8
        sub rsp, r15
        call $raise_error
