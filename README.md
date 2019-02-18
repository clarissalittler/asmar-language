# asmar-language
Asmar is a small assembly language for a fake processor, indended to be useful for teaching.

The syntax is fairly simple. Let's look at an example first

    Jmp main
    .fact
    MovI 1 r15
    .loop
    JCon r0 body endfact
    .body
    Mul r0 r15 r15
    SubI 1 r0 r0
    Jmp loop
    
    .endfact
    JmpR r14
    
    .main
    MovI 4 r0
    Pc r14
    AddI 3 r14 r14
    Jmp fact
    Print r15
