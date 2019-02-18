# asmar-language
Asmar is a small assembly language for a fake processor, indended to be useful for teaching.

# Installing and running

# A tour of the language

The syntax is fairly simple. Let's look at an example first. This program calculates the factorial of 4 and prints it to the screen

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

Here's the general syntax of Asmar in roughly BNF syntax---although it's really just the Haskell datatype

       data Inst = Add Reg Reg Reg
                 | AddI Int Reg Reg
		         | Sub Reg Reg Reg
		         | SubI Int Reg Reg
		         | Div Reg Reg Reg
		         | DivI Int Reg Reg
		         | Mul Reg Reg Reg
		         | MulI Int Reg Reg
		         | MovI Int Reg
		         | Mov Reg Reg
		         | Pc Reg
		         | Jmp Label
		         | JmpR Reg
		         | JCon Reg Label Label
		         | Print Reg
		         | And Reg Reg Reg
		         | Or Reg Reg Reg
		         | XOr Reg Reg Reg
		         | Eql Reg Reg Reg
		         | Lt Reg Reg Reg
		         | Not Reg
		         | Store Reg Reg
		         | Load Reg Reg

To start with, registers are all represented by rN where N is any number from 0--15. Labels always exist on a line by themselves and are a '.' followed by a word. Every instruction is written exactly like in the syntax shown above. To use a label later, like when performing a jump, you don't need to use the '.' just the word

   MovI 10 r10
   Jmp heck
   Add r1 r2 r3

The convention is that 
