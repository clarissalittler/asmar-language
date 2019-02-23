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

The convention is that for instructions with multiple arguments, the last argument is where you're storing the result of instruction. So for example 

    Add r1 r2 r3
   
means that you should take the contents of register 1 and the contents of register 2, add them together, and then store the result in register 3.

Now let's talk about the instructions systematically

    Add
    AddI
	Sub
	SubI
	Mul
	MulI
	Div
	DivI
	
are all the arithematic instructions. The ones with an I at the end take an integer (positive or negative whole number) as their first argument and then two registers as arguments. The ones without the I at the end take three registers as arguments. Here's a little translation for picturing what the instructions means

    Add r1 r2 r3 ==> r1 + r2 -> r3
	AddI n r1 r2 ==> r1 + n -> r2
	Sub r1 r2 r3 ==> r1 - r2 -> r3
	SubI n r1 r2 ==> r1 - n  -> r2
	Mul r1 r2 r3 ==> r1 * r2 -> r3
	MulI n r1 r2 ==> r1 * n  -> r2
	Div r1 r2 r3 ==> r1 / r2 -> r3
	DivI n r1 r2 ==> r1 / n  -> r2
	
Next we have the move instructions, whose soul purpose is to move data into a register. Mov takes a register and copies its contents into another register. MovI takes a number and puts it in a register.

After that, we should discuss the jump instructions. These are used to move around in the program since there aren't any loops in the language itself. 

	Jmp
	JmpR
	JCon
	
Jmp takes a label and immediately jumps to it. JmpR takes a register and jumps to the instruction number given by the number in the register. The number has to be positive for this to work! JCon, short for jump-conditional, is how we make if-statement like choices. It takes a register and if the contents of the register are greater than zero it jumps to the first label, jumping to the second label otherwise. 

Now! One thing that's pretty important is that if we want to make something like a function call, we need to record where the function call is coming from! This requires us to have an idea of where in the program we are when we make the function call! To help us out, we have the Pc, short for program counter, instruction, which puts the current instruction number into the register its given. You can then use an AddI to offset the program counter to the place you want to return-to after the function is called. We do this in the factorial example above!

Next are the Boolean operators

	And
	Or
	Xor
	Not

And takes two registers and puts 1 in the third register if both the first two registers contain positive numbers and 0 otherwise. Or takes two registers and puts 1 in the third register if either of the first two registers contains a positive number and 0 otherwise. Xor, which stands for exclusive-or, takes two registers and puts a 1 in the third if only one of the two registers has a positive number. Not takes a register and replaces its contents with 0 if it was positive and 1 otherwise. 

The Eql operator puts a 1 in its third argument if the first two registers contain the same value. The Lt operator puts a 1 in its third argument if the first register is less than the second register.

Finally, the Store and Load operators manipulate the memory locations. The Store operator takes the value in the first register and it in the location named by the second operator. The Load operation takes the data in the location named by the first operator and stores it in the second register.
