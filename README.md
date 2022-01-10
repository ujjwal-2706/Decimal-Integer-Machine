# Decimal-Integer-Machine
BDIM(Basic Decimal Integer Machine) compiler written in Standard ML

This is the interpreter of a hypothetical language named BDIM (basic decimal integer machine)
The exact syntax of this language along with the opcodes ( meaning of the operations which the number perform)
is given in the pdf named Assignment-1(Updated).

The basic concept behind the working of this interpreter is
that it first reads the .bdim file from top to bottom and then parse it to check if the 
syntax of the .bdim file is in accordance with the language syntax and if not then it will raise exception

After parsing the files we will go through each instruction from top to down and update our memory
and finally print the output followed by halting of the program.
