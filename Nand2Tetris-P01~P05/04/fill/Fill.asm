// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

// Put your code here.

(START)
@24576
D=M
@BLACK
D;JGT
@WHITE
D;JEQ

(BLACK)
@16384
D=A
@R0
M=D
(LOOPB)
@24575
D=A
@R0
D=M-D
@ENDB
D;JGT
@R0
A=M
M=-1
@R0
M=M+1
@LOOPB
0;JMP
(ENDB)
@START
0;JMP

(WHITE)
@16384
D=A
@R0
M=D
(LOOPW)
@24575
D=A
@R0
D=M-D
@ENDW
D;JGT
@R0
A=M
M=0
@R0
M=M+1
@LOOPW
0;JMP
(ENDW)
@START
0;JMP

