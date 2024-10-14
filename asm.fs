VOCABULARY ASSEMBLER  ASSEMBLER DEFINITIONS  HEX

\ register definitions
0 CONSTANT AX  1 CONSTANT CX  2 CONSTANT DX  3 CONSTANT BX
4 CONSTANT RSP 5 CONSTANT RBP 6 CONSTANT SI  7 CONSTANT DI

VARIABLE MODE
: #       1 MODE ! ;
: RESET   0 MODE ! ;

\ ---------------------------------------------------------
\    64 bit mode op+r instr (no rex prefix)
: 64REGOP CREATE c, DOES> c@ OR c, ;

58 64REGOP POP    50 64REGOP PUSH


\ ---------------------------------------------------------
\    1 byte rex.r unnop instr
: 1UNOP CREATE c, 3 LSHIFT c, 
        DOES> dup 48 c, c@ c, 1+ c@ C0 OR OR c, ;

0 FF 1UNOP INC    1 FF 1UNOP DEC
2 F7 1UNOP NOT    3 F7 1UNOP NEG
4 F7 1UNOP MUL    5 F7 1UNOP IMUL
6 F7 1UNOP DIV    7 F7 1UNOP IDIV

\ ---------------------------------------------------------
\    1 byte rex.r unnop instr
\ : REG-BINOP CREATE c,
\             DOES> c@ OR c,
\             MODE @ CASE
\               0 OF c@ c, SWAP 3 LSHIFT C0 OR OR c, ENDOF
\               1 OF 1+ c@ 3 LSHIFT 81 c, C0 OR OR c, , -4 ALLOT ENDOF
\             ENDCASE
\   RESET ;
\ 
\ 89 REG-BINOP MOV

\ ---------------------------------------------------------
\    1 byte rex.r general binop instr
: GENERAL-BINOP CREATE c, c,
         DOES> 48 c, 
         MODE @ CASE
           0 OF c@ c, SWAP 3 LSHIFT C0 OR OR c, ENDOF
           1 OF 1+ c@ 3 LSHIFT 81 c, C0 OR OR c, , -4 ALLOT ENDOF
         ENDCASE
  RESET ; 

0 01 GENERAL-BINOP ADD   5 2B GENERAL-BINOP SUB   6 33 GENERAL-BINOP XOR
1 0B GENERAL-BINOP OR    4 23 GENERAL-BINOP AND   7 39 GENERAL-BINOP CMP

\ ---------------------------------------------------------
: 1PRE-BINOP CREATE c, c,
         DOES> dup 48 c, c@ c, 1+ c@ c, SWAP 3 LSHIFT C0 + + c, ;

AF 0F 1PRE-BINOP IMULL


FORTH DEFINITIONS DECIMAL

assembler

code poo ax pop 10 # ax ADD ax push next end-code

code poo1 ax pop ax inc ax push next end-code
code poo2 ax pop ax dec ax push next end-code
code poo3 ax pop ax neg ax push next end-code
code poo4 ax pop ax not ax push next end-code

code add ax pop bx pop bx ax add ax push next end-code
code sub bx pop ax pop bx ax sub ax push next end-code
code imull ax pop bx pop ax bx imull ax push next end-code
code and ax pop bx pop ax bx and bx push next end-code
code cmp ax pop bx pop ax bx cmp bx push next end-code
code div bx pop ax pop dx dx xor bx div ax push next end-code
code idiv bx pop ax pop dx dx xor bx idiv ax push next end-code
code mul bx pop ax pop dx dx xor bx mul ax push next end-code
code imul bx pop ax pop dx dx xor bx imul ax push next end-code

\ code mov ax pop bx pop ax bx mov bx push next end-code
\ code mov2 0 # ax mov ax push next end-code

\ 10 poo . cr

\ 20 30 add . cr
forth
