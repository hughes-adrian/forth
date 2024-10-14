format PE64 console
use64
stack 4096*4096,4096*4096
org 401000h
entry start

include 'win64ax.inc'
include 'exception.inc'

; rax, rcx, rdx, r8, r9, r10, r11 volatile
; rbx, rbp, rdi, rsi, rsp, r12 - r15 non-volatile
DSP equ r15
RS equ rsp
FSP equ r12
IP equ rsi
LSP equ r13
LBP equ r14

NULL   equ   0
STDOUT equ -11
STDIN  equ -10


ADRIAN_VERSION             = 1
data_stack_size            = 8192
float_stack_size           = 4096
buffer_size                = 4096
initial_data_segment_size  = 262144

; macros for NEXT inner interpreter and stack push/pop
macro NEXT {
    ret
}

macro PUSHSP value {
    lea DSP, [DSP-8]
    mov qword [DSP], value
}

macro POPSP reg {
    mov reg, [DSP]
    lea DSP, [DSP+8]
}

macro FPUSH reg1 {
    lea FSP, [FSP-8]
    movsd [FSP], reg1
}

macro FPOP reg1 {
    movsd reg1, [FSP]
    lea FSP, [FSP+8]
}

macro defcode name, namelen, flags=0, lblw, nvoc=link {
    align 8
label name_#lblw
   dq nvoc              ; link
nvoc = name_#lblw
   db namelen+flags     ; flags + length byte
   db name              ; the name
   align 8              ; padding to next 4 byte boundary
label lblw
}

macro defconst name, namelen, flags=0, lbl, value, nvoc=link {
    align 8
label name_#lbl
    dq nvoc              ; link
nvoc = name_#lbl
    db namelen+flags             ; flags + length byte
    db name                ; the name
    align 8              ; padding to next 4 byte boundary
label lbl
    call DOCONST
label var_#lbl
    dq value
    NEXT
}

macro defvar name, namelen, flags=0, lbl, initial=0, nvoc=link {
    align 8
label name_#lbl
    dq nvoc              ; link
nvoc = name_#lbl
    db namelen+flags             ; flags + length byte
    db name                ; the name
    align 8              ; padding to next 4 byte boundary
label lbl
    call DOVAR
    dq 0
label var_#lbl
    dq initial
    NEXT
}

macro defvoc name, namelen, flags=0, lbl, head, nvoc=link {
    align 8
label name_#lbl
    dq nvoc              ; link
nvoc = name_#lbl
    db namelen+flags             ; flags + length byte
    db name                ; the name
    align 8              ; padding to next 4 byte boundary
label lbl
    call DODOES
    dq VODOES
label v_#lbl
    dq head,name_#lbl,vlink
vlink = name_#lbl
    NEXT
}

macro def [word] {
    call word
}

section '.bss' data readable writeable

ADRIAN_VERSION             = 2
data_stack_size            = 4096
float_stack_size           = 4096
buffer_size                = 4096
initial_data_segment_size  = 262144


align 8
data_stack:
    rq data_stack_size
data_stack_top:
align 8
float_stack:
    rb float_stack_size
float_stack_top:   
align 8
buffer:
    rb buffer_size

section '.text' code readable executable
align 8
start:
    push rbp
    mov rbp, rsp
    mov [BasePtr], rbp ; save the base pointer for clean exit

; get the command line args
    sub rsp, 40
    call    [GetCommandLineA]
    ;mov     [ARGV+16], rax   ; +16 => DOCOL LIT addr
    add     rsp, 40
; get stdout and stdin file handles
    sub     rsp, 32
    mov     rcx, STDOUT
    call    [GetStdHandle]
    mov     qword [WriteHandle], rax
    add     rsp, 32

    sub     rsp, 32
    mov     rcx, STDIN
    call    [GetStdHandle]
    mov     qword [ReadHandle], rax
    add     rsp, 32
    
    ; set console to raw mode, with ctrl-c passed to system
    sub     rsp, 32
    mov     rcx, qword [ReadHandle]
    mov     rdx, 0x1
    call    [SetConsoleMode]
    add     rsp, 32
    
    ; set output console to process virtual terminal escape codes
    ; first get the output mode
    sub     rsp, 32
    mov     rcx, qword [WriteHandle]
    mov     rdx, rsp
    call    [GetConsoleMode]
    mov     rcx, qword [WriteHandle]
    mov     rdx, [rsp]      
    or      rdx, 0x4     ; 0x4 is like terminal raw mode
    call    [SetConsoleMode]
    add     rsp, 32

    ; allocate a heap for the dictionary
    sub     rsp, 32
    ;call    [GetProcessHeap]
    ;mov     qword [HeapHandle], rax     ; store the handle
    ;mov     rcx, rax
    ;mov     rdx, 0x0
    ;mov     r8, initial_data_segment_size
    ;call    [HeapAlloc]   ; address of dictionary in rax
    ;mov     [var_FDP], rax
    ;mov     [HeapBase], rax  ; save the heap base addr for clean exit

    mov     rcx, 0
    mov     rdx, initial_data_segment_size
    mov     r8, 0x00001000
    mov     r9, 0x40
    call    [VirtualAlloc]
    mov     [VirtualBase], rax
    mov     [var_FDP], rax
    mov     [HeapBase], rax

    ; save base of return stack to R0 - we will use the default system stack
    mov     [var_RZ], RS
    
    ; save base of data stack to DSP
    mov     DSP, data_stack_top
    
    ; save base of float stack to FSP
    mov     FSP, float_stack_top
    
    ; set up local stack pointers
    ;mov LSP, LOCSTACK+4096*8
    ;mov LBP, LSP ; base pointer starts at TOS

    jmp     cold
    mov     rcx, 0
    call    [ExitProcess]

struc SMALL_RECT a,b,c,d {
   .l dw a
   .t dw b
   .r dw c
   .b dw d
}
struc COORD x,y {
   .x dw x
   .y dw y
}

struc CONSOLE_FONT_INFO  {
  .dwSize COORD ?,?
  .dwCurs COORD ?,?
  .wAttr  dw ?
  .srWind SMALL_RECT ?,?,?,?
  .dwMax  COORD ?,?
}

section '.data' data readable writeable executable

msg db 0xd, 0xa," AFORTH V1", 0xd, 0xa," 64bit ITC Forth", 0xd, 0xa, 0
msglen equ 32 ;$-msg
errmsg db "Error: invalid memory access" , 0xd, 0xa
errmsglen equ 30
okword db 3," ok"
word2  db 17,"<stack underflow>"
word3  db "AFORTH"
fmt db " %li ", 0
gfmt db "%lf",0
align 16
word_buffer db 33 dup 0
codespace:  rb 1024

WriteHandle     dq 0
ReadHandle      dq 0
HeapHandle      dq 0
Written         dq 0
key_scratch     dq 0
emit_scratch    dq 0
BasePtr         dq 0
HeapBase        dq 0
VirtualBase     dq 0
console CONSOLE_FONT_INFO

; define the word flags
F_IMMED   = 0x80
F_HIDDEN  = 0x20
F_LENMASK = 0x1f


; link is used to chain the words in the dictionary as they are defined
; zero is the end of list null sentinel
    link = 0
    rootlink = 0
    vlink = 0

cold:
    def SAYOK,STATE,FETCH,TX,LIT
    dq 65
    def QRX,TX
    def TX
    def LIT
    dq 0
    def ZBRANCH
    dq cold-$ 
    def QUIT,BYE
;    dq ARGC,TOR
;coldb:
;    dq RFETCH,ZBRANCH,colde
;    dq ARGV,RFETCH,ARGC,SWAP,MINUS,ONEPLUS
;    dq CELLS,MINUS,FETCH,DUPF,STRLEN,INCLUDED
;    dq FROMR,LIT,1,MINUS,TOR
;    dq BRANCH,coldb
;colde:
;    dq RDROP,LIT,msg,LIT,msglen,TYPEF
;cold_thread:
;    dq QUIT,BYE

defvar "STATE",5,0,STATE,70
defvar "DP",2,0,FDP,0
defvar "S0",2,0,S0,data_stack_top
defvar "F0",2,0,F0,float_stack_top
defvar "BASE",4,0,BASE,10
defvar "TIB",3,0,TIB,buffer
defvar "#TIB",4,0,NTIB,0
defvar ">IN",3,0,TOIN,0
defvar "SOURCE-ID",9,0,SOURCEID,0
defvar "HLD",3,0,HLD,0
defvar "'?KEY",5,0,TICKQKEY,QRX
defvar "'EMIT",5,0,TICKEMIT,TX
defvar "'EXPECT",7,0,TICKEXPECT,0;ACCEPT
defvar "'TAP",4,0,TICKTAP,0;KTAP
defvar "'ECHO",5,0,TICKECHO,TX
defvar "SPAN",4,0,SPAN,0
defvar "PRECISION",9,0,PRECISION,18
defvar "CURRENT",7,0,CURRENT,v_FORTH
defvar "CODESPACE",9,0,CSCC,codespace
;defvar "CONTEXT",7,0,CONTEXT,0
defvar "(DEFAULT)",9,F_HIDDEN,PARDEFAULT
; create an array of vocs as if it was
; created with ": variable context #vocs cells allot ;"
label name_CONTEXT
    dq link              ; link
link = name_CONTEXT
    db 7             ; flags + length byte
    db "CONTEXT"                ; the name
    align 8              ; padding to next 4 byte boundary
label CONTEXT
    call DOVAR
    dq   0
label var_CONTEXT
    dq v_FORTH,v_FORTH,v_ROOT,0,0,0,0,0
    dq 0,0,0,0,0,0,0,0
defconst "#VOCS",5,0,NVOCS,16
defvar "VOC-LINK",8,0,VOCLINK,name_FORTH
defvar "LATEST",6,0,LATEST,0;name_FDUMP ; must point to the last word
;                                        @ defined in assembly, SYSCALL0

label name_LOCDICT
    dq link              ; link
link = name_LOCDICT
    db 8             ; flags + length byte
    db "LOC-DICT"                ; the name
    align 8              ; padding to next 4 byte boundary
label LOCDICT
    call DOVAR
    dq   0
label var_LOCDICT
    dq 512 dup 0

label name_LOCSTACK
    dq link              ; link
link = name_LOCSTACK
    db 9             ; flags + length byte
    db "LOC-STACK"                ; the name
    align 8              ; padding to next 4 byte boundary
label LOCSTACK
    call DOVAR
    dq   0
label var_LOCSTACK
    dq 4096 dup 0

label name_LOCVEC
    dq link              ; link
link = name_CONTEXT
    db 6+F_HIDDEN             ; flags + length byte
    db "LOCVEC"                ; the name
    align 8              ; padding to next 4 byte boundary
label LOCVEC
    call DOVAR
    dq   0
label var_LOCVEC
    ;dq TOLOC0,TOLOC1,TOLOC2,TOLOC3,TOLOC4,TOLOC5,TOLOC6,TOLOC7

defvar "LOC-CNT",7,0,LOCCNT,0
defvar "LOC-OFFSET",10,0,LOCOFFSET,0
defvar "LOC-FLG",7,0,LOCFLG,0
defvar "OLD-DP",6,0,OLDDP,0
defvar "LP",2,0,LP,0
defvar "LSTATE",6,0,LSTATE,0 ; false
defvar "LOC-FRAME",9,0,LOCFRAME,0 ; false

defconst "VERSION",7,0,VERSION,71;ADRIAN_VERSION
defconst "R0",2,0,RZ,0
defconst "WBUF",4,0,WBUF,word_buffer
;defconst "DOCOL",5,0,__DOCOL,DOCOL
;defconst "DOCOL2",6,0,__DOCOL2,DOCOL2
defconst "DOVAR",5,0,__DOVAR,DOVAR
defconst "DOVAL",5,0,__DOVAL,DOVAL
defconst "DODOES",6,0,__DODOES,DODOES
defconst "F_IMMED",7,0,__F_IMMED,F_IMMED
defconst "F_HIDDEN",8,0,__F_HIDDEN,F_HIDDEN
defconst "F_LENMASK",9,0,__F_LENMASK,F_LENMASK   

defcode "?RX",3,0,QRX
; ( -- ch T | F )
    sub     rsp, 48
    mov     rcx, qword [ReadHandle]
    lea     rdx, [key_scratch]
    mov     r8, 1
    lea     r9, [Written]
    mov   qword [RSP + 4 * 8], NULL ; 5th param is NULL
    call    [ReadFile]
    add     rsp, 48
    cmp     rax, 0
    je      _ERROR
    mov     al, byte [key_scratch]
    PUSHSP  rax
    PUSHSP   -1
    NEXT
_ERROR:
    PUSHSP    0 ; FALSE
    ; TODO: include code to panic after read error
    NEXT
    
_RX:
    sub     rsp, 32 + 8 + 8
    mov     rcx, qword [ReadHandle]
    lea     rdx, [key_scratch]
    mov     r8, 1
    lea     r9, qword [Written]
    mov     qword [RSP + 4 * 8], NULL ; 5th param is NULL
    call    [ReadFile]
    add     rsp, 48
    ret

defcode "TX",2,0,TX
; ( c -- )
    POPSP   rcx
    mov     byte [emit_scratch], cl
    sub     rsp, 32 + 8 + 8
    mov     rcx, qword [WriteHandle]
    lea     rdx, [emit_scratch]
    mov     r8, 1
    lea     r9, [Written]
    mov     qword [rsp + 4*8], NULL
    call    [WriteFile]
    add     rsp, 48
    NEXT

_TX:
    mov     byte [emit_scratch], cl
    sub     rsp, 32 + 8 + 8
    mov     rcx, qword [WriteHandle]
    lea     rdx, [emit_scratch]
    mov     r8, 1
    lea     r9, [Written]
    mov     qword [rsp + 4*8], NULL
    call    [WriteFile]
    add     rsp, 48
    ret

defcode "DOCONST",7,0,DOCONST
    mov      rbx, [rsp]
    add      qword [rsp],8
    mov      rax, [rbx]
    PUSHSP   rax
    NEXT

defcode "DOVAR",5,0,DOVAR
    mov      rax, [rsp]
    add      qword [rsp],16
    add      rax, 8
    PUSHSP   rax
    NEXT

defcode "DOVAL",5,0,DOVAL
    mov      rax, [rsp]
    add      rax, 8
    PUSHSP   rax
    NEXT

defcode "DODOES",6,0,DODOES
    mov      rax, [rsp]
    add      rax, 8
    PUSHSP   rax
    add      rsp, 8
    call     rax
    NEXT

defcode "VODOES",6,0,VODOES
    NEXT

defcode "LIT",3,0,LIT
    mov rax, [rsp]
    mov rax, [rax]
    add qword [rsp],8
    PUSHSP   rax
    NEXT

defcode "EXECUTE",7,0,EXECUTE
    POPSP     rax
    jmp qword [rax]

defcode "SAYOK",5,0,SAYOK
    invoke printf, msg
    NEXT

defcode "BYE",3,0,BYE
    ;free the virtual memory
    mov     rcx, qword [VirtualBase]
    mov     rdx, 0x0
    mov     r8, 0x8000
    call    [VirtualFree]

    add     rsp, 32
    mov     rbp, [BasePtr]
    leave
    sub     rsp, 32 + 8
    xor     rcx, rcx
    call    [ExitProcess]

defcode "QUIT",4,0,QUIT
; ( -- )
; R0 RP! 0 STATE !
; S0 @ SP!
; F0 @ FP!
; BEGIN
;   QUERY SPACE
;   SOURCE INTERPRET
;   STATE @ 0= IF ." OK" THEN CR
; AGAIN ;
QUIT0:  def CR,RZ,RSPSTORE,LIT
        dq 0
        def STATE,FSTORE
        def S0,FETCH,DSPSTORE
        def F0,FETCH,FSPSTORE
QUIT1:  def QUERY,SPACE
        def SOURCE,INTERPRET
        def STATE,FETCH,LIT
        dq 0
        def EQUAL
        def ZBRANCH
        dq QUIT2-$
        def SAYOK
        def DEPTH,ZBRANCH
        dq QUIT3-$
        def LIT
        dq 58
        def EMIT,DEPTH,DOT
        def BRANCH
        dq QUIT4-$
QUIT3:  def SPACE
QUIT4:  def FDEPTH,ZBRANCH
        dq QUIT2-$
        def LIT
        dq 102
        def EMIT,LIT
        dq 58
        def EMIT
        def FDEPTH,DOT
QUIT2:  def CR,BRANCH
        dq QUIT1-$
        NEXT

defcode "BRANCH",6,0,BRANCH
        mov    rax, [rsp]
        mov    rax, [rax]
        add    [rsp], rax
        NEXT

defcode "0BRANCH",7,0,ZBRANCH
        POPSP     rax
        sub       rax, 0
        je        LBR1
        add qword [rsp], 8
        NEXT
LBR1:   
        mov     rbx, [rsp]
        mov     rbx, [rbx]
        add     [rsp], rbx
        NEXT

defcode "!",1,0,FSTORE
    POPSP    rax
    POPSP    rbx
    mov      [rax], rbx
    NEXT

defcode "@",1,0,FETCH
    POPSP    rbx
    mov      rax, [rbx]
    PUSHSP   rax
    NEXT

defcode "C!",2,0,STOREBYTE
    POPSP    rax
    POPSP    rbx
    mov      byte [rax], bl 
    NEXT

defcode "C@",2,0,FETCHBYTE
    POPSP   rax
    xor     rbx, rbx
    mov     byte  bl, [rax]
    PUSHSP  rbx
    NEXT

defcode "C,",2,0,COMMABYTE
; ( c -- )
; HERE C! 1 CHARS ALLOT ;
;    def HERE,STOREBYTE,LIT
    dq  1
;    def CHARS,ALLOT
    NEXT

defcode "RP@",3,0,RSPFETCH
    PUSHSP   RS
    NEXT

defcode "RP!",3,0,RSPSTORE
    POPSP    RS
    NEXT

defcode ">R",2,0,TOR
    POPSP    rax
    push     rax
    NEXT

defcode "R@",2,0,RFETCH
    mov      rax, [RS]
    PUSHSP   rax
    NEXT

defcode "R>",2,0,FROMR
    pop      rax
    PUSHSP   rax
    NEXT

defcode "RDROP",5,0,RDROP
    add     RS, 8
    NEXT

defcode "2RDROP",6,0,TWORDROP
    add     RS, 16
    NEXT
    
defcode "DROP",4,0,DROP
    add     DSP, 8
    NEXT

defcode "DUP",3,0,DUPF
    mov rax, [DSP]
    sub DSP, 8
    mov [DSP], rax
    NEXT

defcode "SWAP",4,0,SWAP
    POPSP     rax
    POPSP     rbx
    PUSHSP    rax
    PUSHSP    rbx
    NEXT

defcode "OVER",4,0,OVER
    mov      rax, [DSP+8]
    PUSHSP   rax
    NEXT

defcode "SP@",3,0,DSPFETCH
    PUSHSP   DSP
    NEXT

defcode "SP!",3,0,DSPSTORE
    POPSP    DSP
    NEXT

defcode "FSP@",4,0,FSPFETCH
    PUSHSP   FSP
    NEXT

defcode "FSP!",4,0,FSPSTORE
    POPSP    FSP
    NEXT

defcode "0<",2,0,ZLT
    POPSP   rax
    mov     rcx, -1
    mov     rbx, 0
    cmp     rax, rbx
    cmovl   rbx, rcx
    PUSHSP  rbx
    NEXT

defcode "AND",3,0,ANDF
    POPSP   rax
    POPSP   rbx
    and     rax, rbx
    PUSHSP  rax
    NEXT

defcode "OR",2,0,ORF
    POPSP   rax
    POPSP   rbx
    or      rax,rbx
    PUSHSP  rax
    NEXT

defcode "XOR",3,0,XORF
    POPSP   rax
    POPSP   rbx
    xor     rax,rbx
    PUSHSP  rax
    NEXT

defcode "INVERT",6,0,INVERT
    POPSP   rax
    not     rax
    PUSHSP  rax
    NEXT

defcode "UM+",3,0,UMPLUS
    POPSP   rbx
    POPSP   rax
    xor     rcx, rcx
    clc ; clear carry flag
    mov     rdx, 1
    xor     rcx, rcx
    add     rax, rbx
    cmovc   rcx, rdx
    PUSHSP  rax
    PUSHSP  rcx
    NEXT

defcode "OV+",3,0,OVPLUS
    mov     rax, 0x0
    inc     rax ; clear OF flag
    xor     rcx, rcx
    mov     rdx, 1
    POPSP   rax
    POPSP   rbx
    add     rax, rbx
    cmovo   rcx, rdx  ; tests for OF flag
    PUSHSP  rax
    PUSHSP  rcx
    NEXT

defcode "LSHIFT",6,0,LSHIFT
    POPSP   rcx
    POPSP   rax
    shl     rax, cl
    PUSHSP  rax
    NEXT

defcode "RSHIFT",6,0,RSHIFT
    POPSP   rcx
    POPSP   rax
    shr     rax, cl
    PUSHSP  rax
    NEXT

defcode "DO$",3,0,DOSTRING
; R@ DUP 8+ SWAP @ ( a u )
; R> OVER ( a u b u )
; + ALIGNED 8+ ( a u b+u+8+pad )
; >R ( put next instruction onto return stack )
        def RFETCH,DUPF,LIT
        dq  8
        def PLUS,SWAP,FETCH
        def FROMR,OVER,PLUS,ALIGNED
        def LIT
        dq  8
        def PLUS,TOR
        NEXT

defcode "LITSTRING",9,0,LITSTRING
; R@ DUP 8+ SWAP ; ( a u )
; R> OVER ( a u b u )
; + ALIGNED 8+ ( a u b+u+4+pad )
; >R ( put next instruction onto return stack )
        def RFETCH,DUPF,LIT
        dq  8
        def PLUS,SWAP,FETCH
        def FROMR,OVER,PLUS,ALIGNED
        def LIT
        dq  8
        def PLUS,TOR
        NEXT

defcode "DOP$",4,0,DOPSTRING
; R@ DUP C@ OVER ( a u a )
; + 1+ ALIGNED ( a b+u+1+pad )
; >R ( put next instruction onto return stack )
        def RFETCH,DUPF,FETCHBYTE,OVER
        def PLUS,LIT
        dq  1
        def PLUS,ALIGNED
        def TOR
        NEXT

defcode "?DUP",4,0,QDUP
; DUP IF DUP THEN ;
        mov rax, [DSP]
        cmp rax, 0
        je @f
        PUSHSP rax
@@:     NEXT

defcode "ROT",3,0,ROT       
; >R SWAP R> SWAP ;
        POPSP rax
        POPSP rbx
        POPSP rcx
        PUSHSP rbx
        PUSHSP rax
        PUSHSP rcx
        NEXT

defcode "-ROT",4,0,MROT
; ROT ROT ; 
        ;dq ROT,ROT
        ;dq EXIT
        POPSP rax
        POPSP rbx
        POPSP rcx
        PUSHSP rax
        PUSHSP rcx
        PUSHSP rbx
        NEXT

defcode "ROLL",4,0,ROLL
; ( xn ... x0  m -- xn-1 ... x0 xn )
; DUP 0<= IF DROP ELSE SWAP >R 1- RECURSE R> SWAP THEN ;
        def DUPF,ZLTEQ,ZBRANCH
        dq ROL1-$
        def DROP,BRANCH
        dq ROL2-$
ROL1:   def SWAP,TOR,ONEMINUS,ROLL,FROMR,SWAP
ROL2:   NEXT

defcode "2DROP",5,0,TWODROP
; DROP DROP ;
        ;dq DROP,DROP
        ;dq EXIT
        add DSP, 16
        NEXT

defcode "2DUP",4,0,TWODUP
; OVER OVER ;
        ;dq OVER,OVER
        ;dq EXIT
        mov rax, [DSP]
        mov rbx, [DSP+8]
        PUSHSP rbx
        PUSHSP rax
        NEXT

defcode "2OVER",5,0,TWOOVER
; >R 2DUP R> ROT ROT ;
        ;dq TOR,TWODUP,FROMR,ROT,ROT
        ;dq EXIT
        mov rax, [DSP+16]
        mov rbx, [DSP+24]
        PUSHSP rbx
        PUSHSP rax
        NEXT

defcode "2SWAP",5,0,TWOSWAP
; ROT >R ROT R>
        ;dq ROT,TOR,ROT,FROMR
        ;dq EXIT
        mov rax, [DSP]
        mov rbx, [DSP+8]
        mov rcx, [DSP+16]
        mov rdx, [DSP+24]
        mov [DSP+16], rax
        mov [DSP+24], rbx
        mov [DSP],   rcx
        mov [DSP+8], rdx
        NEXT

defcode "2>R",3,0,TWOTOR
; SWAP >R >R ;
        def FROMR,MROT,SWAP,TOR,TOR,TOR
        NEXT

defcode "2R>",3,0,TWOFROMR
; R> R> SWAP ;
        def FROMR,FROMR,FROMR,SWAP,ROT,TOR
        NEXT

defcode "2R@",3,0,TWORFETCH
; RSP; DUP ; SWAP CELL+ ; SWAP ;
        def RSPFETCH,CELLPLUS,DUPF,FETCH,SWAP
        def CELLPLUS,FETCH,SWAP
        NEXT

defcode "NOT",3,0,NOTF
; -1 XOR ;
        ;dq LIT,-1,XORF
        ;dq EXIT
        POPSP rbx
        sub rbx, 1
        sbb rbx, rbx
        PUSHSP rbx
        NEXT

defcode "NEGATE",6,0,NEGATE
; NOT 1 + ;
        ;dq NOTF,LIT,1,PLUS
        ;dq EXIT
        mov rax, [DSP]
        neg rax
        mov [DSP], rax
        NEXT

defcode "DNEGATE",7,0,DNEGATE
; NOT >R NOT 1 UM+ R> + ;
        def NOTF,TOR,NOTF,LIT
        dq 1
        def UMPLUS,FROMR,PLUS
        NEXT

defcode "+",1,0,PLUS
; UM+ DROP ;
    POPSP     rax
    ;add     rax, [DS]
    ;mov     [DS], rax
    add [DSP],rax
    NEXT    

defcode "D+",2,0,DPLUS
; >R SWAP >R UM+ R> R> + + ; 
        def TOR,SWAP,TOR,UMPLUS
        def FROMR,FROMR,PLUS,PLUS
        NEXT

defcode "-",1,0,MINUS
    POPSP   rax
    POPSP   rbx
    sub     rbx, rax 
    PUSHSP  rbx
    NEXT 

defcode "D-",2,0,DMINUS
; DNEGATE D+ ;
        def DNEGATE,DPLUS
        NEXT

defcode "ABS",3,0,ABSF
; DUP 0< IF NEGATE THEN ;
        ;dq DUPF,ZLT,ZBRANCH,ABS1
        ;dq NEGATE
;ABS1:   dq EXIT
        mov rax, [DSP]
        mov rcx, rax
        sar rcx, 63
        xor rax, rcx
        sub rax, rcx
        mov [DSP], rax
        NEXT

defcode "=",1,0,EQUAL
; ( w w -- t )
; XOR IF 0 EXIT THEN -1 ;
;        dq XORF,ZBRANCH,EQ1
;        dq LIT,0
;        dq EXIT
;EQ1:    dq LIT,-1
;        dq EXIT
        POPSP rbx
        POPSP rax
        sub rbx, rax
        sub rbx, 1
        sbb rbx, rbx
        PUSHSP rbx
        NEXT

defcode "<>",2,0,NEQUAL
; ( w w -- t )
; EQUAL INVERT ;
        ;dq EQUAL,INVERT
        ;dq EXIT
        POPSP  rbx
        POPSP  rax
        sub    rax, rbx
        neg    rax
        sbb    rbx, rbx
        PUSHSP rbx
        NEXT

defcode "U<",2,0,ULT
; ( u u -- t )
; 2DUP XOR 0< IF SWAP DROP 0< EXIT THEN - 0< ;
;        dq TWODUP,XORF,ZLT,ZBRANCH,ULT1
;        dq SWAP,DROP,ZLT,EXIT
;ULT1:   dq MINUS,ZLT
;        dq EXIT
        POPSP  rbx
        POPSP  rax
        cmp    rax, rbx
        sbb    rbx, rbx
        PUSHSP rbx
        NEXT

defcode "U>",2,0,UGT
; ( u u -- t )
        mov rdx, 0
        mov rcx, -1
        POPSP rbx
        POPSP rax
        cmp rax, rbx
        cmova rdx, rcx
        PUSHSP rdx
        NEXT

defcode "<",1,0,LTF
; ( n n -- t )
; 2DUP XOR 0< IF DROP 0< EXIT THEN - 0< ;
;        dq TWODUP,XORF,ZLT,ZBRANCH,LT1
;        dq DROP,ZLT,EXIT
;LT1:    dq MINUS,ZLT
;        dq EXIT
        mov rdx, 0
        mov rcx, -1
        POPSP rbx
        POPSP rax
        cmp rax, rbx
        cmovl rdx, rcx
        PUSHSP rdx
        NEXT
        ;jl @f
        ;push 0
        ;NEXT
;@@:     push -1
;        NEXT

defcode ">",1,0,GTF
; ( n n -- t )
; 2DUP XOR 0< IF - 0> EXIT THEN DROP 0> ;
;        dq TWODUP,EQUAL,ZBRANCH,GT1
;        dq TWODROP,LIT,0,EXIT
;GT1:    dq MINUS,ZGT
;        dq EXIT
        mov rdx, 0
        mov rcx, -1
        POPSP rbx
        POPSP rax
        cmp rax, rbx
        cmovg rdx, rcx
        PUSHSP rdx
        NEXT
;        jg @f
;        push 0
;        NEXT
;@@:     push -1
;        NEXT        

defcode ">=",2,0,GTE
; ( n n -- t )
; 2DUP > ROT ROT EQUAL OR ;
;                dq TWODUP,GTF,ROT,ROT,EQUAL,ORF
;                dq EXIT
        mov rdx, 0
        mov rcx, -1
        POPSP rbx
        POPSP rax
        cmp rax, rbx
        cmovge rdx, rcx
        PUSHSP rdx
        NEXT
;        jge @f
;        push 0
;        NEXT
;@@:     push -1
;        NEXT        

defcode "<=",2,0,LTE
; ( n n -- t )
; SWAP 2DUP > ROT ROT EQUAL OR ;
;       dq SWAP,TWODUP,GTF,ROT,ROT,EQUAL,ORF
;       dq EXIT
        mov rdx, 0
        mov rcx, -1
        POPSP rbx
        POPSP rax
        cmp rax, rbx
        cmovle rdx, rcx
        PUSHSP rdx
        NEXT
;        jle @f
;        push 0
;        NEXT
;@@:     push -1
;        NEXT        

defcode "0>",2,0,ZGT
; ( n -- t )
; DUP 0< SWAP 0= OR ;
;                dq LIT,1,MINUS,ZLT,NOTF
;                dq EXIT
        POPSP rax
        mov rbx, 0x7fffffffffffff
        dec rax
        cmp rax, rbx
        sbb rax, rax
        PUSHSP rax
        NEXT

defcode "0=",2,0,ZEQ
; ( n -- t )
; 0 = ;
;       dq LIT,0,EQUAL
;       dq EXIT
        POPSP rax
        sub rax, 1
        sbb rax, rax
        PUSHSP rax
        NEXT

defcode "0<>",3,0,ZNEQ
; ( n -- t )
; 0 = NOT ;
;       dq LIT,0,EQUAL,NOTF
;       dq EXIT
        POPSP rax
        sub rax, 1
        sbb rax, rax
        not rax
        PUSHSP rax
        NEXT

defcode "0<=",3,0,ZLTEQ
; ( n -- t )
; DUP 0= SWAP 0< OR ;
        def DUPF,ZEQ
        def SWAP,ZLT,ORF
        NEXT

defcode "D<",2,0,DLT
; ( d d -- t )
; D- NIP 0< ;
        def DMINUS,NIP,ZLT
        NEXT

defcode "D>",2,0,DGT
; ( d d -- t )
; 2SWAP D<
        def TWOSWAP,DLT
        NEXT

defcode "D=",2,0,DEQ
; ( d d -- t )
; ROT = -ROT = AND
       def ROT,EQUAL,MROT,EQUAL,ANDF
       NEXT

defcode "1+",2,0,ONEPLUS
; ( n -- n )
; 1 +
          ; dq LIT,1,PLUS
          ; dq EXIT
    ;pop rax
    ;inc rax
    ;push rax
    inc qword [DSP]
    NEXT
 
defcode "1-",2,0,ONEMINUS
; ( n -- n )
; 1 -
;                dq LIT,1,MINUS
;                dq EXIT
    ;pop rax
    ;sub rax, 1
    ;push rax
    dec qword [DSP]
    NEXT

defcode "2*",2,0,TWOTIMES
; ( n -- n )
; 2 *
;                dq LIT,2,MULF
;                dq EXIT
    mov rax,[DSP]
    add [DSP], rax
    NEXT

defcode "2/",2,0,TWODIV
; ( n -- n )
; 2 /
;                dq LIT,2,DIVF
;                dq EXIT
    POPSP rax
    sar rax, 1
    PUSHSP rax
    NEXT

defcode "MAX",3,0,MAX
; ( n n -- n )
; 2DUP < IF SWAP THEN DROP ;
;        dq TWODUP,LTF,ZBRANCH,MAX1
;        dq SWAP
;MAX1:   dq DROP
;        dq EXIT
    POPSP rbx
    POPSP rax
    cmp rbx, rax
    jg @f
    mov rbx, rax  
@@: PUSHSP rbx
    NEXT

defcode "MIN",3,0,MIN
; ( n n -- n )
; 2DUP SWAP < IF SWAP THEN DROP ;
;        dq TWODUP,SWAP,LTF,ZBRANCH,MIN1
;        dq SWAP
;MIN1:   dq DROP
;        dq EXIT
    POPSP rbx
    POPSP rax
    cmp rbx, rax
    jl @f
    mov rbx, rax
@@: PUSHSP rbx
    NEXT

defcode "WITHIN",6,0,WITHIN
; ( u ul uh -- t ) \ ul <= u < uh
; OVER - >R - R> U< ;
;        dq OVER,MINUS,TOR
;        dq MINUS,FROMR,ULT
;        dq EXIT
    POPSP rbx
    POPSP rax
    POPSP rcx
    sub rbx, rax
    sub rcx, rax
    sub rcx, rbx
    sbb rbx, rbx
    PUSHSP rbx
    NEXT

defcode "S>D",3,0,STOD
; ( u -- d )
; DUP 0< ;
        def DUPF,ZLT
        NEXT

defcode "D>S",3,0,DTOS
; ( d -- u )
; DROP ;
        def DROP
        NEXT

defcode "DABS",4,0,DABS
; ( d -- |d| )
; DUP 0< IF DNEGATE THEN ;
        def DUPF,ZLT,ZBRANCH
        dq DAB1-$
        def DNEGATE
DAB1:   NEXT

defcode "SM/REM",6,0,SMDIVREM
        POPSP     rbx
        POPSP     rdx
        POPSP     rax
        idiv      rbx
        PUSHSP    rdx
        PUSHSP    rax
        NEXT

defcode "UM/MOD",6,0,UMDIVMOD
; ( ud u -- ur uq ) divides an unsigned double by a unsigned single int
; 2DUP U< 
; IF NEGATE   32  
;  BEGIN DUP 
;  WHILE >R >R DUP um+ >R >R DUP um+ R> + 
;    dup r> r; swap >r um+ r> or
;    IF >R DROP 1 + R> ELSE DROP THEN R> R> 1 - 
;  REPEAT 2DROP SWAP EXIT 
; THEN DROP 2DROP -1 DUP ; 
;        dq TWODUP,ULT
;        dq ZBRANCH,UMMODA
;        dq NEGATE,LIT,64
;UMBEG:  dq DUPF,ZBRANCH,UMREP
;        dq TOR,TOR,DUPF,UMPLUS,TOR,TOR,DUPF,UMPLUS,FROMR,PLUS
;        dq DUPF,FROMR,RFETCH,SWAP,TOR,UMPLUS,FROMR,ORF
;        dq ZBRANCH,UMELSE
;        dq TOR,DROP,LIT,1,PLUS,FROMR,BRANCH,UMTHEN
;UMELSE: dq DROP
;UMTHEN: dq FROMR,FROMR,LIT,1,MINUS
;        dq BRANCH,UMBEG
;UMREP:  dq TWODROP,SWAP,EXIT
;UMMODA: dq DROP,TWODROP,LIT,-1,DUPF
;        dq EXIT
    mov rdx, rcx
    POPSP rbx
    POPSP rdx
    POPSP rax
    div rbx
    PUSHSP rdx
    ;mov rbx, rax
    mov rdx, rcx
    PUSHSP rax
    NEXT

defcode "UD/MOD",6,0,UDDIVMOD
; ( ud u -- u u )
; >R 0 R; UM/MOD ROT ROT R> UM/MOD ROT ;
        def TOR,LIT
        dq 0
        def RFETCH,UMDIVMOD
        def ROT,ROT,FROMR,UMDIVMOD,ROT
        NEXT
        
defcode "FM/MOD",6,0,FMDIVMOD
; floored division
; ( d n -- r q )
; DUP 0< DUP >R
; IF NEGATE >R DNEGATE R>
; THEN >R DUP 0< IF R; + THEN R> UM/MOD R>
; IF SWAP NEGATE SWAP THEN ;
        def DUPF,ZLT,DUPF,TOR
        def ZBRANCH
        dq MMOD1-$
        def NEGATE,TOR,DNEGATE,FROMR
MMOD1:  def TOR,DUPF,ZLT,ZBRANCH
        dq MMOD2-$
        def RFETCH,PLUS
MMOD2:  def FROMR,UMDIVMOD,FROMR
        def ZBRANCH
        dq MMOD3-$
        def SWAP,NEGATE,SWAP
MMOD3:  NEXT

defcode "/MOD",4,0,DIVMOD
; ( n n -- r q )
; OVER 0< SWAP M/MOD ;
        def OVER,ZLT,SWAP,FMDIVMOD
        NEXT

;defcode "MOD",3,0,MODF
; ( n n  -- r )
; /MOD DROP ;
;        dq DIVMOD,DROP
;        dq EXIT
defcode "MOD",3,0,MODF
        POPSP rbx
        mov rdx, 0
        POPSP rax
        div rbx
        PUSHSP rdx
        NEXT

;defcode "/",1,0,DIVF
; ( n n -- q )
; /MOD SWAP DROP ;
;        dq DIVMOD,SWAP,DROP
;        dq EXIT
defcode "/",1,0,DIVF
        POPSP rbx
        mov rdx, 0
        POPSP rax
        div rbx
        PUSHSP rax
        NEXT

defcode "UM*",3,0,UMMUL
; ( u u -- ud )
; 0 SWAP ( u1 0 u2 ) 32
; BEGIN DUP
; WHILE >R DUP UM+ >R >R DUP UM+ R> + R>
;  IF >R OVER UM+ R> + THEN R> 1 -
; REPEAT DROP >R NIP R> ;
        def LIT
        dq 0
        def SWAP,LIT
        dq 64
MBEG:   def DUPF,ZBRANCH
        dq MEND-$
        def TOR,DUPF,UMPLUS,TOR,TOR
        def DUPF,UMPLUS,FROMR,PLUS,FROMR
        def ZBRANCH
        dq MTH1-$
        def TOR,OVER,UMPLUS,FROMR,PLUS
MTH1:   def FROMR,LIT
        dq 1
        def MINUS
        def BRANCH
        dq MBEG-$
MEND:   def DROP,TOR,NIP,FROMR
        NEXT

defcode "*",1,0,MULF
; ( n n -- n )
; UM* DROP ;
        def UMMUL,DROP
        NEXT

defcode "M*",2,0,MMUL
; ( n n -- d )
; 2DUP XOR 0< >R ABS SWAP ABS 
; UM* R> IF NEGATE THEN ;
        def TWODUP,XORF,ZLT,TOR
        def ABSF,SWAP,ABSF
        def UMMUL,FROMR
        def ZBRANCH
        dq MMUL1-$
        def DNEGATE
MMUL1:  NEXT

defcode "*/MOD",5,0,MULDIVMOD
; ( n n n -- r q )
; >R M* R> M/MOD ;
        def TOR,MMUL,FROMR,FMDIVMOD
        NEXT

defcode "*/",2,0,MULDIV
; ( n n n -- q )
; */MOD SWAP DROP ;
        def MULDIVMOD,SWAP,DROP
        NEXT

defcode "CELL-",5,0,CELLMINUS
; ( a -- a)
; -4 + ;
        def LIT
        dq -8
        def PLUS
        NEXT

defcode "CELL+",5,0,CELLPLUS
; ( a -- a )
; 8 + ;
        def LIT
        dq 8
        def PLUS
        NEXT

defcode "FLOAT+",6,0,FLOATPLUS
; ( a -- a )
; 8 + ;
        def CELLPLUS
        NEXT

defcode "FLOATS",6,0,FLOATS
; ( n1 -- n2 )
        def LIT
        dq 8
        def MULF
        NEXT
    
defcode "CELLS",5,0,CELLS
; ( n -- n )
; 4 * ;
        def LIT
        dq 8
        def MULF
        NEXT

defcode "CELL",4,0,CELL
; ( -- n )
; 4 ;
        def LIT
        dq 8
        NEXT

defcode "ALIGNED",7,0,ALIGNED
; ( b -- a )
; 3 + 3 INVERT AND ;
        def LIT
        dq 7
        def PLUS,LIT
        dq 7
        def INVERT,ANDF
        NEXT

defcode "ALIGN",5,0,ALIGNF
; ( -- )
; HERE ALIGNED DP !
        def HERE,ALIGNED,FDP,FSTORE
        NEXT

defcode "FALIGNED",8,0,FALIGNED
; ( b -- a )
; ALIGNED ;
        def ALIGNED
        NEXT

defcode "FALIGN",6,0,FALIGNF
; ( -- )
; ALIGN ;
        def ALIGNF
        NEXT
        
defcode "BL",2,0,BLF
; ( -- 32 )
; 32 ;
        def LIT
        dq 32
        NEXT

defcode ">CHAR",5,0,TOCHAR
; 0x7F AND DUP 127 BL WITHIN
; IF DROP 95 THEN ;
        def LIT
        dq 0x7F
        def ANDF,DUPF
        def LIT
        dq 127
        def BLF,WITHIN
        def ZBRANCH
        dq CHAR1-$
        def DROP,LIT
        dq 95
CHAR1:  NEXT

defcode "CHARS",5,0,CHARS
; ( n -- n )
; 1 * ; actually perform a noop and compile nothing
        NEXT

defcode "CHAR+",5,0,CHARPLUS
; ( n -- n )
; 1 + ; 
        def ONEPLUS
        NEXT

defcode "DEPTH",5,0,DEPTH
; ( -- n ) 
; DSP; S0 ; SWAP - 4 / ;
        def DSPFETCH,S0,FETCH
        def SWAP,MINUS,LIT
        dq 8
        def DIVF
        NEXT

defcode "PICK",4,0,PICK
; ( +n -- w )
; 1 + CELLS DSP; + ; ;
        def LIT
        dq 1
        def PLUS,CELLS
        def DSPFETCH,PLUS,FETCH
        NEXT

defcode "TUCK",4,0,TUCK
; ( n m -- m n m )
; SWAP OVER ;
        def SWAP,OVER
        NEXT

defcode "NIP",3,0,NIP
; ( n m -- m )
; SWAP DROP ;
        def SWAP,DROP
        NEXT

defcode "+!",2,0,PLUSSTORE
; ( n a -- ) 
; SWAP OVER ; + SWAP ! ;
        def SWAP,OVER,FETCH,PLUS,SWAP,FSTORE
        NEXT

defcode "2!",2,0,TWOSTORE
; ( d a -- )
; SWAP OVER ! CELL+ ! ;
        def SWAP,OVER,FSTORE
        def CELLPLUS,FSTORE
        NEXT

defcode "2@",2,0,TWOFETCH
; ( a -- d )
; DUP CELL+ ; SWAP ; ;
        def DUPF,CELLPLUS,FETCH
        def SWAP,FETCH
        NEXT

defcode "COUNT",5,0,COUNT
; ( b -- b +n ) 
; DUP 1 + SWAP C; ;
        def DUPF,LIT
        dq 1
        def PLUS
        def SWAP,FETCHBYTE
        NEXT

defcode "HERE",4,0,HERE
; ( -- a )
; DP ; ;
        def FDP,FETCH
        NEXT

defcode "ALLOT",5,0,ALLOT
; ( n -- )
; DP +! ;
        def FDP,PLUSSTORE
        NEXT

defcode "FREE",4,0,FREE
        NEXT
defcode "RESIZE",6,0,RESIZE
        NEXT

defcode "PAD",3,0,PAD
; ( -- a )
; HERE 80 + ;
        def HERE,LIT
        dq 80
        def PLUS
        NEXT

;defcode "TIB",3,,TIB
;; ( -- a )
;; TIB ;
;        dq LIT,buffer
;        dq EXIT

defcode "CMOVE",5,0,CMOVEF
; ( b b u -- )
; BEGIN
;    ?DUP    ( b1 b2 u u)
;    WHILE
;        >R >R  ( b1 | u b2 )
;        DUP ( b1 b1 | u b2 )
;        C@  ( b1 c  | u b2 )
;        R@  ( b1 c b2 | u b2 )
;        C!  ( b1 | u b2 )
;        1 + ( b1 | u b2 )
;        R> 1 + 
;        R> ( b1 b2 u )
;        1 - 
;    REPEAT  ( b1 b2 0 )
;    2DROP ;
CMBEG1: def QDUP,ZBRANCH
        dq CMEND-$
        def TOR,TOR,DUPF,FETCHBYTE
        def RFETCH,STOREBYTE,LIT
        dq 1
        def PLUS
        def FROMR,LIT
        dq 1
        def PLUS
        def FROMR,LIT
        dq 1
        def MINUS
        def BRANCH
        dq CMBEG1-$
CMEND:  def TWODROP
        NEXT
        
defcode "CMOVE>",6,0,CMOVETO
; Like above but proceeds from high memory to low
; ( b1 b2 u -- )
; DUP >R + 1- SWAP R; + 1- SWAP R>  ( b1+u-1 b2+u-1 u )
; BEGIN
;    ?DUP    ( b1 b2 u u)
;    WHILE
;        >R >R  ( b1 | u b2 )
;        DUP ( b1 b1 | u b2 )
;        C@  ( b1 c  | u b2 )
;        R@  ( b1 c b2 | u b2 )
;        C!  ( b1 | u b2 )
;        1 - ( b1 | u b2 )
;        R> 1 - 
;        R> ( b1 b2 u )
;        1 - 
;    REPEAT  ( b1 b2 0 )
;    2DROP ;
         def DUPF,TOR,PLUS,ONEMINUS,SWAP
         def RFETCH,PLUS,ONEMINUS
         def SWAP,FROMR
CMBEGT:  def QDUP,ZBRANCH
         dq CMENDT-$
         def TOR,TOR,DUPF,FETCHBYTE
         def RFETCH,STOREBYTE,LIT
         dq 1
         def MINUS
         def FROMR,LIT
         dq 1
         def MINUS
         def FROMR,LIT
         dq 1
         def MINUS
         def BRANCH
         dq CMBEGT-$
CMENDT:  def TWODROP
         NEXT

defcode "COMPARE",7,0,COMPARE
; ( a1 u1 a2 u2 -- n )
  ; >r swap >r swap ( a2 a1 | r: u2 u1 )
  ; begin
  ;   2r> 2dup 1- swap 1- swap 2>r 0= swap 0= or
  ;   if
  ;     2drop
  ;     2r> 2dup < if 1 exit then  
  ;     2dup > if -1 exit then
  ;     0  exit then
  ;   2dup c@ swap c@ =
  ; while
  ;   1+ swap 1+ swap
  ; repeat
  ; c@ swap c@ swap -
  ; 0> if -1 else 1 then
  ; rdrop rdrop ; 
    def TOR,SWAP,TOR,SWAP
cmpb:
    def TWOFROMR,TWODUP,ONEMINUS,SWAP
    def ONEMINUS,SWAP,TWOTOR,ZEQ
    def SWAP,ZEQ,ORF,ZBRANCH
    dq CMP16$
    def TWODROP,TWOFROMR,TWODUP,LTF,ZBRANCH
    dq CMP4-$
    def TWODROP,LIT
    dq 1
    NEXT
CMP4:
    def TWODUP,GTF,ZBRANCH
    dq CMP5-$
    def TWODROP,LIT
    dq -1
    NEXT
CMP5:
    def TWODROP,LIT
    dq 0
    NEXT
CMP1:
    def TWODUP,FETCHBYTE,SWAP,FETCHBYTE
    def EQUAL,ZBRANCH
    dq cmpe-$
    def ONEPLUS,SWAP,ONEPLUS,SWAP
    def BRANCH
    dq cmpb-$
cmpe:
    def FETCHBYTE,SWAP,FETCHBYTE,SWAP,MINUS
    def ZGT,ZBRANCH
    dq CMP2-$
    def LIT
    dq -1
    def BRANCH
    dq CMP3-$
CMP2:
    def LIT
    dq 1
CMP3:
    def TWORDROP
    NEXT

defcode "FILL",4,0,FILL
;  ( b u c -- )
; SWAP >R SWAP BEGIN R; WHILE   ( c b | u )
; 2DUP C! 1 + R> 1 - >R REPEAT
; 2DROP R> DROP ;
        def SWAP,TOR,SWAP
FBEG:   def RFETCH,ZBRANCH
        dq FEND-$
        def TWODUP,STOREBYTE
        def LIT
        dq 1
        def PLUS
        def FROMR,LIT
        dq 1
        def MINUS,TOR
        def BRANCH
        dq FBEG-$
FEND:   def TWODROP,FROMR,DROP
        NEXT

defcode "ERASE",5,0,ERASE
;  ( b u -- )
; 0 SWAP >R SWAP BEGIN R; WHILE   ( c b | u )
; 2DUP C! 1 + R> 1 - >R REPEAT
; 2DROP R> DROP ;
        def BLF,SWAP,TOR,SWAP
EBEG:   def RFETCH,ZBRANCH
        dq EEND-$
        def TWODUP,STOREBYTE
        def LIT
        dq 1
        def PLUS
        def FROMR,LIT
        dq 1
        def MINUS,TOR
        def BRANCH
        dq EBEG-$
EEND:   def TWODROP,FROMR,DROP
        NEXT

defcode "-TRAILING",9,0,MTRAILING  
; ( c-addr u1 -- c-addr u2 , strip trailing blanks )
;  DUP 0>
;  IF
;  BEGIN
;    2DUP 1- CHARS + C; BL =
;    OVER 0> AND
;  WHILE
;    1-
;  REPEAT
;  THEN ;
        def DUPF,ZGT,ZBRANCH
        dq TRAIL1-$
TRABEG: def TWODUP,LIT
        dq 1
        def MINUS,CHARS,PLUS
        def FETCHBYTE,BLF,EQUAL
        def OVER,ZGT,ANDF
        def ZBRANCH
        dq TRAEND-$
        def LIT
        dq 1
        def MINUS
        def BRANCH
        dq TRABEG-$
TRAEND: 
TRAIL1: NEXT

defcode "PACK$",5,0,PACKS
; ( b u a -- a ) null fill
; DUP >R 2DUP C! 1+ SWAP CMOVE R> ;
        def DUPF,TOR,TWODUP,STOREBYTE
        def ONEPLUS,SWAP,CMOVEF
        def FROMR
        NEXT

defcode "DIGIT",5,0,DIGIT
; ( u -- c )
; 9 OVER < 7 AND + 48 + ;
     def LIT
     dq 9
     def OVER,LTF,LIT
     dq 7
     def ANDF,PLUS
     def LIT
     dq 48
     def PLUS
     NEXT

defcode "EXTRACT",7,0,EXTRACT
; ( n base -- n c ) 
; 0 SWAP UM/MOD SWAP DIGIT ;
     def LIT
     dq 0
     def SWAP,UMDIVMOD
     def SWAP,DIGIT
     NEXT

defcode "HOLD",4,0,HOLD
; ( c -- ) 
; HLD @ 1 - DUP HLD ! C! ;
     def HLD,FETCH,LIT
     dq 1
     def MINUS,DUPF,HLD
     def FSTORE,STOREBYTE
     NEXT

defcode "HOLDS",5,0,HOLDS
; ( a u -- ) 
; BEGIN DUP WHILE 1- 2DUP + C@ HOLD REPEAT 2DROP ;
HOLDB:
        def DUPF,ZBRANCH
        dq HOLDE-$
        def ONEMINUS,TWODUP,PLUS,FETCHBYTE,HOLD
        def BRANCH
        dq HOLDB-$
HOLDE:
        def TWODROP
        NEXT

defcode "<#",2,0,BRAHASH
; ( -- )
; PAD HLD ! ;
        def PAD,HLD,FSTORE
        NEXT

defcode "#",1,0,HASH
; ( ud -- u ) 
;; BASE ; EXTRACT HOLD ;
; BASE ; UD/MOD ROT DIGIT HOLD
        ;dq BASE,FETCH,EXTRACT,HOLD
        def BASE,FETCH,UDDIVMOD
        def ROT,DIGIT,HOLD
        NEXT

defcode "#S",2,0,HASHS
; ( u -- 0 )
;; BEGIN # DUP WHILE REPEAT ;
; BEGIN # 2DUP OR 0= UNTIL ;
HASBEG: def HASH,TWODUP,ORF,LIT,0,EQUAL
        def ZBRANCH
        dq HASBEG-$
HASEND: NEXT

defcode "SIGN",4,0,SIGN
; ( n -- )
; 0< IF 45 HOLD THEN ;
        def ZLT,ZBRANCH
        dq SIGN1-$
        def LIT
        dq 45
        def HOLD
SIGN1:  NEXT

defcode "#>",2,0,HASHBRA
; ( ud -- b u )
; 2DROP HLD ; PAD OVER - ;
     def TWODROP,HLD,FETCH
     def PAD,OVER,MINUS
     NEXT

defcode "STR",3,0,STRF
; ( d -- b u )
; DUP >R DABS <# #S R> SIGN #> ;
        def DUPF,TOR,DABS,BRAHASH
        def HASHS,FROMR,SIGN,HASHBRA
        NEXT

defcode "HEX",3,0,HEX
; ( -- )
; 16 BASE ! ;
        def LIT
        dq 16
        def BASE,FSTORE
        NEXT

defcode "DECIMAL",7,0,DECIMAL
; ( -- )
; 10 BASE ! ;
        def LIT
        dq 10
        def BASE,FSTORE
        NEXT

defcode "BINARY",6,0,BINARY
; ( -- )
; 2 BASE ! ;
        def LIT
        dq 2
        def BASE,FSTORE
        NEXT

defcode ".R",2,0,DOTR
; ( n +n -- ) display a signed int in
; n columns, right justified
; >R 0 str
; R> OVER - SPACES TYPE
        def TOR,LIT
        dq 0
        def STRF
        def FROMR,OVER,MINUS,LIT
        dq 0
        def MAX,SPACES,TYPEF
        NEXT

defcode "U.R",3,0,UDOTR
; ( u +n -- )
; >R <# 0 #S #> R>
; OVER - SPACES TYPE ;
        def TOR,BRAHASH,LIT
        dq 0
        def HASHS,HASHBRA
        def FROMR,OVER,MINUS,LIT
        dq 0
        def MAX,SPACES,TYPEF
        NEXT

defcode "U.",2,0,UDOT
; ( u -- )
; <# 0 #S #> TYPE SPACE ;
        def BRAHASH,LIT
        dq 0
        def HASHS,HASHBRA
        def TYPEF,SPACE
        NEXT

defcode ".",1,0,DOT
; ( n -- ) 
; <# DUP ABS 0 #S ROT SIGN #> TYPE SPACE ;
        def DEPTH,LIT
        dq 0
        def NEQUAL,ZBRANCH
        dq DOT1-$
        def BRAHASH,DUPF,ABSF,LIT
        dq 0
        def HASHS,ROT,SIGN,HASHBRA
        def TYPEF,SPACE,BRANCH
        dq DOT2-$
DOT1:   def LIT
        dq word2
        def COUNT,TYPEF,SPACE
DOT2:   NEXT

defcode "D.",2,0,DDOT
; ( d -- ) 
; TUCK DABS <# #S ROT SIGN #>
; TYPE SPACE ;
        def DEPTH,LIT
        dq 0
        def NEQUAL,ZBRANCH
        dq DOT3-$
        def TUCK,DABS,BRAHASH,HASHS,ROT,SIGN,HASHBRA
        def TYPEF,SPACE,BRANCH
        dq DOT4-$
DOT3:   def LIT
        dq word2
        def COUNT,TYPEF,SPACE
DOT4:   NEXT

defcode "UD.",3,0,UDDOT
; ( ud -- ) 
; <# #S #>
; TYPE SPACE ;
        def DEPTH,LIT
        dq 0
        def NEQUAL,ZBRANCH
        dq DOT5-$
        def BRAHASH,HASHS,HASHBRA
        def TYPEF,SPACE,BRANCH
        dq DOT6-$
DOT5:   def LIT
        dq word2
        def COUNT,TYPEF,SPACE
DOT6:   NEXT

defcode "UD.R",4,0,UDDOTR
; ( ud n -- ) 
; >R <# #S #> R> OVER - SPACES TYPE ;
        def DEPTH,LIT
        dq 0
        def NEQUAL,ZBRANCH
        dq DOT7-$
        def TOR,BRAHASH,HASHS,HASHBRA
        def FROMR,OVER,MINUS,SPACES
        def TYPEF,BRANCH
        dq DOT8-$
DOT7:   def LIT
        dq word2
        def COUNT,TYPEF,SPACE
DOT8:   NEXT

defcode "D.R",3,0,DDOTR
; ( ud n -- ) 
; >R TUCK DABS <# #S ROT SIGN #> R> OVER - SPACES TYPE ;
        def DEPTH,LIT
        dq 0
        def NEQUAL,ZBRANCH
        dq DOT9-$
        def TOR,TUCK,DABS,BRAHASH,HASHS
        def ROT,SIGN,HASHBRA
        def FROMR,OVER,MINUS,SPACES
        def TYPEF,BRANCH
        dq DOT10-$
DOT9:   def LIT
        dq word2
        def COUNT,TYPEF,SPACE
DOT10:  NEXT

defcode "UWIDTH",6,0,UWIDTH 
; ( u -- width )
; BASE ; / ( rem quot )
; ?DUP IF  ( if quotient <> 0 then )
;  RECURSE 1+ ( return 1+recursive call )
; ELSE
;  1  ( return 1 )
; THEN
;;
        def BASE,FETCH,DIVF
        def QDUP,ZBRANCH
        dq UW1-$
        def UWIDTH,LIT
        dq 1
        def PLUS
        def BRANCH
        dq UW2-$
UW1:    def LIT
        dq 1
UW2:    NEXT

defcode ".S",2,0,DOTS
; ( -- )
; ." <"
; DEPTH .
; ." > "
; \ DSP;
; S0 @ 4 -
; BEGIN
; \ DUP S0 ; <
; DUP DSP; 4+ >
; WHILE
;  DUP @ . ( print the stack element )
; \ SPACE
; \ 4+
;  4 -
; REPEAT
; DROP ;
        def LIT
        dq 60
        def EMIT
        def DEPTH,UDOT
        def LIT
        dq 8
        def EMIT
        def LIT
        dq 62
        def EMIT,SPACE
        def S0,FETCH,LIT
        dq 8
        def MINUS
DSBEG:  def DUPF,DSPFETCH,LIT
        dq 8
        def PLUS
        def GTF,ZBRANCH
        dq DSEND-$
        def DUPF,FETCH,DOT
        def LIT
        dq 8
        def MINUS
        def BRANCH
        dq DSBEG-$
DSEND:  def DROP
        NEXT

defcode "?",1,0,QUEST
; ( a -- )
; ; . ; display the contents in a cell
        def FETCH,DOT
        NEXT

defcode "DIGIT?",6,,DIGITQ
; ( c  -- u t )
; BASE ; >R 48 - 9 OVER <
; IF 7 - DUP 10 < OR THEN DUP R> U< ;
        def BASE,FETCH
        def TOR,LIT
        dq 48
        def MINUS
        def LIT
        dq 9
        def OVER,LTF
        def ZBRANCH
        dq DIGI1-$
        def LIT
        dq 7
        def MINUS,DUPF,LIT
        dq 10
        def LTF,ORF
DIGI1:  def DUPF,FROMR,ULT
        NEXT

defcode "S>NUMBER?",9,0,STONUMBERQ
; ( a u -- n -1 | a 0 )
;    over c; ( caddr u c ) 
;    dup 45 = 
;    IF  ( negative )
;        1 >r ( sign flag )
;        drop
;        swap 1 + swap 1-
;        over c; ( caddr u c )
;    ELSE ( positive )
;        0 >r ( sign flag )
;    THEN
;    48 -
;    0 <
;    IF
;        ( END - NaN )
;        2drop drop
;        r>  ( result )
;        r>  ( sign flag )
;        IF NEGATE THEN
;        0
;        EXIT
;    ELSE
;        ( start processing )
;        ( caddr u )
;        0 >r ( accumulator )
;        begin
;            over c;
;            >r
;            swap 1 + swap
;            1 - 
;            r>
;            48 -
;            ( caddr+1 u-1 c )
;            over 0 >=
;            over 10 < AND
;            dup NOT IF 0 EXIT THEN
;        while
;            r> base ; * >r
;            dup 9 >
;            IF
;                ( hex )
;                0x11 - ( 17 in hex )
;                dup
;                0 <
;                IF
;                    ( NaN - END )
;                    2drop drop
;                    r> base ; / ( result )
;                    r> ( sign flag )
;                    IF NEGATE THEN
;                    0
;                    EXIT
;                THEN
;                0xA + ( 10 in hex )
;            THEN
;            dup base ; >
;            IF
;                ( bigger than base - END )
;                2drop drop
;                r> base ; / ( result )
;                r>  ( sign flag )
;                IF NEGATE THEN
;                0
;                EXIT
;            THEN
;            r> +  >r
;        repeat
;    THEN
;    2drop drop
;    r>  ( result )
;    r>  ( sign flag )
;    IF NEGATE THEN
;    -1 ;
        def OVER,FETCHBYTE,DUPF,LIT
        dq 45
        def EQUAL,ZBRANCH
        dq TN1-$
        def LIT
        dq 1
        def TOR,DROP,SWAP,LIT
        dq 1
        def PLUS
        def SWAP,LIT
        dq 1
        def MINUS,OVER,FETCHBYTE
        def BRANCH
        dq TN2-$
TN1:    def LIT
        dq 0
        def TOR
TN2:    def LIT
        dq 48
        def MINUS,LIT
        dq 0
        def LTF
        def ZBRANCH
        dq TN3-$
        def TWODROP,DROP,FROMR,FROMR
        def ZBRANCH
        dq TN4-$
        def NEGATE
TN4:    def LIT
        dq 0
        NEXT
        def BRANCH
        dq TN10-$
TN3:    def LIT
        dq 0
        def TOR
TNBEG:  def OVER,FETCHBYTE,TOR,SWAP
        def LIT
        dq 1
        def PLUS,SWAP,LIT
        dq 1
        def MINUS
        def FROMR
        def LIT
        dq 48
        def MINUS,OVER,LIT
        dq 0
        def GTE
        def OVER,LIT
        dq 10
        def LTF,ANDF
        def ZBRANCH
        dq TNEND-$
        def FROMR,BASE,FETCH,MULF,TOR
        def DUPF,LIT
        dq 9
        def GTF,ZBRANCH
        dq TN5-$
        def LIT
        dq 17
        def MINUS,DUPF,LIT
        dq 0
        def LTF
        def ZBRANCH
        dq TN6-$
        def TWODROP,DROP,FROMR,BASE,FETCH,DIVF
        def FROMR
        def ZBRANCH
        dq TN7-$
        def NEGATE
TN7:    def LIT
        dq 0
        NEXT
TN6:    def LIT
        dq 10
        def PLUS
TN5:    def DUPF,BASE,FETCH,GTF
        def ZBRANCH
        dq TN8-$
        def TWODROP,DROP,FROMR,BASE,FETCH
        def DIVF,FROMR
        def ZBRANCH
        dq TN9-$
        def NEGATE
TN9:    def LIT
        dq 0
        NEXT
TN8:    def FROMR,PLUS,TOR
        def BRANCH
        dq TNBEG-$
TNEND:
TN10:   def TWODROP,DROP,FROMR,FROMR
        def ZBRANCH
        dq TN11-$
        def NEGATE
TN11:   def LIT
        dq -1
        NEXT

defcode "ASCII",5,F_IMMED,ASCII
; ( <char> -- char , state smart )
; parse-name drop c;
; state ;
; IF [compile] literal
; THEN ; immediate
        def PARSENAME,DROP,FETCHBYTE,STATE,FETCH
        def ZBRANCH
        dq ASC1-$
        def LITERAL
ASC1:   NEXT

defcode ">DIGIT",6,0,TODIGIT
; ( char base -- n true | char false )
; >r \ convert lower to upper
; dup ascii a < not
; IF
;   ascii a - ascii A +
; THEN
;   dup dup ascii A 1- >
; IF ascii A - ascii 9 + 1+
; ELSE ( char char )
;    dup ascii 9 >
;    IF
;    ( between 9 and A is bad )
;      drop 0 ( trigger error below )
;    THEN
; THEN
;    ascii 0 -
;    dup r> <
;    IF dup 1+ 0>
;      IF nip true
;      ELSE drop FALSE
;      THEN
;    ELSE drop FALSE
;    THEN   ;
        def TOR,DUPF,LIT
        dq 97
        def LTF,NOTF
        def ZBRANCH
        dq DIG1-$
        def LIT
        dq 97
        def MINUS,LIT
        dq 65
        def PLUS
DIG1:   def DUPF,DUPF,LIT
        dq 65
        def ONEMINUS,GTF
        def ZBRANCH
        dq DIG2-$
        def LIT
        dq 65
        def MINUS,LIT
        dq 57
        def PLUS,ONEPLUS
        def BRANCH
        dq DIG3-$
DIG2:   def DUPF,LIT
        dq 57
        def GTF
        def ZBRANCH
        dq DIG4-$
        def DROP,LIT
        dq 0
DIG4:
DIG3:   def LIT
        dq 48
        def MINUS,DUPF,FROMR,LTF
        def ZBRANCH
        dq DIG5-$
        def DUPF,ONEPLUS,ZGT
        def ZBRANCH
        dq DIG7-$
        def NIP,LIT
        dq -1
        def BRANCH
        dq DIG8-$
DIG7:   def DROP,LIT
        dq 0
DIG8:
        def BRANCH
        dq DIG6-$
DIG5:   def DROP,LIT
        dq 0
DIG6:   NEXT

defcode ">NUMBER",7,0,TONUMBER
; ( ud1 c-addr1 u1 -- ud2 c-addr2 u2 , convert till bad char , CORE )
; >r
; BEGIN
;   r; 0>    \ any characters left?
;   IF
;    dup c; base ;
;    digit ( ud1 c-addr , n true | char false )
;    IF
;      TRUE
;    ELSE
;      drop FALSE
;    THEN
;   ELSE
;    false
;   THEN
; WHILE ( -- ud1 c-addr n  )
;    swap >r  ( -- ud1lo ud1hi n  )
;    swap  base ; ( -- ud1lo n ud1hi base  )
;    um* drop ( -- ud1lo n ud1hi*baselo  )
;    rot  base ; ( -- n ud1hi*baselo ud1lo base )
;    um* ( -- n ud1hi*baselo ud1lo*basello ud1lo*baselhi )
;    d+  ( -- ud2 )
;    r> 1+     \ increment char*
;    r> 1- >r  \ decrement count
; REPEAT
;    r> ;
        def TOR
TNUB:   def RFETCH,ZGT
        def ZBRANCH
        dq TNU1-$
        def DUPF,FETCHBYTE,BASE,FETCH,TODIGIT
        def ZBRANCH
        dq TNU3-$
        def LIT
        dq -1
        def BRANCH
        dq TNU2-$
TNU3:   def DROP,LIT
        dq 0
        def BRANCH
        dq TNU2-$
TNU1:   def LIT
        dq 0
TNU2:   def ZBRANCH
        dq TNUR-$
        def SWAP,TOR,SWAP,BASE,FETCH
        def UMMUL,DROP,ROT,BASE,FETCH
        def UMMUL,DPLUS,FROMR,ONEPLUS
        def FROMR,ONEMINUS,TOR
        def BRANCH
        dq TNUB-$
TNUR:   def FROMR
        NEXT

defcode "ISFLOAT?",8,0,ISFLOATQ
; ( a u -- f )
; 0 >R  \ flag
; BEGIN
;       ?DUP
; WHILE
;       OVER C; DUP >R -33 AND 69 = 
;       R; DIGIT? SWAP DROP OR
;       R; 45 = OR
;       R; 43 = OR
;       NOT IF
;               2DROP 0 2RDROP EXIT     
;       THEN
;       R> 69 = R> OR >R
;       SWAP 1+ SWAP 1-
; REPEAT
; DROP R> ;
        def LIT
        dq 0
        def TOR
ISFB:   def QDUP,ZBRANCH
        dq ISFE-$
        def OVER,FETCHBYTE,DUPF,TOR,LIT
        dq -33
        def ANDF
        def LIT
        dq 69
        def EQUAL
        def RFETCH,DIGITQ,SWAP,DROP,ORF
        def RFETCH,LIT
        dq 43
        def EQUAL,ORF
        def RFETCH,LIT
        dq 45
        def EQUAL,ORF
        def RFETCH,LIT
        dq 46
        def EQUAL,ORF
        def NOTF,ZBRANCH
        dq ISF1-$
        def TWODROP,LIT
        dq 0
        def RDROP,RDROP
        NEXT
ISF1:   def FROMR,LIT
        dq 69
        def EQUAL,FROMR,ORF,TOR
        def SWAP,ONEPLUS,SWAP,ONEMINUS
        def BRANCH
        dq ISFB-$
ISFE:   def DROP,FROMR
        NEXT

defcode "ISHEX?",6,0,ISHEXQ
; ( a u -- f )
; 2 > 
; IF ( a )
;   DUP C@ [CHAR] 0 = 
;   SWAP 1+ C@ [CHAR] x =
;   AND EXIT
; THEN DROP 0 ;
        def LIT
        dq 2
        def GTF,ZBRANCH
        dq ISH1-$
        def DUPF,FETCHBYTE,LIT
        dq 48
        def EQUAL
        def SWAP,ONEPLUS,FETCHBYTE,LIT
        dq 88
        def EQUAL
        def ANDF
        NEXT
ISH1:   def DROP,LIT
        dq 0
        NEXT

defcode "skip-digits",11,F_HIDDEN,skipDigs
; ( a u -- a u )
; BEGIN
;       OVER C@ DIGIT? SWAP DROP
; WHILE
;       SWAP 1+ SWAP 1-
; REPEAT
skipb:  def OVER,FETCHBYTE,DIGITQ,SWAP,DROP
        def ZBRANCH
        dq skipe-$
        def SWAP,ONEPLUS,SWAP,ONEMINUS
        def BRANCH
        dq skipb-$
skipe:  NEXT

defcode "countDec",8,F_HIDDEN,cntDec
; ( a u -- n )
; skip-digits OVER C@ 46 =
; IF
;       SWAP 1+ SWAP 1-
;       0 R> BEGIN
;               OVER C@ -33 AND 69 = NOT
;       WHILE
;               R> 1+ >R
;               SWAP 1+ SWAP 1-
;       REPEAT
;       2DROP R>
; EXIT THEN
; 2DROP 0 ;
        def skipDigs
        def OVER,FETCHBYTE,LIT
        dq 46
        def EQUAL
        def ZBRANCH
        dq cdec1-$
        def SWAP,ONEPLUS,SWAP,ONEMINUS
        def LIT
        dq 0
        def TOR
cdecb:  def OVER,FETCHBYTE,LIT
        dq -33
        def ANDF,LIT
        dq 69
        def EQUAL,NOTF
        def ZBRANCH
        dq cdece-$
        def FROMR,ONEPLUS,TOR
        def SWAP,ONEPLUS,SWAP,ONEMINUS
        def BRANCH
        dq cdecb-$
cdece:  def TWODROP,FROMR
        NEXT
cdec1:  def TWODROP,LIT
        dq 0
        NEXT

align 8
twop32 dq 0x41F0000000000000 ;0x1.0p32
twop52 dq 0x4330000000000000 ;0x1.0p52
twop64 dq 0x43F0000000000000 ;0x1.0p63
twop12 dq 0x40B0000000000000 ;0x1.0p12

defcode "S>F",3,0,STOF
; ( n -- r )
        ; pop {r0}      @ hi
        ; pop {r1}      @ lo
        ; vmov s0,r0
        ; vcvt.f64.s32 d0,s0 @ d0 = hi = (a>>32)
        ; ldr r2,twop32
        ; ldr r2, [r2]
        ; mov r3, #0
        ; vmov d1, r3, r2
        ; vmul.f64 d0, d0, d1 @ d0 = high = (a>>32)*twop32
        ; ldr r2,twop52
        ; ldr r2,[r2]
        ; vmov d1, r3, r2   @ d1 = twop52
        ; eor r3, r3, r1
        ; vmov d2, r3, r2   @ d2 = low.d = twop52 | lo
        ; vsub.f64 d3, d0, d1
        ; vadd.f64 d1, d3, d2 @ d1 = (high - twop52 + low.d
        ; fpush d1
        ; NEXT
        ; movsd   xmm0, [DS]
        ; add     DS, 8
        POPSP rax
        cvtsi2sd xmm0,rax
        ; sub DS, 8
        ; movsd [DS], xmm0
        FPUSH xmm0
        NEXT

defcode "D>F",3,0,DTOF
    ;cvtsi2sd xmm0, [DS]
    ; cvtsi2sd xmm1, 8[DS]
    POPSP rax ; hi
    POPSP rbx ; lo
    mov r8, rbx
    mov r9, rax
    
    xorpd xmm0, xmm0
    xorpd xmm1, xmm1
    xorpd xmm5, xmm5
    mov rcx, 0x00000000FFFFFFFF
    and r9, rcx
    mov rdx, [twop52]
    or rdx, r9
    PUSHSP rdx
    movsd xmm3, [DSP]
    POPSP rdx
    movsd xmm2, [twop52]
    subsd xmm3, xmm2
    addsd xmm5, xmm3
    
    mov rcx, 32
    shr rax, cl
    mov rdx, [twop52]
    or rdx, rax
    PUSHSP rdx
    movsd xmm3, [DSP]
    POPSP rdx
    subsd xmm3, xmm2
    addsd xmm1, xmm3
    movsd xmm2, [twop32]
    mulsd xmm1, xmm2
    addsd xmm5, xmm1
    movsd xmm2, [twop64]
    mulsd xmm5, xmm2
    
    xorpd xmm0, xmm0
    xorpd xmm1, xmm1
    mov rcx, 0x00000000FFFFFFFF
    and r8, rcx
    mov rdx, [twop52]
    or rdx, r8
    PUSHSP rdx
    movsd xmm3, [DSP]
    POPSP rdx
    movsd xmm2, [twop52]
    subsd xmm3, xmm2
    addsd xmm0, xmm3
    
    mov rcx, 32
    shr rbx, cl
    mov rdx, [twop52]
    or rdx, rbx
    PUSHSP rdx
    movsd xmm3, [DSP]
    POPSP rdx
    subsd xmm3, xmm2
    addsd xmm1, xmm3
    movsd xmm2, [twop32]
    mulsd xmm1, xmm2
    addsd xmm0, xmm1
    
    addsd xmm0, xmm5
    FPUSH xmm0
    NEXT

defcode "(F.)",4,0,PARENFDOT
; ( r -- )
        ; fpop d0
        ; pop {r0}
        ; bl d2s_buffered
        ; NEXT
    
    FPOP    xmm0
    POPSP     r8
    sub     rsp, 32
    mov     rdx, [var_PRECISION]
    call    [_gcvt]
    add     rsp, 32
    
    ; FPOP    xmm0
    ; pop     rcx
    ; sub     rsp, 32
    ; lea     rdx, [gfmt]
    ; call    sprintf
    ; add     rsp, 32
    NEXT

defcode "F.",2,0,FDOT
        def PAD,LIT
        dq 60
        def PLUS
        def PARENFDOT
        def PAD,LIT
        dq 60
        def PLUS
        def DUPF,STRLEN,TYPEF
        def SPACE
        NEXT

defcode "F.S",3,0,FDOTS
; ." <"
; DEPTH .
; ." > "
; S0 @ 8 -
; BEGIN
; DUP SP@ 8+ >
; WHILE
;  DUP @ . ( print the stack element )
;  8 -
; REPEAT
; DROP ;

; ( -- )
; ." <"
; FDEPTH .
; ." > "
; F0 @ 8 -
; BEGIN
; DUP FSP@ 8 + >
; WHILE
;  DUP f@ fe. ( print the stack element )
;  8 -
; REPEAT
; DROP ;
        
        def LIT
        dq 60
        def EMIT
        def FDEPTH,UDOT
        def LIT
        dq 8
        def EMIT
        def LIT
        dq 62
        def EMIT,SPACE
        def F0,FETCH
FDSB:   def DUPF,FSPFETCH
        def GTF,ZBRANCH
        dq FDSE-$
        def DUPF,LIT
        dq 8
        def MINUS,FFETCH,FDOT
        def LIT
        dq 8
        def MINUS
        def BRANCH
        dq FDSB-$
FDSE:   def DROP
        NEXT

; create an array of powers of ten as if it was 
; created with : array create 1e1 f, 1e2 f, ... etc ; 
label name_POWERSOFTEN
    dq link              ; link
link = name_POWERSOFTEN
    db 11+F_HIDDEN             ; flags + length byte
    db "POWERSOFTEN"                ; the name
    align 8              ; padding to next 4 byte boundary
label POWERSOFTEN
    dq DOVAR,0
label var_POWERSOFTEN
    dq 0x4024000000000000
    dq 0x4059000000000000
    dq 0x40C3880000000000
    dq 0x4197D78400000000
    dq 0x4341C37937E08000
    dq 0x4693B8B5B5056E17
    dq 0x4D384F03E93FF9F5
    dq 0x5A827748F9301D32
    dq 0x75154FDD7F73BF3C
    dq EXIT

defcode ">FLOAT",6,0,TOFLOAT
        def OVER,FETCHBYTE,LIT
        dq 45
        def EQUAL
        def ZBRANCH
        dq TFE-$
        def ONEMINUS,SWAP,ONEPLUS,SWAP
        def LIT
        dq -1
        def TOR
        def BRANCH
        dq TFT-$
TFE:    def LIT
        dq 1
        def TOR
TFT:    def OVER,FETCHBYTE,LIT
        dq 43
        def EQUAL
        def ZBRANCH
        dq TF2-$
        def ONEMINUS,SWAP,ONEPLUS,SWAP
TF2:    def LIT
        dq 0
        def DUPF,TWOSWAP,TONUMBER
        def TWOSWAP,TWOSWAP,OVER
        def FETCHBYTE,LIT
        dq 46
        def EQUAL
        def ZBRANCH
        dq TFE3-$
        def ONEMINUS,SWAP,ONEPLUS,SWAP
        def DUPF,TOR,TONUMBER,DUPF,FROMR
        def MINUS,TOR
        def BRANCH
        dq TF3-$
TFE3:   def LIT
        dq 0
        def TOR
TF3:    def TWOSWAP,DTOF,OVER,FETCHBYTE
        def LIT
        dq 32
        def ORF,LIT
        dq 101
        def EQUAL
        def ZBRANCH
        dq TF4-$
        def ONEMINUS,SWAP,ONEPLUS,SWAP
        def OVER,FETCHBYTE,LIT
        dq 45
        def EQUAL
        def ZBRANCH
        dq TFE5-$
        def ONEMINUS,SWAP,ONEPLUS,SWAP
        def LIT
        dq -1
        def TOR
        def BRANCH
        dq TF5-$
TFE5:   def LIT
        dq 1
        def TOR
TF5:    def OVER,FETCHBYTE,LIT
        dq 43
        def EQUAL
        def ZBRANCH
        dq TF6-$
        def ONEMINUS,SWAP,ONEPLUS,SWAP
TF6:    def LIT
        dq 0
        def DUPF,TWOSWAP,TONUMBER,TWOSWAP
        def DROP,FROMR,MULF,FROMR,PLUS,TOR
TF4:    def FROMR,DUPF,ZLT
        def ZBRANCH
        dq TFE7-$
        def NEGATE,LIT
        dq -1
        def TOR
        def BRANCH
        dq TF7-$
TFE7:   def LIT
        dq 1
        def TOR
TF7:    def LIT
        dq 1
        def STOF,LIT
        dq 0
        def SWAP
TFB:    def DUPF,LIT
        dq 0
        def NEQUAL
        def ZBRANCH
        dq TFEND-$
        def DUPF,LIT
        dq 1
        def ANDF
        def ZBRANCH
        dq TF8-$
        def OVER,FLOATS,POWERSOFTEN
        def PLUS,FFETCH,FMULF
TF8:    def LIT
        dq 1
        def RSHIFT
        def SWAP,ONEPLUS,SWAP
        def BRANCH
        dq TFB-$
TFEND:  def TWODROP,FROMR,ZLT
        def ZBRANCH
        dq TFE9-$
        def FDIVF
        def BRANCH
        dq TF9-$
TFE9:   def FMULF
TF9:    def FROMR,ZLT
        def ZBRANCH
        dq TF10-$
        def FNEGATE
TF10:   def TWODROP
        NEXT

defcode "CONVERT",7,0,CONVERT
; ( a u -- n t | d t | a u f )
; 2DUP ISFLOAT? IF >FLOAT EXIT THEN
; BASE ; >R  ( save the current base )
; 2DUP >R >R OVER C; 45 = IF SWAP 1+ SWAP 1- -1 ELSE 1 THEN  ( get sign )
; -ROT ( s a u )
; 2DUP ISHEX? IF SWAP 2 + SWAP 2 - HEX THEN
; 0 DUP 2SWAP >NUMBER ( s dl dh a u )
; ?DUP 0= IF
;           DROP D>S SWAP * POSTPONE LITERAL -1
;           RDROP RDROP
;           R> BASE !
;         ELSE
;           OVER C@ 46 =    ( is it a double? )
;           IF
;             SWAP 1+ SWAP 1- ( s dl dh a u )
;             >NUMBER ( s dl dh )
;             2DROP
;             ROT 0< IF DNEGATE THEN
;             POSTPONE 2LITERAL -1 RDROP RDROP
;             R> BASE !
;           ELSE
;             2DROP 2DROP DROP ABORT R> R> 0
;             R> BASE !
;           THEN
;         THEN
; ;
        def TWODUP,ISFLOATQ,ZBRANCH
        dq CON10-$
        def TOFLOAT,FLITERAL,LIT
        dq -1
        NEXT
CON10:  def BASE,FETCH,TOR
        def TWODUP,TOR,TOR,OVER,FETCHBYTE,LIT
        dq 45
        def EQUAL
        def ZBRANCH
        dq CON1-$
        def SWAP,ONEPLUS,SWAP,ONEMINUS,LIT
        dq -1
        def BRANCH
        dq CON2-$
CON1:   def LIT
        dq 1
CON2:   def MROT,TWODUP,ISHEXQ,ZBRANCH
        dq CON7-$
        def SWAP,LIT
        dq 2
        def PLUS,SWAP,LIT
        dq 2
        def MINUS,HEX
CON7:   def LIT
        dq 0
        def DUPF,TWOSWAP,TONUMBER,QDUP
        def ZEQ,ZBRANCH
        def CON3-$
        def DROP,DTOS,SWAP,MULF,LITERAL,LIT
        dq -1
        def RDROP,RDROP
        def FROMR,BASE,FSTORE
        def BRANCH
        dq CON4-$
CON3:   def OVER,FETCHBYTE,LIT
        dq 46
        def EQUAL
        def ZBRANCH
        dq CON5-$
        def SWAP,ONEPLUS,SWAP,ONEMINUS
        def TONUMBER,TWODROP,ROT,ZLT,RDROP,RDROP
        def FROMR,BASE,FSTORE
        def ZBRANCH
        dq CON6-$
        def DNEGATE
CON6:   def TWOLITERAL,LIT
        dq -1
        def BRANCH
        dq CON4-$
CON5:   def TWODROP,TWODROP,DROP
        def ABORT,FROMR,FROMR,LIT
        dq 0
        def FROMR,BASE,FSTORE
CON4:   NEXT

defcode "@EXECUTE",8,0,ATEXECUTE
; ( a -- )
; ; ?DUP IF EXECUTE THEN ;
        def FETCH,QDUP,ZBRANCH
        dq ATEX1-$
        def EXECUTE
ATEX1:  NEXT

defcode "KEY?",4,0,QKEY
; ( -- c T | F )
; '?KEY ;EXECUTE ;
        def TICKQKEY,ATEXECUTE
        NEXT

defcode "KEY",3,0,KEY
; ( -- c)
; BEGIN ?KEY UNTIL ;
KBEG:   def QKEY
        def ZBRANCH
        dq KBEG-$
        NEXT

defcode "EMIT",4,0,EMIT
; ( c -- )
; 'EMIT ; ;EXECUTE
        def TICKEMIT,ATEXECUTE
        NEXT

defcode "PACE",4,0,PACE
; ( -- )
; 11 EMIT
        def LIT
        dq 11
        def EMIT
        NEXT

defcode "SPACE",5,0,SPACE
; ( -- ) 
; BL EMIT
        def BLF,EMIT
        NEXT

defcode "SPACES",6,0,SPACES
; ( n -- )
; BEGIN DUP WHILE SPACE 1- REPEAT DROP ;
SPBEG:  def DUPF
        def ZBRANCH
        dq SPEND-$
        def SPACE,LIT
        dq 1
        def MINUS
        def BRANCH
        dq SPBEG-$
SPEND:  def DROP
        NEXT

defcode "TYPE",4,0,TYPEF
; ( b u -- )
; ?DUP IF
;   BEGIN
;       DUP
;   WHILE
;    OVER C; EMIT
;    SWAP 1 + SWAP 1 -
; REPEAT 
; TWODROP EXIT
; ELSE DROP THEN ;
        def QDUP,ZBRANCH
        dq TYP1-$
TYPBEG: def DUPF
        def ZBRANCH
        dq TYPEND-$
        def OVER,FETCHBYTE,EMIT
        def SWAP,LIT
        dq 1
        def PLUS
        def SWAP,LIT
        dq 1
        def MINUS
        def BRANCH
        dq TYPBEG-$
TYPEND: def TWODROP
        NEXT
TYP1:   def DROP
        NEXT

defcode "SLITERAL",8,F_IMMED,SLITERAL 
; ( c u -- )
; STATE @ IF
;   [COMPILE] DO$ DUP DUP >R ,  
;   HERE SWAP CMOVE
;   HERE R> + ALIGNED DP !
; THEN ; IMMEDIATE
        def STATE,FETCH,ZBRANCH
        dq SLI1-$
        def LIT
        dq DOSTRING
        def COMMA
        def DUPF,DUPF,TOR,COMMA
        def HERE,SWAP,CMOVEF
        def HERE,FROMR,PLUS,ALIGNED,FDP,FSTORE
SLI1:   NEXT

defcode 'S"',2,F_IMMED,SQUOTE
; ( -- )
; STATE ; 
;   IF
;       34 PARSE SLITERAL
;   ELSE
;       34 PARSE PAD SWAP DUP >R CMOVE PAD R>
;   THEN ;
        def STATE,FETCH,ZBRANCH
        dq SQ1-$
        def LIT
        dq 34
        def PARSE
        def SLITERAL
        def BRANCH
        dq SQ2-$
SQ1:    def LIT
        dq 34
        def PARSE,PAD,LIT
        dq 20
        def PLUS,SWAP,DUPF
        def TOR,CMOVEF
        def PAD,LIT
        dq 20
        def PLUS,FROMR
SQ2:    NEXT

defcode 'C"',2,F_IMMED,CQUOTE
; ( -- )
; STATE ; 
;   IF
;       34 PARSE 
;       [COMPILE] DOP$ DUP DUP >R C,  
;       HERE SWAP CMOVE
;       HERE R> + ALIGNED DP !
;   ELSE
;       34 PARSE PAD 2DUP C! 1+ SWAP CMOVE PAD
;   THEN ;
        def STATE,FETCH,ZBRANCH
        dq CQ1-$
        def LIT
        dq 34
        def PARSE
        def LIT
        dq DOPSTRING
        def COMMA
        def DUPF,DUPF,TOR,COMMABYTE
        def HERE,SWAP,CMOVEF
        def HERE,FROMR,PLUS,ALIGNED,FDP,FSTORE
        def BRANCH
        dq CQ2-$
CQ1:    def LIT
        dq 34
        def PARSE,PAD,TWODUP
        def STOREBYTE,ONEPLUS,SWAP,CMOVEF,PAD
CQ2:    NEXT


defcode '."',2,F_IMMED,DOTQUOTE
; ( -- )
; POSTPONE S" POSTPONE TYPE ; IMMEDIATE
; [COMPILE] S" ['] TYPE , ; IMMEDIATE
        def STATE,FETCH,ZBRANCH
        dq DQ1-$
        def SQUOTE
        def LIT
        dq TYPEF
        def COMMA
        def BRANCH
        dq DQ2-$
DQ1:    def LIT
        dq 34
        def PARSE,TYPEF
DQ2:    NEXT

defcode "^H",2,0,BKSP
; ( b b b -- b b b )
; >R OVER R> SWAP OVER XOR
; IF 8 'ECHO ;EXECUTE
;    32 'ECHO ;EXECUTE
;    8 'ECHO ;EXECUTE
; THEN ;
        def TOR,OVER,FROMR,SWAP,OVER
        def XORF,ZBRANCH
        dq BKSP1-$
        def LIT
        dq 8
        def TICKECHO,ATEXECUTE
        def LIT
        dq 1
        def MINUS
        def LIT
        dq 32
        def TICKECHO,ATEXECUTE
        def LIT
        dq 8
        def TICKECHO,ATEXECUTE
BKSP1:  NEXT

defcode "TAP",3,0,TAP
; ( bot eot cur c -- bot eot cur )
; DUP 'ECHO @EXECUTE OVER C! 1 + ;
        def DUPF,TICKECHO,ATEXECUTE,OVER
        def STOREBYTE,LIT
        dq 1
        def PLUS
        NEXT

defcode "KTAP",4,0,KTAP
; ( bot eot cur c -- bot eot cur )
; DUP 13 XOR
; IF 8 XOR IF BL TAP ELSE ^H THEN EXIT
; THEN DROP SWAP DROP DUP ;
        def DUPF,LIT
        dq 9
        def EQUAL
        def ZBRANCH
        dq kT3-$ ; if it's a tab then replace with space
        def DROP,LIT
        dq 2
        def SPACES,BLF,TAP
        NEXT
kT3:    def DUPF,LIT
        dq 13
        def XORF
        def ZBRANCH
        dq kT1-$
        def DUPF,LIT
        dq 8
        def XORF,SWAP
        def LIT
        dq 127
        def XORF,ANDF,ZBRANCH
        dq kT2-$
        def BLF,TAP
        NEXT
kT2:    def BKSP
        NEXT
kT1:    def DROP,SWAP,DROP,DUPF
        NEXT

defcode "ACCEPT",6,0,ACCEPT
; ( b u -- u )
; OVER + OVER
; BEGIN 2DUP XOR
; WHILE KEY DUP BL - 95 U<
;   IF TAP ELSE 'TAP @EXECUTE THEN
; REPEAT DROP OVER - ;
        def OVER,PLUS,OVER
ACBEG:  def TWODUP,XORF,ZBRANCH
        dq ACEND-$
        def KEY,DUPF
        def BLF,LIT
        dq 127
        def WITHIN
        def ZBRANCH
        dq AC1-$
        def TAP,BRANCH
        dq AC2-$
AC1:    def TICKTAP,ATEXECUTE
AC2:    def BRANCH
        dq ACBEG-$
ACEND:  def DROP,OVER,MINUS
        def SWAP,DROP
        NEXT

defcode "EXPECT",6,0,EXPECT
; ( b u -- )
; 'EXPECT ;EXECUTE SPAN ! DROP ;
        def TICKEXPECT,ATEXECUTE,SPAN
        def FSTORE
        NEXT

defcode "QUERY",5,0,QUERY
; ( a u -- )
; TIB @ 130 'EXPECT @EXECUTE #TIB ! 0 >IN ! ;
      def TIB,FETCH,LIT
      dq 130
      def TICKEXPECT,ATEXECUTE
      def NTIB,FSTORE
      def LIT
      dq 0
      def TOIN,FSTORE
      NEXT

defcode "REFILL",6,0,REFILL
; ( -- f )
; QUERY -1
        ;dq TIB,FETCH,LIT,80
    def SOURCEID,FETCH,ZEQ,ZBRANCH
    dq QU1-$
    def QUERY,LIT
    dq -1
QU1:
    def SOURCEID,FETCH,ZGT,ZBRANCH
    dq QU2-$
    def TIB,FETCH,LIT
    dq 130
    def SOURCEID,FETCH
    def READLINE
    def THROW,SWAP,NTIB,FSTORE
    def SOURCE,INTERPRET
QU2:
    NEXT

defcode "SOURCE",6,0,SOURCE
; ( -- a u )
; TIB ; #TIB ; ;
        def TIB,FETCH,NTIB,FETCH
        NEXT

defcode "CR",2,0,CR
; ( -- )
; 13 EMIT 10 EMIT ;
        def LIT
        dq 13
        def EMIT,LIT
        dq 10
        def EMIT
        NEXT

defcode "same?",5,F_HIDDEN,SAMEQ
; ( a a u -- f )
; swap >r
; begin dup
; while char- 2dup + c; over r; + c; xor
; until r> drop 2drop 0 exit ( no match )
; then r> drop 2drop -1 ; ( found )
        def SWAP,TOR
SABEG:  def DUPF,ZBRANCH
        dq SATHE-$
        def LIT
        dq 1
        def MINUS,TWODUP
        def PLUS,FETCHBYTE,OVER
        def RFETCH,PLUS,FETCHBYTE
        def XORF,ZBRANCH
        dq SABEG-$
SAUNT:  def FROMR,DROP,TWODROP
        def LIT
        dq 0
        NEXT
SATHE:  def FROMR,DROP,TWODROP
        def LIT
        dq -1
        NEXT

defcode "(FIND)",6,0,PARENSFIND
; ( voc a -- xt t | a f )
        def COUNT,TOUPPER,SWAP,TOR,TOR
        def FETCH,FETCH
FIBEG:  def QDUP,ZBRANCH
        dq FIEND-$                  ; keep searching until latest=0
        def DUPF,LIT
        dq 8
        def PLUS,FETCHBYTE
        def LIT
        dq F_HIDDEN
        def LIT
        dq F_LENMASK
        def ORF,ANDF   ; ( L n | a u )
        def DUPF,RFETCH,EQUAL,ZBRANCH
        dq FI1-$
        ; ( L n | a u )
        def OVER,LIT
        dq 9
        def PLUS,SWAP                ; ( L L+5 n | a u )
        ; ( now compare characters )
        def FROMR,FROMR,TWODUP,TOR,TOR,SWAP,DROP  ; ( L L+5 n a | a u )
        def SWAP                                  ; ( L L+5 a n | a u )
        def SAMEQ                            ; ( L t/f | a u )
        def ZBRANCH
        dq FI3-$
        def RDROP,RDROP                      ; ( L )
        def DUPF                             ; ( L L )
        def TCFA                             ; ( L cfa )
        def SWAP,IMMEDQ,ZBRANCH
        dq FI5-$
        def LIT
        dq 1
        def BRANCH
        dq FI4-$
FI5:    def LIT
        dq -1
FI4:    NEXT
FI3:    def BRANCH
        dq FI2-$  ; ( exit if )
FI1:    def DROP                ; ( L n | a u : else branch - not equal length ) 
FI2:    
        def FETCH,BRANCH
        dq FIBEG-$
FIEND:  def RDROP,FROMR
        def LIT
        dq 1
        def MINUS       ; get back to c-addr
        def LIT
        dq 0
        NEXT

defcode "FIND",4,0,FIND
        def TOR,CONTEXT
FFBEG:  def DUPF,FETCH,ZBRANCH
        dq FFEND-$
        def DUPF
        def RFETCH,PARENSFIND
        def DUPF,ZBRANCH
        dq FFF1-$
        def ROT,DROP,RDROP
        NEXT
FFF1:   def TWODROP
        def CELLPLUS,BRANCH
        dq FFBEG-$
FFEND:  def DROP,FROMR,LIT
        dq 0
        NEXT

defcode ">CFA",4,0,TCFA
; ( a -- xt )
; DUP CELL + C@ F_LENMASK AND + CELL 1 + + ALIGNED ;
        def DUPF,CELL,PLUS,FETCHBYTE
        def LIT
        dq F_LENMASK
        def ANDF
        def CELL,LIT
        dq 1
        def PLUS,PLUS
        def PLUS
        def ALIGNED
        NEXT

defcode ">DFA",4,0,TDFA
; ( a -- xt )
; >CFA CELL + ;
        def TCFA,CELL,PLUS
        NEXT

defcode "CFA>",4,0,CFATO
; ( xt -- link )
; LATEST @ BEGIN
; ?DUP WHILE 2DUP SWAP < IF NIP EXIT THEN
; @ REPEAT DROP 0 ;
       ; dq LATEST,FETCH
        def CURRENT,FETCH,FETCH
CFTB:   def QDUP,ZBRANCH
        dq CFT1-$
        def TWODUP,SWAP,LTF,ZBRANCH
        dq CFT2-$
        def NIP
        NEXT
CFT2:   def FETCH,BRANCH
        dq CFTB-$
CFT1:   def DROP,LIT
        dq 0
        NEXT

defcode ">BODY",5,0,TOBODY
; ( xt -- dfa )
; undefined if used on a word not created wth CREATE
; CELL+ ;
        def LIT
        dq 2
        def CELLS,PLUS
        NEXT

defcode "FORGET",6,,FORGET
; ( -- )
; BL WORD FIND IF
; CFA> DUP @ LATEST !
; DP ! 
; THEN ;
; FIX to go back to link field from cfa, i.e. xt
        def BLF,WORDF,FIND
        def ZBRANCH
        dq FORG1-$
        def CFATO,DUPF,FETCH;,DUPF
        def CURRENT,FETCH,FSTORE
        def FDP,FSTORE
FORG1:  NEXT

defcode "HEADER,",7,0,HEADERCOMMA
; ( a u -- )
; TOUPPER LATEST @              ( get last word address )
; HERE LATEST !        ( store this word in the LATEST variable )
; ,                    ( store last word in link field )
; DUP DUP >R C,        ( a u  - store word length, save a copy on RS )
; HERE SWAP CMOVE      ( copy name to dictionary )
; HERE R> + ALIGNED DP !    ( add name length and align for padding )
        def TOUPPER,CURRENT,FETCH,FETCH
        def HERE,CURRENT,FETCH,FSTORE
        def COMMA,DUPF,DUPF,TOR,COMMABYTE
        def HERE,SWAP,CMOVEF
       ; align
        def HERE,FROMR,PLUS,ALIGNED,FDP,FSTORE 
        NEXT

defcode "TOUPPER",7,0,TOUPPER
; ( a u -- a u )
; 2DUP BEGIN 
;       DUP ( a u u )
;       WHILE ( a u ) 
;           OVER C; DUP 122 <= SWAP 97 >= AND
;           IF 32 OR THEN ( a u c )
;           2 PICK C!
;           1 - SWAP 1 + SWAP ( a+1 u-1 )
;       REPEAT
; 2DROP ;
        def TWODUP
TUPB:   def DUPF,ZBRANCH
        dq TUPE-$
        def OVER,FETCHBYTE,DUPF,DUPF,LIT
        dq 123
        def LTF,SWAP,LIT
        dq 97
        def GTE,ANDF
        def ZBRANCH
        dq TUP1-$
        def LIT
        dq 32
        def XORF
TUP1:   def LIT
        dq 2
        def PICK,STOREBYTE
        def LIT
        dq 1
        def MINUS,SWAP,LIT
        dq 1
        def PLUS,SWAP
        def BRANCH
        dq TUPB-$
TUPE:   def TWODROP
        NEXT

defcode "CREATE",6,0,CREATE
; ( -- )
; PARSE-NAME HEADER, COMPILE DOVAR 0 , ;
; Note that no EXIT is compiled
        def PARSENAME,HEADERCOMMA
        def LIT
        ; TODO fix this for adding a call opcode
        dq DOVAR
        def COMMA
        def LIT
        dq 0
        def COMMA
        NEXT

defcode "DOES>",5,0,DOES
; ( -- )
; ['] DODOES LITERAL LATEST @ >CFA !
; R> LATEST @ >CFA >BODY ! ;
        ; dq LIT,DODOES,LATEST,FETCH,TCFA
        ; TODO fix this to add a call opcode
        def LIT
        dq DODOES
        def CURRENT,FETCH,FETCH,TCFA
        def FSTORE
        ; dq FROMR,LATEST,FETCH,TCFA,TOBODY
        def FROMR,CURRENT,FETCH,FETCH,TDFA
        def FSTORE
        NEXT

defcode "VALUE",5,0,VALUE
; ( n -- )
; CREATE , DOES> @ ;   -- my old way!
; PARSE-NAME HEADER, COMPILE DOVAL , ;
        def PARSENAME,HEADERCOMMA
        def LIT
        ; TODO fix this to add a call opcode
        dq DOVAL
        def COMMA
        ;dq LIT,0,COMMA
        dq COMMA
        NEXT

defcode "2VALUE",6,0,TWOVALUE
; ( n n -- )
; CREATE swap , , DOES> dup ; swap cell+ ; ; -- old way
        def PARSENAME,HEADERCOMMA
        def LIT
        ; TODO fix this to add a call opcode
        dq DO2VAL
        def COMMA
        def COMMA,COMMA
        NEXT

decode "TO",2,F_IMMED,TOO
; ( x -- )
; ' >DFA STATE @
; IF
;   LITERAL [COMPILE] !
; ELSE
;   !
; THEN ;
;TODO fix this to add a call opcode
        dq TICK,DUPF,FETCH,LIT,DOCOL2,EQUAL,ZBRANCH,TO3
        dq CELLPLUS,FETCH
        dq LIT,ATLOC0,OVER,EQUAL,ZBRANCH,TO4
        dq DROP,COMPILE,TOLOC0,BRANCH,TOX
TO4:    dq LIT,ATLOC1,OVER,EQUAL,ZBRANCH,TO5
        dq DROP,COMPILE,TOLOC1,BRANCH,TOX
TO5:    dq LIT,ATLOC2,OVER,EQUAL,ZBRANCH,TO6
        dq DROP,COMPILE,TOLOC2,BRANCH,TOX
TO6:    dq LIT,ATLOC3,OVER,EQUAL,ZBRANCH,TO7
        dq DROP,COMPILE,TOLOC3,BRANCH,TOX
TO7:    dq LIT,ATLOC4,OVER,EQUAL,ZBRANCH,TO8
        dq DROP,COMPILE,TOLOC4,BRANCH,TOX
TO8:    dq LIT,ATLOC5,OVER,EQUAL,ZBRANCH,TO9
        dq DROP,COMPILE,TOLOC5,BRANCH,TOX
TO9:    dq LIT,ATLOC6,OVER,EQUAL,ZBRANCH,TO10
        dq DROP,COMPILE,TOLOC6,BRANCH,TOX
TO10:   dq LIT,ATLOC7,OVER,EQUAL,ZBRANCH,TOX
        dq DROP,COMPILE,TOLOC7,BRANCH,TOX
TO3:    dq TDFA,STATE,FETCH,ZBRANCH,TO1
        dq LITERAL,COMPILE,FSTORE
        dq BRANCH,TOX
TO1:    dq FSTORE
TOX:    NEXT

defcode "+TO",3,F_IMMED,PLUSTO
; ( x -- )
; ' >DFA STATE @
; IF
;   LITERAL [COMPILE] +!
; ELSE
;   +!
; THEN ;
;TODO fix this to add a call opcode
        dq TICK,DUPF,FETCH,LIT,DOCOL2,EQUAL,ZBRANCH,PTO3
        dq CELLPLUS,FETCH        
        dq LIT,ATLOC0,OVER,EQUAL,ZBRANCH,PTO4
        dq DROP,COMPILE,ATLOC0,COMPILE,PLUS
        dq COMPILE,TOLOC0,BRANCH,PTOX
PTO4:   dq LIT,ATLOC1,OVER,EQUAL,ZBRANCH,PTO5
        dq DROP,COMPILE,ATLOC1,COMPILE,PLUS
        dq COMPILE,TOLOC1,BRANCH,PTOX
PTO5:   dq LIT,ATLOC2,OVER,EQUAL,ZBRANCH,PTO6
        dq DROP,COMPILE,ATLOC2,COMPILE,PLUS
        dq COMPILE,TOLOC2,BRANCH,PTOX
PTO6:   dq LIT,ATLOC3,OVER,EQUAL,ZBRANCH,PTO7
        dq DROP,COMPILE,ATLOC3,COMPILE,PLUS
        dq COMPILE,TOLOC3,BRANCH,PTOX
PTO7:   dq LIT,ATLOC4,OVER,EQUAL,ZBRANCH,PTO8
        dq DROP,COMPILE,ATLOC4,COMPILE,PLUS
        dq COMPILE,TOLOC4,BRANCH,PTOX
PTO8:   dq LIT,ATLOC5,OVER,EQUAL,ZBRANCH,PTO9
        dq DROP,COMPILE,ATLOC5,COMPILE,PLUS
        dq COMPILE,TOLOC5,BRANCH,PTOX
PTO9:   dq LIT,ATLOC6,OVER,EQUAL,ZBRANCH,PTO10
        dq DROP,COMPILE,ATLOC6,COMPILE,PLUS
        dq COMPILE,TOLOC6,BRANCH,PTOX
PTO10:  dq LIT,ATLOC7,OVER,EQUAL,ZBRANCH,PTOX
        dq DROP,COMPILE,ATLOC7,COMPILE,PLUS
        dq COMPILE,TOLOC7,BRANCH,PTOX
PTO3:   dq TDFA,STATE,FETCH,ZBRANCH,PTO1
        dq LITERAL,COMPILE,PLUSSTORE
        dq BRANCH,PTOX
PTO1:   dq PLUSSTORE
PTOX:   NEXT

defcode "VARIABLE",8,0,VARIABLE
; ( -- )
; CREATE 0 , ;
        def CREATE,LIT
        dq 0
        def COMMA
        NEXT

defcode "FVARIABLE",9,0,FVARIABLE
; ( -- )
; CREATE 0 , ;   -- floats are the same size as ints
        def VARIABLE
        NEXT
    
defcode "CONSTANT",8,0,CONSTANT
; ( n -- )
; CREATE , DOES> @ ; -- the old way
; VALUE
        def PARSENAME,HEADERCOMMA
        def LIT
        ; TODO add a call opcode
        dq DOCONST
        def COMMA,COMMA
        NEXT

defcode "FCONSTANT",9,0,FCONSTANT
; ( n -- )
; CREATE , DOES> @ ;
        def CONSTANT
        NEXT
    
defcode "2VARIABLE",9,0,TWOVARIABLE
; ( -- )
; CREATE 0 ! 0 ! ;
        def CREATE,LIT
        dq 0
        def COMMA,LIT
        dq 0
        def COMMA
        NEXT

defcode "2CONSTANT",9,0,TWOCONSTANT
; ( d -- )
; CREATE SWAP , , DOES> DUP @ SWAP CELL+ @ ;
        def CREATE,SWAP,COMMA,COMMA,DOES,DUPF,FETCH
        def SWAP,CELLPLUS,FETCH
        NEXT

defcode ",",1,0,COMMA
; ( n -- )
; ; HERE DUP CELL+ DP ! ! ;
; HERE ! 1 CELLS ALLOT ;
        def HERE,DUPF,CELLPLUS
        def FDP,FSTORE,FSTORE
        NEXT

defcode "[",1,F_IMMED,LBRAC
; ( -- )
;  0 STATE ! ;
        def LIT
        dq 0
        def STATE,FSTORE
        NEXT

defcode "]",1,0,RBRAC
; ( -- ) 
; 1 STATE ! ;
        def LIT
        dq 1
        def STATE,FSTORE
        NEXT

defcode "HIDDEN",6,0,HIDDEN
; ( a -- )
;  8 + DUP @ F_HIDDEN XOR SWAP !
        def LIT
        dq 8
        def PLUS
        def DUPF,FETCH
        def LIT
        dq F_HIDDEN
        def XORF,SWAP,FSTORE
        NEXT

defcode "UNHIDE",6,0,UNHIDE
; ( a -- )
;  8 + DUP @ F_HIDDEN XOR SWAP !
        def LIT
        dq 8
        def PLUS
        def DUPF,FETCH
        def LIT
        dq F_HIDDEN
        def INVERT
        def ANDF,SWAP,FSTORE
        NEXT

defcode ":",1,0,COLON
; ( -- )
; BL WORD COUNT HEADER,
; DOCOL , LATEST @ HIDDEN ] ;
        def BLF,WORDF,COUNT
        def HEADERCOMMA,LIT
        ; TODO add a call opcode
        dq DOCOL
        def COMMA,CURRENT,FETCH,FETCH
        def HIDDEN,RBRAC
        NEXT

defcode ";",1,F_IMMED,SEMICOLON
; ( -- )
; EXIT , LATEST @ HIDDEN ] ;
        def STATE,FETCH,ZBRANCH
        dq COLX-$
        def LOCFLG,FETCH,ZBRANCH
        dq COL1-$
        def LIT
        dq 0
        def HERE
COLB:   def CELLMINUS,DUPF,FETCH,LIT
        ; TODO treat the call opcode
        dq DOCOL
        def NEQUAL,ZBRANCH
        dq COLE-$
        def SWAP,ONEPLUS,SWAP
        def DUPF,FETCH,LIT
        dq PARENSDO
        def EQUAL,ZBRANCH
        dq COL2-$
        def CELL,OVER,LIT
        dq 2
        def CELLS,PLUS,PLUSSTORE
COL2:   def DUPF,FETCH,LIT
        dq PARENSLOOP
        def EQUAL,ZBRANCH
        dq COL3-$
        def CELL,OVER,LIT
        dq 2
        def CELLS,PLUS,PLUSSTORE
COL3:   def DUPF,FETCH,LIT
        dq PARENSQDO
        def EQUAL,ZBRANCH
        dq COL4-$
        def CELL,OVER,LIT
        dq 2
        def CELLS,PLUS,PLUSSTORE
COL4:   def DUPF,FETCH,LIT
        dq PARENSPLUSLOOP
        def EQUAL,ZBRANCH
        dq COL5-$
        def CELL,OVER,LIT
        dq 2
        def CELLS,PLUS,PLUSSTORE
COL5:   def DUPF,FETCH,LIT
        dq BRANCH
        dq EQUAL,ZBRANCH
        dq COL6-$
        def CELL,OVER,LIT
        dq 2
        def CELLS,PLUS,PLUSSTORE
COL6:   def DUPF,FETCH,LIT
        dq ZBRANCH
        def EQUAL,ZBRANCH
        dq COL7-$
        def CELL,OVER,LIT
        dq 2
        def CELLS,PLUS,PLUSSTORE
COL7:   def DUPF,FETCH,OVER,CELLPLUS,FSTORE
        def BRANCH
        dq COLB-$
COLE:   def CELLPLUS,LIT
        dq LOCINIT
        def SWAP,FSTORE
        def ZBRANCH
        dq COL1-$
        def CELL,FDP,PLUSSTORE
COL1:   def RESETLOCALS
        def LIT
        ; TODO treat the call opcode 
        dq 0 ; wa an EXIT word
        def COMMA
        def CURRENT,FETCH,FETCH,UNHIDE
        def LBRAC
COLX:   NEXT

defvoc "LOC-VOC",7,0,LOCVOC,0
defvoc "ROOT",4,0,ROOT,name_FORTH
defvoc "FORTH",5,0,FORTH,name_FDUMP,rootlink

defcode "DUMP",4,0,FDUMP
    NEXT

section '.idata' import data readable writeable

  library kernel32,'kernel32.dll',\
      kernelbase32,'kernelbase32.dll',\
      user32,'user32.dll',\
      shell32,'shell32.dll',\
      msvcrt,'msvcrt.dll',\
      ucrtbase,'ucrtbase.dll'

  include 'api\kernel32.inc'
  include 'api\user32.inc'
  import msvcrt,\
     printf,'printf'
