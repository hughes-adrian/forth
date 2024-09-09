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

macro defword name, namelen, flags=0, lblw, nvoc=link {
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
    ;mov     [var_FDP], rax
    mov     [HeapBase], rax

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
defvar "S0",2,0,S0,0
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
defconst "R0",2,0,RZ,return_stack_top
defconst "WBUF",4,0,WBUF,word_buffer
defconst "DOCOL",5,0,__DOCOL,DOCOL
defconst "DOCOL2",6,0,__DOCOL2,DOCOL2
defconst "DOVAR",5,0,__DOVAR,DOVAR
defconst "DOVAL",5,0,__DOVAL,DOVAL
defconst "DODOES",6,0,__DODOES,DODOES
defconst "F_IMMED",7,0,__F_IMMED,F_IMMED
defconst "F_HIDDEN",8,0,__F_HIDDEN,F_HIDDEN
defconst "F_LENMASK",9,0,__F_LENMASK,F_LENMASK   

defword "?RX",3,0,QRX
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

defword "TX",2,0,TX
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

defword "DOCONST",7,0,DOCONST
    mov      rbx, [rsp]
    add      qword [rsp],8
    mov      rax, [rbx]
    PUSHSP   rax
    NEXT

defword "DOVAR",5,0,DOVAR
    mov      rax, [rsp]
    add      qword [rsp],16
    add      rax, 8
    PUSHSP   rax
    NEXT

defword "DODOES",6,0,DODOES
    mov      rax, [rsp]
    add      rax, 8
    PUSHSP   rax
    add      rsp, 8
    call     rax
    NEXT

defword "VODOES",6,0,VODOES
    NEXT

defword "LIT",3,0,LIT
    mov rax, [rsp]
    mov rax, [rax]
    add qword [rsp],8
    PUSHSP   rax
    NEXT

defword "EXECUTE",7,0,EXECUTE
    POPSP     rax
    jmp qword [rax]

defword "SAYOK",5,0,SAYOK
    invoke printf, msg
    NEXT

defword "BYE",3,0,BYE
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

defword "QUIT",4,0,QUIT
; ( -- )
; R0 RP! 0 STATE !
; S0 @ SP!
; F0 @ FP!
; BEGIN
;   QUERY SPACE
;   SOURCE INTERPRET
;   STATE @ 0= IF ." OK" THEN CR
; AGAIN ;
QUIT0:  def CR,RZ,RSPSTORE,LIT,0,STATE,FSTORE
        def S0,FETCH,DSPSTORE
        def F0,FETCH,FSPSTORE
QUIT1:  def QUERY,SPACE
        def SOURCE,INTERPRET
        def STATE,FETCH,LIT,0,EQUAL
        def ZBRANCH
        dq QUIT2-$
        def SAYOK
        def DEPTH,ZBRANCH
        dq QUIT3-$
        def LIT,58,EMIT,DEPTH,DOT
        def BRANCH
        dq QUIT4-$
QUIT3:  def SPACE
QUIT4:  def FDEPTH,ZBRANCH
        dq QUIT2-$
        def LIT,102,EMIT,LIT,58,EMIT
        def FDEPTH,DOT
QUIT2:  def CR,BRANCH
        dq QUIT1-$
        NEXT

defword "BRANCH",6,0,BRANCH
        mov    rax, [rsp]
        mov    rax, [rax]
        add    [rsp], rax
        NEXT

defword "0BRANCH",7,0,ZBRANCH
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

defword "!",1,0,FSTORE
    POPSP    rax
    POPSP    rbx
    mov      [rax], rbx
    NEXT

defword "@",1,0,FETCH
    POPSP    rbx
    mov      rax, [rbx]
    PUSHSP   rax
    NEXT

defword "C!",2,0,STOREBYTE
    POPSP    rax
    POPSP    rbx
    mov      byte [rax], bl 
    NEXT

defword "C@",2,0,FETCHBYTE
    POPSP   rax
    xor     rbx, rbx
    mov     byte  bl, [rax]
    PUSHSP  rbx
    NEXT

defword "C,",2,0,COMMABYTE
; ( c -- )
; HERE C! 1 CHARS ALLOT ;
;    def HERE,STOREBYTE,LIT
    dq  1
;    def CHARS,ALLOT
    NEXT

defword "RP@",3,0,RSPFETCH
    PUSHSP   RS
    NEXT

defword "RP!",3,0,RSPSTORE
    POPSP    RS
    NEXT

defword ">R",2,0,TOR
    POPSP    rax
    push     rax
    NEXT

defword "R@",2,0,RFETCH
    mov      rax, [RS]
    PUSHSP   rax
    NEXT

defword "R>",2,0,FROMR
    pop      rax
    PUSHSP   rax
    NEXT

defword "RDROP",5,0,RDROP
    add     RS, 8
    NEXT

defword "2RDROP",6,0,TWORDROP
    add     RS, 16
    NEXT
    
defword "DROP",4,0,DROP
    add     DSP, 8
    NEXT

defword "DUP",3,0,DUPF
    mov rax, [DSP]
    sub DSP, 8
    mov [DSP], rax
    NEXT

defword "SWAP",4,0,SWAP
    POPSP     rax
    POPSP     rbx
    PUSHSP    rax
    PUSHSP    rbx
    NEXT

defword "OVER",4,0,OVER
    mov      rax, [DSP+8]
    PUSHSP   rax
    NEXT

defword "SP@",3,0,DSPFETCH
    PUSHSP   DSP
    NEXT

defword "SP!",3,0,DSPSTORE
    POPSP    DS
    NEXT

defword "FSP@",4,0,FSPFETCH
    PUSHSP   FSP
    NEXT

defword "FSP!",4,0,FSPSTORE
    POPSP    FSP
    NEXT

defword "0<",2,0,ZLT
    POPSP   rax
    mov     rcx, -1
    mov     rbx, 0
    cmp     rax, rbx
    cmovl   rbx, rcx
    PUSHSP  rbx
    NEXT

defword "AND",3,0,ANDF
    POPSP   rax
    POPSP   rbx
    and     rax, rbx
    PUSHSP  rax
    NEXT

defword "OR",2,0,ORF
    POPSP   rax
    POPSP   rbx
    or      rax,rbx
    PUSHSP  rax
    NEXT

defword "XOR",3,0,XORF
    POPSP   rax
    POPSP   rbx
    xor     rax,rbx
    PUSHSP  rax
    NEXT

defword "INVERT",6,0,INVERT
    POPSP   rax
    not     rax
    PUSHSP  rax
    NEXT

defword "UM+",3,0,UMPLUS
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

defword "OV+",3,0,OVPLUS
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

defword "LSHIFT",6,0,LSHIFT
    POPSP   rcx
    POPSP   rax
    shl     rax, cl
    PUSHSP  rax
    NEXT

defword "RSHIFT",6,0,RSHIFT
    POPSP   rcx
    POPSP   rax
    shr     rax, cl
    PUSHSP  rax
    NEXT

defword "DO$",3,0,DOSTRING
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

defword "LITSTRING",9,0,LITSTRING
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

defword "DOP$",4,0,DOPSTRING
; R@ DUP C@ OVER ( a u a )
; + 1+ ALIGNED ( a b+u+1+pad )
; >R ( put next instruction onto return stack )
        def RFETCH,DUPF,FETCHBYTE,OVER
        def PLUS,LIT
        dq  1
        def PLUS,ALIGNED
        def TOR
        NEXT

defword "?DUP",4,0,QDUP
; DUP IF DUP THEN ;
        mov rax, [DS]
        cmp rax, 0
        je @f
        PUSHSP rax
@@:     NEXT

defword "ROT",3,0,ROT       
; >R SWAP R> SWAP ;
        POPSP rax
        POPSP rbx
        POPSP rcx
        PUSHSP rbx
        PUSHSP rax
        PUSHSP rcx
        NEXT

defword "-ROT",4,0,MROT
; ROT ROT ; 
        ;dq ROT,ROT
        ;dq EXIT
        pop rax
        pop rbx
        pop rcx
        push rax
        push rcx
        push rbx
        NEXT

defvoc "LOC-VOC",7,0,LOCVOC,0
defvoc "ROOT",4,0,ROOT,name_FORTH
defvoc "FORTH",5,0,FORTH,name_FDUMP,rootlink

defword "DUMP",4,0,FDUMP
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
