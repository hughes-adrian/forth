format PE64 console
use64
stack 4096*4096,4096*4096
org 401000h
entry start

include 'win64ax.inc'
include 'exception.inc'
IMAGE_DIRECTORY_ENTRY_EXCEPTION = 3 ; Exception Directory

;; TODO
;; relative jumps, not absolute
;; buffered io, especially reading and readline
;; move user variables to user vector and read from offsets
;;   -> this is to aid threading
;; investigate threading
;; investigate mutlitasking (green threads) like F83
;; finish the word SEE
;; fix DUMP
;; finish assembler vocabulary

; macros for NEXT inner interpreter and stack push/pop
macro NEXT {
    lodsq             ; mov qw into rax from rsi and incr rsi
    jmp qword [rax]   ; jump to location in rax
    ;mov rbx, qword [rax]
    ;jmp rbx
}

macro PUSHRSP reg1 {
    lea RS, [RS-8]
    mov [RS], reg1
}

macro POPRSP reg1 {
    mov reg1, [RS]
    lea RS, [RS+8]
}

macro FPUSH reg1 {
    lea FSP, [FSP-8]
    movsd [FSP], reg1
}

macro FPOP reg1 {
    movsd reg1, [FSP]
    lea FSP, [FSP+8]
}

; defcode macro helps defining new native words in assembly
macro defcode name, namelen, flags=0, lbl, nvoc=link {
    align 8
label name_#lbl
   dq nvoc              ; link
nvoc = name_#lbl
   db namelen+flags             ; flags + length byte
   db name                ; the name
   align 8              ; padding to next 4 byte boundary
label lbl
   dq code_#lbl           ; codeword
label code_#lbl                 ; assembler code follows
}


macro defword name, namelen, flags=0, lblw, nvoc=link {
    align 8
label name_#lblw
   dq nvoc              ; link
nvoc = name_#lblw
   db namelen+flags             ; flags + length byte
   db name                ; the name
   align 8              ; padding to next 4 byte boundary
label lblw
   dq DOCOL
}

; defvar macro helps defining Forth variables in assembly

macro defvar name, namelen, flags=0, lbl, initial=0, nvoc=link {
    align 8
label name_#lbl
    dq nvoc              ; link
nvoc = name_#lbl
    db namelen+flags             ; flags + length byte
    db name                ; the name
    align 8              ; padding to next 4 byte boundary
label lbl
    dq DOVAR,0
label var_#lbl
    dq initial
    dq EXIT
}
        
; defconst macro helps defining Forth constants in assembly

macro defconst name, namelen, flags=0, lbl, value, nvoc=link {
    align 8
label name_#lbl
    dq nvoc              ; link
nvoc = name_#lbl
    db namelen+flags             ; flags + length byte
    db name                ; the name
    align 8              ; padding to next 4 byte boundary
label lbl
    dq DOCONST
label var_#lbl
    dq value
    dq EXIT
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
    dq DODOES,VODOES
label v_#lbl
    dq head,name_#lbl,vlink
vlink = name_#lbl
    dq EXIT
}

NULL   equ   0
STDOUT equ -11
STDIN  equ -10

section '.bss' data readable writeable

ADRIAN_VERSION             = 1
return_stack_size          = 8192
float_stack_size           = 4096
buffer_size                = 4096
initial_data_segment_size  = 262144

DS equ rsp
RS equ rbp
FSP equ r12
IP equ rsi
LSP equ r13
LBP equ r14

align 8
return_stack:
    rq return_stack_size
return_stack_top:
align 8
float_stack:
    rb float_stack_size
float_stack_top:   
align 8
buffer:
    rb buffer_size
con             rq 128
;align 8
;word_buffer:
;    rb 1+32

section '.text' code readable executable

;section '.text' code readable executable
  start:
    push rbp
    mov rbp, rsp
    mov [BasePtr], rbp ; save the base pointer for clean exit

    ; sub     rsp, 32
    ; xor     rax,rax
    ; mov     rax,QWORD [rax]  ; cause exception

; get the command line args
    sub rsp, 40
    call    [GetCommandLineA]
    mov     [ARGV+16], rax   ; +16 => DOCOL LIT addr
   ; mov     rcx, rax
   ; mov     rdx, [rsp+32]
   ;call   [CommandLineToArgvW]
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
    ;evoke   VirtualAlloc,0,initial_data_segment_size,0x1000,0x40

    add     rsp, 32
    
    ;sub     rsp, 32
    ;mov     rcx, ctrlc
    ;mov     rdx, 1
    ;call    SetConsoleCtrlHandler
    ;add     rsp, 32
    
    ; sub     rsp, 32
    ; mov     rcx, 1
    ; lea     rdx, [memerr]
    ; call    AddVectoredExceptionHandler
    ;call SetUnhandledExceptionFilter
    ; mov     rcx, 11
    ; lea     rdx, [memerr]
    ; call    signal
    ; mov     rcx, 15
    ; lea     rdx, [memerr]
    ; call    signal
    ; add     rsp, 32

    mov rax, [ARGV+16]
    mov     [var_S0], DS

    cmp byte [rax], 34 ; compare first char to double quote "
    je quoted
    jmp nonquoted
quoted:
        ;mov rax, [ARGV+16]
        inc rax
lp0:    ; skip to second quote
        cmp byte [rax], 34
        je nonquoted
        inc rax
        jmp lp0
nonquoted:
lp1:    ; skip to first space or end
        cmp byte [rax], 32
        je endb
        inc rax
        cmp byte [rax], 0
        je endd
        jmp lp1
endb:
lp2:    ; skip to next arg or end
;mov byte [rax],0
        cmp byte [rax], 32
        jne endc
        inc rax
        cmp byte [rax], 0
        je endd
        jmp lp2
endc:   ; now at the first word
        inc qword [ARGC+16] ; increment argc
        push rax  ; push addr of arg
lp3:
        ; skip to next space or end
        inc rax
        cmp byte [rax], 0
        je endd
        cmp byte [rax], 32
        jne lp3
        mov byte [rax],0
        inc rax
        jmp lp2
endd:
    mov rbx, [var_S0]  ; temp save of argv
    mov [ARGV+16],rbx  ; move top of stack to args
    
    ; save base of data stack to S0 - we will use the default system stack
    mov     [var_S0], DS
    
    ; save base of return stack to RSP
    mov     RS, return_stack_top

    ; save base of float stack to FSP
    mov     FSP, float_stack_top
    
    ; set up local stack pointers
    mov LSP, LOCSTACK+4096*8
    mov LBP, LSP ; base pointer starts at TOS

    sub     rsp, 32
    and     spl, 0xF0   ; align the stack
    lea     IP, [cold]  ; now start FORTH from cold start
    NEXT
    ; should never reach this!
    mov     rcx, 0
    call  [ExitProcess]

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

align 8
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
DOCOL:
    PUSHRSP IP
    add rax, 8
    mov IP, rax
    NEXT
DOCOL2:
    PUSHRSP IP
    add rax, 8
    mov IP, rax
    NEXT
DOVAR:
    add     rax, 16
    push    rax
    NEXT
DOVAL:
    ;push    qword [rax+16]
    push    qword [rax+8]
    NEXT
DO2VAL:
    ;push    qword [rax+24]
    ;push    qword [rax+16]
    push    qword [rax+16]
    push    qword [rax+8]
    NEXT
DOCONST:
    push    qword [rax+8]
    NEXT
DODOES:
    PUSHRSP IP
    mov IP, [rax+8]
    add rax, 16
    push rax
    NEXT

; the start address for the FORTH virtual machine
; i.e. cold start
align 8
ex_handl_start:
cold:
    dq ARGC,TOR
coldb:
    dq RFETCH,ZBRANCH,colde
    dq ARGV,RFETCH,ARGC,SWAP,MINUS,ONEPLUS
    dq CELLS,MINUS,FETCH,DUPF,STRLEN,INCLUDED
    dq FROMR,LIT,1,MINUS,TOR
    dq BRANCH,coldb
colde:
    dq RDROP,LIT,msg,LIT,msglen,TYPEF
cold_thread:
    dq QUIT,BYE

lpMsgBuf: rb 512

defcode "ERRORMSG",8,F_HIDDEN,ERRORMSG,link
    pop r8
    mov r15, rsp
    sub rsp, 32
    and spl, 0xF0
    mov rcx, 0
    call [GetLastError]
    mov rsp, r15
    push rax
    NEXT
    ; save the stack
    mov r15, rsp
    sub rsp, 32
    and spl, 0xF0
    ; set up call
    ;mov rcx,(0x1000 or 0x200) 
    ;mov rdx, 0
    ;mov r9, 0
    ;push lpMsgBuf
    ;push 256
    ;push 0
    ;call [FormatMessage]
    invoke FormatMessage,(0x1000 or 0x200),0,r8,0,lpMsgBuf,512,0
    mov rsp, r15
    push rax
    mov al, byte [lpMsgBuf]
    push rax
    push lpMsgBuf
    NEXT
;FormatMessage(
;        FORMAT_MESSAGE_ALLOCATE_BUFFER | 
;        FORMAT_MESSAGE_FROM_SYSTEM |
;        FORMAT_MESSAGE_IGNORE_INSERTS,
;        NULL,
;        dw,
;        MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
;        (LPTSTR) &lpMsgBuf,
;        0, NULL );
    
fromError:
    dq LIT,errmsg,LIT,errmsglen,TYPEF
    dq DUPF,HEX,DOT,DECIMAL,ERRORMSG
    dq DOT
    ;dq STRLEN,TYPEF,DOT,DOT
    dq QUIT
    
defvar "STATE",5,0,STATE,0
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
defvar "'EXPECT",7,0,TICKEXPECT,ACCEPT
defvar "'TAP",4,0,TICKTAP,KTAP
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
    dq DOVAR,0
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
    dq DOVAR,0
label var_LOCDICT
    dq 512 dup 0

label name_LOCSTACK
    dq link              ; link
link = name_LOCSTACK
    db 9             ; flags + length byte
    db "LOC-STACK"                ; the name
    align 8              ; padding to next 4 byte boundary
label LOCSTACK
    dq DOVAR,0
label var_LOCSTACK
    dq 4096 dup 0

label name_LOCVEC
    dq link              ; link
link = name_CONTEXT
    db 6+F_HIDDEN             ; flags + length byte
    db "LOCVEC"                ; the name
    align 8              ; padding to next 4 byte boundary
label LOCVEC
    dq DOVAR,0
label var_LOCVEC
    dq TOLOC0,TOLOC1,TOLOC2,TOLOC3,TOLOC4,TOLOC5,TOLOC6,TOLOC7

defvar "LOC-CNT",7,0,LOCCNT,0
defvar "LOC-OFFSET",10,0,LOCOFFSET,0
defvar "LOC-FLG",7,0,LOCFLG,0
defvar "OLD-DP",6,0,OLDDP,0
defvar "LP",2,0,LP,0
defvar "LSTATE",6,0,LSTATE,0 ; false
defvar "LOC-FRAME",9,0,LOCFRAME,0 ; false

defconst "VERSION",7,0,VERSION,ADRIAN_VERSION
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

defcode "CONSOLE",7,0,CONSOLE;,link
    frame
    invoke GetConsoleScreenBufferInfo,[WriteHandle],console
    endf
    xor rax,rax
    mov ax, word ptr console.srWind.r
    mov bx, word ptr console.srWind.l
    sub ax,bx
    inc ax
    push rax
    mov ax, word ptr console.srWind.b
    mov bx, word ptr console.srWind.t
    sub ax,bx
    inc ax
    push rax
    ;push word ptr console.dwSize.y
    NEXT

defcode "PRINT",5,0,PRINT
    mov rcx, msg
    call [printf]
    NEXT
    ; pop     rdx
    ; push    IP
    ; push    RS
    ; push    DS
    ; sub     rsp, 32
    ; lea     rcx, [fmt]
    ; call    printf
    ; add     rsp, 32
    ; pop     DS
    ; pop     RS
    ; pop     IP
    ; NEXT

defcode "?RX",3,0,QRX
; ( -- ch T | F )
    ;push    RS
    ;mov     RS, DS
    sub     rsp, 48
    mov     rcx, qword [ReadHandle]
    lea     rdx, [key_scratch]
    mov     r8, 1
    lea     r9, [Written]
    mov   qword [RSP + 4 * 8], NULL ; 5th param is NULL
    call    [ReadFile]
    ; sub     rsp, 32
    ; call    getchar ;_RX
    add     rsp, 48
    ; mov     DS, r15 ; restore stack
    ; mov     RS, r14
    cmp     rax, 0
    je      _ERROR
    mov     al, byte [key_scratch]
    push    rax
    push    -1
    NEXT
_ERROR:
    ;push    65
    push    0 ; FALSE
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
    pop     rcx
    ; mov     r15, DS ; save the stack
    ; sub     rsp, 32
    ; mov     rax, ~15
    ; and     rsp, rax ; align stack
    mov     byte [emit_scratch], cl
    sub     rsp, 32 + 8 + 8
    mov     rcx, qword [WriteHandle]
    lea     rdx, [emit_scratch]
    mov     r8, 1
    lea     r9, [Written]
    mov     qword [rsp + 4*8], NULL
    call    [WriteFile]
    add     rsp, 48
    ; mov     DS, r15 ; restore stack
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

defcode "CODE-EXIT",9,0,EXIT
    POPRSP  IP
    NEXT

defcode "LIT",3,0,LIT
    lodsq
    push    rax
    NEXT

defcode "EXECUTE",7,0,EXECUTE
    pop     rax
    jmp qword [rax]

defcode "BYE",3,0,BYE,rootlink
    ; emit a newline
    mov    cl, 10
    call    _TX
    mov    cl, 13
    call    _TX
    ;free the heap memory
    ;mov     rcx, qword [HeapHandle]
    ;mov     rdx, 0x0
    ;mov     r8, qword [HeapBase]
    ;sub     rsp, 32
    ;call    [HeapFree]   ; address of dictionary in rax
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
    ;push rax
    call    [ExitProcess]
   ; will never reach this !
   ; NEXT

defcode "BRANCH",6,0,BRANCH
    mov     rbx, [IP]
    mov     IP, rbx
    NEXT

defcode "0BRANCH",7,0,ZBRANCH
        pop     rax
        sub     rax, 0
        je      LBR1
        add     IP, 8
        NEXT
LBR1:   
        mov     rbx, [IP]
        mov     IP, rbx
        NEXT
    
defcode "!",1,0,FSTORE
    pop     rax
    pop     rbx
    mov     [rax], rbx
    NEXT

defcode "@",1,0,FETCH
    pop     rbx
    mov     rax, [rbx]
    push    rax
    NEXT

defcode "C!",2,0,STOREBYTE
    pop     rax
    pop     rbx
    mov     byte [rax], bl 
    NEXT

defcode "C@",2,0,FETCHBYTE
    pop     rax
    xor     rbx, rbx
    mov     byte  bl, [rax]
    push    rbx
    NEXT

defword "C,",2,0,COMMABYTE
; ( c -- )
; HERE C! 1 CHARS ALLOT ;
        dq HERE,STOREBYTE,LIT,1
        dq CHARS,ALLOT
        dq EXIT

defcode "RP@",3,0,RSPFETCH
    push    RS
    NEXT

defcode "RP!",3,0,RSPSTORE
    pop     RS
    NEXT

defcode ">R",2,0,TOR
    pop     rax
    PUSHRSP rax
    NEXT

defcode "R@",2,0,RFETCH
    mov     rax, [RS]
    push    rax
    NEXT

defcode "R>",2,0,FROMR
    POPRSP  rax
    push    rax
    NEXT
    
defcode "RDROP",5,0,RDROP
    add     RS, 8
    NEXT

defcode "2RDROP",6,0,TWORDROP
    add     RS, 16
    NEXT
    
defcode "DROP",4,0,DROP
    ;pop     rbx
    add DS,8
    NEXT

defcode "DUP",3,0,DUPF
    ;pop     rax
    ;push    rax
    ;push    rax
    mov rax,[DS]
    sub DS,8
    mov [DS],rax
    NEXT

defcode "SWAP",4,0,SWAP
    pop     rax
    pop     rbx
    push    rax
    push    rbx
    NEXT

defcode "OVER",4,0,OVER
    mov     rax, [DS+8]
    push    rax
    NEXT

defcode "SP@",3,0,DSPFETCH
    push    DS
    NEXT

defcode "SP!",3,0,DSPSTORE
    pop     DS
    NEXT

defcode "FSP@",4,0,FSPFETCH
    push    FSP
    NEXT

defcode "FSP!",4,0,FSPSTORE
    pop     FSP
    NEXT

defcode "0<",2,0,ZLT
    pop     rax
    mov     rcx, -1
    mov     rbx, 0
    cmp     rax, rbx
    cmovl   rbx, rcx
    push    rbx
    NEXT

defcode "AND",3,0,ANDF
    pop     rax
    pop     rbx
    and     rax, rbx
    push    rax
    NEXT

defcode "OR",2,0,ORF
    pop     rax
    pop     rbx
    or      rax,rbx
    push    rax
    NEXT

defcode "XOR",3,0,XORF
    pop     rax
    pop     rbx
    xor     rax,rbx
    push    rax
    NEXT

defcode "INVERT",6,0,INVERT
    pop     rax
    not     rax
    push    rax
    NEXT

defcode "UM+",3,0,UMPLUS
    pop     rbx
    pop     rax
    xor     rcx, rcx
    clc ; clear carry flag
    mov     rdx, 1
    xor     rcx, rcx
    add     rax, rbx
    cmovc   rcx, rdx
    push    rax
    push    rcx
    NEXT

defcode "OV+",3,0,OVPLUS
    mov     rax, 0x0
    inc     rax ; clear OF flag
    xor     rcx, rcx
    mov     rdx, 1
    pop     rax
    pop     rbx
    add     rax, rbx
    cmovo   rcx, rdx  ; tests for OF flag
    push    rax
    push    rcx
    NEXT

defcode "LSHIFT",6,0,LSHIFT
    pop     rcx
    pop     rax
    shl     rax, cl
    push    rax
    NEXT

defcode "RSHIFT",6,0,RSHIFT
    pop     rcx
    pop     rax
    shr     rax, cl
    push    rax
    NEXT

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
QUIT0:  dq CR,RZ,RSPSTORE,LIT,0,STATE,FSTORE
        dq S0,FETCH,DSPSTORE
        dq F0,FETCH,FSPSTORE
QUIT1:  dq QUERY,SPACE
        dq SOURCE,INTERPRET
        dq STATE,FETCH,LIT,0,EQUAL
        dq ZBRANCH,QUIT2
        dq SAYOK
        dq DEPTH,ZBRANCH,QUIT3
        dq LIT,58,EMIT,DEPTH,DOT
        dq BRANCH,QUIT4
QUIT3:  dq SPACE
QUIT4:  dq FDEPTH,ZBRANCH,QUIT2
        dq LIT,102,EMIT,LIT,58,EMIT
        dq FDEPTH,DOT
QUIT2:  dq CR,BRANCH,QUIT1
        dq EXIT

defword "DO$",3,0,DOSTRING
; R@ DUP 8+ SWAP @ ( a u )
; R> OVER ( a u b u )
; + ALIGNED 8+ ( a u b+u+8+pad )
; >R ( put next instruction onto return stack )
        dq RFETCH,DUPF,LIT,8,PLUS,SWAP,FETCH
        dq FROMR,OVER,PLUS,ALIGNED
        dq LIT,8,PLUS,TOR
        dq EXIT

defword "LITSTRING",9,0,LITSTRING
; R@ DUP 8+ SWAP ; ( a u )
; R> OVER ( a u b u )
; + ALIGNED 8+ ( a u b+u+4+pad )
; >R ( put next instruction onto return stack )
        dq RFETCH,DUPF,LIT,8,PLUS,SWAP,FETCH
        dq FROMR,OVER,PLUS,ALIGNED
        dq LIT,8,PLUS,TOR
        dq EXIT

defword "DOP$",4,0,DOPSTRING
; R@ DUP C@ OVER ( a u a )
; + 1+ ALIGNED ( a b+u+1+pad )
; >R ( put next instruction onto return stack )
        dq RFETCH,DUPF,FETCHBYTE,OVER
        dq PLUS,LIT,1,PLUS,ALIGNED
        dq TOR
        dq EXIT

defcode "?DUP",4,0,QDUP
; DUP IF DUP THEN ;
        mov rax, [DS]
        cmp rax, 0
        je @f
        push rax
@@:     NEXT

defcode "ROT",3,0,ROT       
; >R SWAP R> SWAP ;
        pop rax
        pop rbx
        pop rcx
        push rbx
        push rax
        push rcx
        NEXT

defcode "-ROT",4,0,MROT
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

defword "ROLL",4,0,ROLL
; ( xn ... x0  m -- xn-1 ... x0 xn )
; DUP 0<= IF DROP ELSE SWAP >R 1- RECURSE R> SWAP THEN ;
        dq DUPF,ZLTEQ,ZBRANCH,ROL1
        dq DROP,BRANCH,ROL2
ROL1:   dq SWAP,TOR,ONEMINUS,ROLL,FROMR,SWAP
ROL2:   dq EXIT

defcode "2DROP",5,0,TWODROP
; DROP DROP ;
        ;dq DROP,DROP
        ;dq EXIT
        add DS, 16
        NEXT

defcode "2DUP",4,0,TWODUP
; OVER OVER ;
        ;dq OVER,OVER
        ;dq EXIT
        mov rax, [DS]
        mov rbx, [DS+8]
        push rbx
        push rax
        NEXT

defcode "2OVER",5,0,TWOOVER
; >R 2DUP R> ROT ROT ;
        ;dq TOR,TWODUP,FROMR,ROT,ROT
        ;dq EXIT
        mov rax, [DS+16]
        mov rbx, [DS+24]
        push rbx
        push rax
        NEXT

defcode "2SWAP",5,0,TWOSWAP
; ROT >R ROT R>
        ;dq ROT,TOR,ROT,FROMR
        ;dq EXIT
        mov rax, [DS]
        mov rbx, [DS+8]
        mov rcx, [DS+16]
        mov rdx, [DS+24]
        mov [DS+16], rax
        mov [DS+24], rbx
        mov [DS],   rcx
        mov [DS+8], rdx
        NEXT

defword "2>R",3,0,TWOTOR
; SWAP >R >R ;
        dq FROMR,MROT,SWAP,TOR,TOR,TOR
        dq EXIT

defword "2R>",3,0,TWOFROMR
; R> R> SWAP ;
        dq FROMR,FROMR,FROMR,SWAP,ROT,TOR
        dq EXIT

defword "2R@",3,0,TWORFETCH
; RSP; DUP ; SWAP CELL+ ; SWAP ;
        dq RSPFETCH,CELLPLUS,DUPF,FETCH,SWAP
        dq CELLPLUS,FETCH,SWAP
        dq EXIT

defcode "NOT",3,0,NOTF
; -1 XOR ;
        ;dq LIT,-1,XORF
        ;dq EXIT
        pop rbx
        sub rbx, 1
        sbb rbx, rbx
        push rbx
        NEXT

defcode "NEGATE",6,0,NEGATE
; NOT 1 + ;
        ;dq NOTF,LIT,1,PLUS
        ;dq EXIT
        mov rax, [DS]
        neg rax
        mov [DS], rax
        NEXT

defword "DNEGATE",7,0,DNEGATE
; NOT >R NOT 1 UM+ R> + ;
        dq NOTF,TOR,NOTF,LIT,1,UMPLUS,FROMR,PLUS
        dq EXIT


defcode "+",1,0,PLUS
; UM+ DROP ;
    pop     rax
    ;add     rax, [DS]
    ;mov     [DS], rax
    add [DS],rax
    NEXT    

defword "D+",2,0,DPLUS
; >R SWAP >R UM+ R> R> + + ; 
        dq TOR,SWAP,TOR,UMPLUS
        dq FROMR,FROMR,PLUS,PLUS
        dq EXIT

defcode "-",1,0,MINUS
    pop     rax
    pop     rbx
    sub     rbx, rax 
    push    rbx
    NEXT 

defword "D-",2,0,DMINUS
; DNEGATE D+ ;
        dq DNEGATE,DPLUS
        dq EXIT

defcode "ABS",3,0,ABSF
; DUP 0< IF NEGATE THEN ;
        ;dq DUPF,ZLT,ZBRANCH,ABS1
        ;dq NEGATE
;ABS1:   dq EXIT
        mov rax, [DS]
        mov rcx, rax
        sar rcx, 63
        xor rax, rcx
        sub rax, rcx
        mov [DS], rax
        NEXT

defcode "=",1,0,EQUAL
; ( w w -- t )
; XOR IF 0 EXIT THEN -1 ;
;        dq XORF,ZBRANCH,EQ1
;        dq LIT,0
;        dq EXIT
;EQ1:    dq LIT,-1
;        dq EXIT
        pop rbx
        pop rax
        sub rbx, rax
        sub rbx, 1
        sbb rbx, rbx
        push rbx
        NEXT

defcode "<>",2,0,NEQUAL
; ( w w -- t )
; EQUAL INVERT ;
        ;dq EQUAL,INVERT
        ;dq EXIT
        pop rbx
        pop rax
        sub rax, rbx
        neg rax
        sbb rbx, rbx
        push rbx
        NEXT

defcode "U<",2,0,ULT
; ( u u -- t )
; 2DUP XOR 0< IF SWAP DROP 0< EXIT THEN - 0< ;
;        dq TWODUP,XORF,ZLT,ZBRANCH,ULT1
;        dq SWAP,DROP,ZLT,EXIT
;ULT1:   dq MINUS,ZLT
;        dq EXIT
        pop rbx
        pop rax
        cmp rax, rbx
        sbb rbx, rbx
        push rbx
        NEXT

defcode "U>",2,0,UGT
; ( u u -- t )
        mov rdx, 0
        mov rcx, -1
        pop rbx
        pop rax
        cmp rax, rbx
        cmova rdx, rcx
        push rdx
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
        pop rbx
        pop rax
        cmp rax, rbx
        cmovl rdx, rcx
        push rdx
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
        pop rbx
        pop rax
        cmp rax, rbx
        cmovg rdx, rcx
        push rdx
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
        pop rbx
        pop rax
        cmp rax, rbx
        cmovge rdx, rcx
        push rdx
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
        pop rbx
        pop rax
        cmp rax, rbx
        cmovle rdx, rcx
        push rdx
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
        pop rax
        mov rbx, 0x7fffffffffffff
        dec rax
        cmp rax, rbx
        sbb rax, rax
        push rax
        NEXT

defcode "0=",2,0,ZEQ
; ( n -- t )
; 0 = ;
;                dq LIT,0,EQUAL
;                dq EXIT
        pop rax
        sub rax, 1
        sbb rax, rax
        push rax
        NEXT

defcode "0<>",3,0,ZNEQ
; ( n -- t )
; 0 = NOT ;
;                dq LIT,0,EQUAL,NOTF
;                dq EXIT
        pop rax
        sub rax, 1
        sbb rax, rax
        not rax
        push rax
        NEXT

defword "0<=",3,0,ZLTEQ
; ( n -- t )
; DUP 0= SWAP 0< OR ;
                dq DUPF,ZEQ
                dq SWAP,ZLT,ORF
                dq EXIT

defword "D<",2,0,DLT
; ( d d -- t )
; D- NIP 0< ;
                dq DMINUS,NIP,ZLT
                dq EXIT

defword "D>",2,0,DGT
; ( d d -- t )
; 2SWAP D<
                dq TWOSWAP,DLT
                dq EXIT

defword "D=",2,0,DEQ
; ( d d -- t )
; ROT = -ROT = AND
                dq ROT,EQUAL,MROT,EQUAL,ANDF
                dq EXIT

defcode "1+",2,0,ONEPLUS
; ( n -- n )
; 1 +
          ; dq LIT,1,PLUS
          ; dq EXIT
    ;pop rax
    ;inc rax
    ;push rax
    inc qword [rsp]
    NEXT
 
defcode "1-",2,0,ONEMINUS
; ( n -- n )
; 1 -
;                dq LIT,1,MINUS
;                dq EXIT
    ;pop rax
    ;sub rax, 1
    ;push rax
    dec qword [rsp]
    NEXT

defcode "2*",2,0,TWOTIMES
; ( n -- n )
; 2 *
;                dq LIT,2,MULF
;                dq EXIT
    mov rax,[DS]
    add [DS], rax
    NEXT

defcode "2/",2,0,TWODIV
; ( n -- n )
; 2 /
;                dq LIT,2,DIVF
;                dq EXIT
    pop rax
    sar rax, 1
    push rax
    NEXT

defcode "MAX",3,0,MAX
; ( n n -- n )
; 2DUP < IF SWAP THEN DROP ;
;        dq TWODUP,LTF,ZBRANCH,MAX1
;        dq SWAP
;MAX1:   dq DROP
;        dq EXIT
    pop rbx
    pop rax
    cmp rbx, rax
    jg @f
    mov rbx, rax  
@@: push rbx
    NEXT

defcode "MIN",3,0,MIN
; ( n n -- n )
; 2DUP SWAP < IF SWAP THEN DROP ;
;        dq TWODUP,SWAP,LTF,ZBRANCH,MIN1
;        dq SWAP
;MIN1:   dq DROP
;        dq EXIT
    pop rbx
    pop rax
    cmp rbx, rax
    jl @f
    mov rbx, rax
@@: push rbx
    NEXT

defcode "WITHIN",6,0,WITHIN
; ( u ul uh -- t ) \ ul <= u < uh
; OVER - >R - R> U< ;
;        dq OVER,MINUS,TOR
;        dq MINUS,FROMR,ULT
;        dq EXIT
    pop rbx
    pop rax
    pop rcx
    sub rbx, rax
    sub rcx, rax
    sub rcx, rbx
    sbb rbx, rbx
    push rbx
    NEXT

defword "S>D",3,0,STOD
; ( u -- d )
; DUP 0< ;
        dq DUPF,ZLT
        dq EXIT

defword "D>S",3,0,DTOS
; ( d -- u )
; DROP ;
        dq DROP
        dq EXIT

defword "DABS",4,0,DABS
; ( d -- |d| )
; DUP 0< IF DNEGATE THEN ;
        dq DUPF,ZLT,ZBRANCH,DAB1
        dq DNEGATE
DAB1:   dq EXIT

defcode "SM/REM",6,0,SMDIVREM
        pop     rbx
        pop     rdx
        pop     rax
        idiv    rbx
        push    rdx
        push    rax
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
    pop rbx
    pop rdx
    pop rax
    div rbx
    push rdx
    ;mov rbx, rax
    mov rdx, rcx
    push rax
    NEXT

defword "UD/MOD",6,0,UDDIVMOD
; ( ud u -- u u )
; >R 0 R; UM/MOD ROT ROT R> UM/MOD ROT ;
        dq TOR,LIT,0,RFETCH,UMDIVMOD
        dq ROT,ROT,FROMR,UMDIVMOD,ROT
        dq EXIT
        
defword "FM/MOD",6,0,FMDIVMOD
; floored division
; ( d n -- r q )
; DUP 0< DUP >R
; IF NEGATE >R DNEGATE R>
; THEN >R DUP 0< IF R; + THEN R> UM/MOD R>
; IF SWAP NEGATE SWAP THEN ;
        dq DUPF,ZLT,DUPF,TOR
        dq ZBRANCH,MMOD1
        dq NEGATE,TOR,DNEGATE,FROMR
MMOD1:  dq TOR,DUPF,ZLT,ZBRANCH,MMOD2
        dq RFETCH,PLUS
MMOD2:  dq FROMR,UMDIVMOD,FROMR
        dq ZBRANCH,MMOD3
        dq SWAP,NEGATE,SWAP
MMOD3:  dq EXIT

defword "/MOD",4,0,DIVMOD
; ( n n -- r q )
; OVER 0< SWAP M/MOD ;
        dq OVER,ZLT,SWAP,FMDIVMOD
        dq EXIT

;defword "MOD",3,0,MODF
; ( n n  -- r )
; /MOD DROP ;
;        dq DIVMOD,DROP
;        dq EXIT
defcode "MOD",3,0,MODF
        pop rbx
        mov rdx, 0
        pop rax
        div rbx
        push rdx
        NEXT

;defword "/",1,0,DIVF
; ( n n -- q )
; /MOD SWAP DROP ;
;        dq DIVMOD,SWAP,DROP
;        dq EXIT
defcode "/",1,0,DIVF
        pop rbx
        mov rdx, 0
        pop rax
        div rbx
        push rax
        NEXT

defword "UM*",3,0,UMMUL
; ( u u -- ud )
; 0 SWAP ( u1 0 u2 ) 32
; BEGIN DUP
; WHILE >R DUP UM+ >R >R DUP UM+ R> + R>
;  IF >R OVER UM+ R> + THEN R> 1 -
; REPEAT DROP >R NIP R> ;
        dq LIT,0,SWAP,LIT,64
MBEG:   dq DUPF,ZBRANCH,MEND
        dq TOR,DUPF,UMPLUS,TOR,TOR
        dq DUPF,UMPLUS,FROMR,PLUS,FROMR
        dq ZBRANCH,MTH1
        dq TOR,OVER,UMPLUS,FROMR,PLUS
MTH1:   dq FROMR,LIT,1,MINUS
        dq BRANCH,MBEG
MEND:   dq DROP,TOR,NIP,FROMR
        dq EXIT

defword "*",1,0,MULF
; ( n n -- n )
; UM* DROP ;
        dq UMMUL,DROP
        dq EXIT

defword "M*",2,0,MMUL
; ( n n -- d )
; 2DUP XOR 0< >R ABS SWAP ABS 
; UM* R> IF NEGATE THEN ;
        dq TWODUP,XORF,ZLT,TOR
        dq ABSF,SWAP,ABSF
        dq UMMUL,FROMR
        dq ZBRANCH,MMUL1
        dq DNEGATE
MMUL1:  dq EXIT

defword "*/MOD",5,0,MULDIVMOD
; ( n n n -- r q )
; >R M* R> M/MOD ;
        dq TOR,MMUL,FROMR,FMDIVMOD
        dq EXIT

defword "*/",2,0,MULDIV
; ( n n n -- q )
; */MOD SWAP DROP ;
        dq MULDIVMOD,SWAP,DROP
        dq EXIT

defword "CELL-",5,0,CELLMINUS
; ( a -- a)
; -4 + ;
        dq LIT,-8,PLUS
        dq EXIT

defword "CELL+",5,0,CELLPLUS
; ( a -- a )
; 8 + ;
        dq LIT,8,PLUS
        dq EXIT

defword "FLOAT+",6,0,FLOATPLUS
; ( a -- a )
; 8 + ;
        dq CELLPLUS
        dq EXIT
   
defword "FLOATS",6,0,FLOATS
; ( n1 -- n2 )
        dq LIT, 8, MULF
        dq EXIT
    
defword "CELLS",5,0,CELLS
; ( n -- n )
; 4 * ;
        dq LIT,8,MULF
        dq EXIT

defword "CELL",4,0,CELL
; ( -- n )
; 4 ;
        dq LIT,8
        dq EXIT

; ;defword "ALIGNED",7,,ALIGNED
; ;; ( b -- a )
; ;; DUP 0 4 UM/MOD DROP DUP
; ;; IF 4 SWAP - THEN + ;
; ;        dq DUP,LIT,0,LIT,4,UMDIVMOD
; ;        dq DROP,DUP,ZBRANCH,ALI1
; ;        dq LIT,4,SWAP,MINUS
; ;ALI1:   dq PLUS
; ;        dq EXIT

defword "ALIGNED",7,0,ALIGNED
; ( b -- a )
; 3 + 3 INVERT AND ;
                dq LIT,7,PLUS,LIT,7
                dq INVERT,ANDF
                dq EXIT

defword "ALIGN",5,0,ALIGNF
; ( -- )
; HERE ALIGNED DP !
                dq HERE,ALIGNED,FDP,FSTORE
                dq EXIT

defword "FALIGNED",8,0,FALIGNED
; ( b -- a )
; ALIGNED ;
                dq ALIGNED
                dq EXIT

defword "FALIGN",6,0,FALIGNF
; ( -- )
; ALIGN ;
                dq ALIGNF
                dq EXIT
        
defword "BL",2,0,BLF
; ( -- 32 )
; 32 ;
                dq LIT,32
                dq EXIT

defword ">CHAR",5,0,TOCHAR
; 0x7F AND DUP 127 BL WITHIN
; IF DROP 95 THEN ;
        dq LIT,0x7F,ANDF,DUPF
        dq LIT,127,BLF,WITHIN
        dq ZBRANCH,CHAR1
        dq DROP,LIT,95
CHAR1:  dq EXIT

defword "CHARS",5,0,CHARS
; ( n -- n )
; 1 * ; actually perform a noop and compile nothing
                dq EXIT

defword "CHAR+",5,0,CHARPLUS
; ( n -- n )
; 1 + ; 
                dq ONEPLUS
                dq EXIT

defword "DEPTH",5,0,DEPTH
; ( -- n ) 
; DSP; S0 ; SWAP - 4 / ;
                dq DSPFETCH,S0,FETCH
                dq SWAP,MINUS,LIT,8,DIVF
                dq EXIT

defword "PICK",4,0,PICK
; ( +n -- w )
; 1 + CELLS DSP; + ; ;
                dq LIT,1,PLUS,CELLS
                dq DSPFETCH,PLUS,FETCH
                dq EXIT

defword "TUCK",4,0,TUCK
; ( n m -- m n m )
; SWAP OVER ;
                dq SWAP,OVER
                dq EXIT

defword "NIP",3,0,NIP
; ( n m -- m )
; SWAP DROP ;
                dq SWAP,DROP
                dq EXIT

defword "+!",2,0,PLUSSTORE
; ( n a -- ) 
; SWAP OVER ; + SWAP ! ;
                dq SWAP,OVER,FETCH,PLUS,SWAP,FSTORE
                dq EXIT

defword "2!",2,0,TWOSTORE
; ( d a -- )
; SWAP OVER ! CELL+ ! ;
                dq SWAP,OVER,FSTORE
                dq CELLPLUS,FSTORE
                dq EXIT

defword "2@",2,0,TWOFETCH
; ( a -- d )
; DUP CELL+ ; SWAP ; ;
                dq DUPF,CELLPLUS,FETCH
                dq SWAP,FETCH
                dq EXIT

defword "COUNT",5,0,COUNT
; ( b -- b +n ) 
; DUP 1 + SWAP C; ;
                dq DUPF,LIT,1,PLUS
                dq SWAP,FETCHBYTE
                dq EXIT

defword "HERE",4,0,HERE
; ( -- a )
; DP ; ;
                dq FDP,FETCH
                dq EXIT

defword "ALLOT",5,0,ALLOT
; ( n -- )
; DP +! ;
                dq FDP,PLUSSTORE,EXIT


defcode "FREE",4,0,FREE
defcode "RESIZE",6,0,RESIZE

defword "PAD",3,0,PAD
; ( -- a )
; HERE 80 + ;
                dq HERE,LIT,80,PLUS
                dq EXIT

;defword "TIB",3,,TIB
;; ( -- a )
;; TIB ;
;        dq LIT,buffer
;        dq EXIT

defword "CMOVE",5,0,CMOVEF
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
       ; dq CR,LIT,666,DOTS,DROP
CMBEG1:  dq QDUP,ZBRANCH,CMEND
        dq TOR,TOR,DUPF,FETCHBYTE
        dq RFETCH,STOREBYTE,LIT,1,PLUS
        dq FROMR,LIT,1,PLUS
        dq FROMR,LIT,1,MINUS
       ; dq CR,DOTS
        dq BRANCH,CMBEG1
CMEND:  dq TWODROP,EXIT
        
defword "CMOVE>",6,0,CMOVETO
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
         dq DUPF,TOR,PLUS,ONEMINUS,SWAP
         dq RFETCH,PLUS,ONEMINUS
         dq SWAP,FROMR
CMBEGT:  dq QDUP,ZBRANCH,CMENDT
         dq TOR,TOR,DUPF,FETCHBYTE
         dq RFETCH,STOREBYTE,LIT,1,MINUS
         dq FROMR,LIT,1,MINUS
         dq FROMR,LIT,1,MINUS
         dq BRANCH,CMBEGT
CMENDT:  dq TWODROP,EXIT

defword "COMPARE",7,0,COMPARE
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
    dq TOR,SWAP,TOR,SWAP
cmpb:
    dq TWOFROMR,TWODUP,ONEMINUS,SWAP
    dq ONEMINUS,SWAP,TWOTOR,ZEQ
    dq SWAP,ZEQ,ORF,ZBRANCH,CMP1
    dq TWODROP,TWOFROMR,TWODUP,LTF,ZBRANCH,CMP4
    dq TWODROP,LIT,1,EXIT
CMP4:
    dq TWODUP,GTF,ZBRANCH,CMP5
    dq TWODROP,LIT,-1,EXIT
CMP5:
    dq TWODROP,LIT,0,EXIT
CMP1:
    dq TWODUP,FETCHBYTE,SWAP,FETCHBYTE
    dq EQUAL,ZBRANCH,cmpe
    dq ONEPLUS,SWAP,ONEPLUS,SWAP
    dq BRANCH,cmpb
cmpe:
    dq FETCHBYTE,SWAP,FETCHBYTE,SWAP,MINUS
    dq ZGT,ZBRANCH,CMP2
    dq LIT,-1,BRANCH,CMP3
CMP2:
    dq LIT,1
CMP3:
    dq TWORDROP,EXIT

defword "FILL",4,0,FILL
;  ( b u c -- )
; SWAP >R SWAP BEGIN R; WHILE   ( c b | u )
; 2DUP C! 1 + R> 1 - >R REPEAT
; 2DROP R> DROP ;
        dq SWAP,TOR,SWAP
FBEG:   dq RFETCH,ZBRANCH,FEND
        dq TWODUP,STOREBYTE
        dq LIT,1,PLUS
        dq FROMR,LIT,1,MINUS,TOR
        dq BRANCH,FBEG
FEND:   dq TWODROP,FROMR,DROP
        dq EXIT

defword "ERASE",5,0,ERASE
;  ( b u -- )
; 0 SWAP >R SWAP BEGIN R; WHILE   ( c b | u )
; 2DUP C! 1 + R> 1 - >R REPEAT
; 2DROP R> DROP ;
        dq BLF,SWAP,TOR,SWAP
EBEG:   dq RFETCH,ZBRANCH,EEND
        dq TWODUP,STOREBYTE
        dq LIT,1,PLUS
        dq FROMR,LIT,1,MINUS,TOR
        dq BRANCH,EBEG
EEND:   dq TWODROP,FROMR,DROP
        dq EXIT

defword "-TRAILING",9,0,MTRAILING  
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
        dq DUPF,ZGT,ZBRANCH,TRAIL1
TRABEG: dq TWODUP,LIT,1,MINUS,CHARS,PLUS
        dq FETCHBYTE,BLF,EQUAL
        dq OVER,ZGT,ANDF
        dq ZBRANCH,TRAEND
        dq LIT,1,MINUS
        dq BRANCH,TRABEG
TRAEND: 
TRAIL1: dq EXIT

defword "PACK$",5,0,PACKS
; ( b u a -- a ) null fill
; DUP >R 2DUP C! 1+ SWAP CMOVE R> ;
        dq DUPF,TOR,TWODUP,STOREBYTE
        dq ONEPLUS,SWAP,CMOVEF
        dq FROMR,EXIT

defword "DIGIT",5,0,DIGIT
; ( u -- c )
; 9 OVER < 7 AND + 48 + ;
     dq LIT,9,OVER,LTF,LIT,7,ANDF,PLUS
     dq LIT,48,PLUS
     dq EXIT

defword "EXTRACT",7,0,EXTRACT
; ( n base -- n c ) 
; 0 SWAP UM/MOD SWAP DIGIT ;
     dq LIT,0,SWAP,UMDIVMOD
     dq SWAP,DIGIT
     dq EXIT

defword "HOLD",4,0,HOLD
; ( c -- ) 
; HLD @ 1 - DUP HLD ! C! ;
     dq HLD,FETCH,LIT,1
     dq MINUS,DUPF,HLD
     dq FSTORE,STOREBYTE
     dq EXIT

defword "HOLDS",5,0,HOLDS
; ( a u -- ) 
; BEGIN DUP WHILE 1- 2DUP + C@ HOLD REPEAT 2DROP ;
HOLDB:
        dq DUPF,ZBRANCH,HOLDE
        dq ONEMINUS,TWODUP,PLUS,FETCHBYTE,HOLD
        dq BRANCH,HOLDB
HOLDE:
        dq TWODROP
        dq EXIT

defword "<#",2,0,BRAHASH
; ( -- )
; PAD HLD ! ;
        dq PAD,HLD,FSTORE
        dq EXIT

defword "#",1,0,HASH
; ( ud -- u ) 
;; BASE ; EXTRACT HOLD ;
; BASE ; UD/MOD ROT DIGIT HOLD
        ;dq BASE,FETCH,EXTRACT,HOLD
        dq BASE,FETCH,UDDIVMOD
        dq ROT,DIGIT,HOLD
        dq EXIT

defword "#S",2,0,HASHS
; ( u -- 0 )
;; BEGIN # DUP WHILE REPEAT ;
; BEGIN # 2DUP OR 0= UNTIL ;
HASBEG: dq HASH,TWODUP,ORF,LIT,0,EQUAL
        dq ZBRANCH,HASBEG
HASEND: dq EXIT

defword "SIGN",4,0,SIGN
; ( n -- )
; 0< IF 45 HOLD THEN ;
        dq ZLT,ZBRANCH,SIGN1
        dq LIT,45,HOLD
SIGN1:  dq EXIT

defword "#>",2,0,HASHBRA
; ( ud -- b u )
; 2DROP HLD ; PAD OVER - ;
     dq TWODROP,HLD,FETCH
     dq PAD,OVER,MINUS
     dq EXIT

defword "STR",3,0,STRF
; ( d -- b u )
; DUP >R DABS <# #S R> SIGN #> ;
        dq DUPF,TOR,DABS,BRAHASH
        ;dq LIT,0,HASHS
        dq HASHS
        dq FROMR
        dq SIGN
        dq HASHBRA
        dq EXIT

defword "HEX",3,0,HEX
; ( -- )
; 16 BASE ! ;
        dq LIT,16,BASE,FSTORE
        dq EXIT

defword "DECIMAL",7,0,DECIMAL
; ( -- )
; 10 BASE ! ;
        dq LIT,10,BASE,FSTORE
        dq EXIT

defword "BINARY",6,0,BINARY
; ( -- )
; 2 BASE ! ;
        dq LIT,2,BASE,FSTORE
        dq EXIT

defword ".R",2,0,DOTR
; ( n +n -- ) display a signed int in
; n columns, right justified
; >R 0 str
; R> OVER - SPACES TYPE
        dq TOR,LIT,0,STRF
        dq FROMR,OVER,MINUS,LIT,0,MAX,SPACES,TYPEF
        dq EXIT

defword "U.R",3,0,UDOTR
; ( u +n -- )
; >R <# 0 #S #> R>
; OVER - SPACES TYPE ;
        dq TOR,BRAHASH,LIT,0,HASHS,HASHBRA
        dq FROMR,OVER,MINUS,LIT,0,MAX,SPACES,TYPEF
        dq EXIT

defword "U.",2,0,UDOT
; ( u -- )
; <# 0 #S #> TYPE SPACE ;
        dq BRAHASH,LIT,0
        dq HASHS,HASHBRA
        dq TYPEF,SPACE
        dq EXIT

defword ".",1,0,DOT
; ( n -- ) 
; <# DUP ABS 0 #S ROT SIGN #> TYPE SPACE ;
        dq DEPTH,LIT,0,NEQUAL,ZBRANCH,DOT1
        dq BRAHASH,DUPF,ABSF,LIT,0
        dq HASHS,ROT,SIGN,HASHBRA
        dq TYPEF,SPACE,BRANCH,DOT2
DOT1:   dq LIT,word2,COUNT,TYPEF,SPACE
DOT2:   dq EXIT

defword "D.",2,0,DDOT
; ( d -- ) 
; TUCK DABS <# #S ROT SIGN #>
; TYPE SPACE ;
        dq DEPTH,LIT,0,NEQUAL,ZBRANCH,DOT3
        dq TUCK,DABS,BRAHASH,HASHS,ROT,SIGN,HASHBRA
        dq TYPEF,SPACE,BRANCH,DOT4
DOT3:   dq LIT,word2,COUNT,TYPEF,SPACE
DOT4:   dq EXIT

defword "UD.",3,0,UDDOT
; ( ud -- ) 
; <# #S #>
; TYPE SPACE ;
        dq DEPTH,LIT,0,NEQUAL,ZBRANCH,DOT5
        dq BRAHASH,HASHS,HASHBRA
        dq TYPEF,SPACE,BRANCH,DOT6
DOT5:   dq LIT,word2,COUNT,TYPEF,SPACE
DOT6:   dq EXIT

defword "UD.R",4,0,UDDOTR
; ( ud n -- ) 
; >R <# #S #> R> OVER - SPACES TYPE ;
        dq DEPTH,LIT,0,NEQUAL,ZBRANCH,DOT7
        dq TOR,BRAHASH,HASHS,HASHBRA
        dq FROMR,OVER,MINUS,SPACES
        dq TYPEF,BRANCH,DOT8
DOT7:   dq LIT,word2,COUNT,TYPEF,SPACE
DOT8:   dq EXIT

defword "D.R",3,0,DDOTR
; ( ud n -- ) 
; >R TUCK DABS <# #S ROT SIGN #> R> OVER - SPACES TYPE ;
        dq DEPTH,LIT,0,NEQUAL,ZBRANCH,DOT9
        dq TOR,TUCK,DABS,BRAHASH,HASHS
        dq ROT,SIGN,HASHBRA
        dq FROMR,OVER,MINUS,SPACES
        dq TYPEF,BRANCH,DOT10
DOT9:   dq LIT,word2,COUNT,TYPEF,SPACE
DOT10:  dq EXIT

defword "UWIDTH",6,0,UWIDTH 
; ( u -- width )
; BASE ; / ( rem quot )
; ?DUP IF  ( if quotient <> 0 then )
;  RECURSE 1+ ( return 1+recursive call )
; ELSE
;  1  ( return 1 )
; THEN
;;
        dq BASE,FETCH,DIVF
        dq QDUP,ZBRANCH,UW1
        dq UWIDTH,LIT,1,PLUS
        dq BRANCH,UW2
UW1:    dq LIT,1
UW2:    dq EXIT

defword ".S",2,0,DOTS
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
        dq LIT,60,EMIT
        dq DEPTH,UDOT
        dq LIT,8,EMIT
        dq LIT,62,EMIT,SPACE
        dq S0,FETCH,LIT,8,MINUS
DSBEG:  dq DUPF,DSPFETCH,LIT,8,PLUS
        dq GTF,ZBRANCH,DSEND
        dq DUPF,FETCH,DOT;,SPACE
        dq LIT,8,MINUS
        dq BRANCH,DSBEG
DSEND:  dq DROP,EXIT

defword "?",1,0,QUEST
; ( a -- )
; ; . ; display the contents in a cell
        dq FETCH,DOT
        dq EXIT

defword "DIGIT?",6,,DIGITQ
; ( c  -- u t )
; BASE ; >R 48 - 9 OVER <
; IF 7 - DUP 10 < OR THEN DUP R> U< ;
        dq BASE,FETCH
        dq TOR,LIT,48,MINUS
        dq LIT,9,OVER,LTF
        dq ZBRANCH,DIGI1
        dq LIT,7,MINUS,DUPF,LIT,10
        dq LTF,ORF
DIGI1:  dq DUPF,FROMR,ULT
        dq EXIT

defword "S>NUMBER?",9,0,STONUMBERQ
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
        dq OVER,FETCHBYTE,DUPF,LIT,45,EQUAL
        dq ZBRANCH,TN1
        dq LIT,1,TOR,DROP,SWAP,LIT,1,PLUS
        dq SWAP,LIT,1,MINUS,OVER,FETCHBYTE
        dq BRANCH,TN2
TN1:    dq LIT,0,TOR
TN2:    dq LIT,48,MINUS,LIT,0,LTF
        dq ZBRANCH,TN3
        dq TWODROP,DROP,FROMR,FROMR
        dq ZBRANCH,TN4
        dq NEGATE
TN4:    dq LIT,0,EXIT 
        dq BRANCH,TN10
TN3:    dq LIT,0,TOR
TNBEG:  dq OVER,FETCHBYTE,TOR,SWAP
        dq LIT,1,PLUS,SWAP,LIT,1,MINUS
        dq FROMR
        dq LIT,48,MINUS,OVER,LIT,0,GTE
        dq OVER,LIT,10,LTF,ANDF
        dq ZBRANCH,TNEND
        dq FROMR,BASE,FETCH,MULF,TOR
        dq DUPF,LIT,9,GTF,ZBRANCH,TN5
        dq LIT,17,MINUS,DUPF,LIT,0,LTF
        dq ZBRANCH,TN6
        dq TWODROP,DROP,FROMR,BASE,FETCH,DIVF
        dq FROMR
        dq ZBRANCH,TN7
        dq NEGATE
TN7:    dq LIT,0,EXIT
TN6:    dq LIT,10,PLUS
TN5:    dq DUPF,BASE,FETCH,GTF
        dq ZBRANCH,TN8
        dq TWODROP,DROP,FROMR,BASE,FETCH
        dq DIVF,FROMR
        dq ZBRANCH,TN9
        dq NEGATE
TN9:    dq LIT,0,EXIT
TN8:    dq FROMR,PLUS,TOR
        dq BRANCH,TNBEG
TNEND:
TN10:   dq TWODROP,DROP,FROMR,FROMR
        dq ZBRANCH,TN11
        dq NEGATE
TN11:   dq LIT,-1
        dq EXIT

defword "ASCII",5,F_IMMED,ASCII
; ( <char> -- char , state smart )
; parse-name drop c;
; state ;
; IF [compile] literal
; THEN ; immediate
        dq PARSENAME,DROP,FETCHBYTE,STATE,FETCH
        dq ZBRANCH,ASC1
        dq LITERAL
ASC1:   dq EXIT

defword ">DIGIT",6,0,TODIGIT
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
        dq TOR,DUPF,LIT,97,LTF,NOTF
        dq ZBRANCH,DIG1
        dq LIT,97,MINUS,LIT,65,PLUS
DIG1:   dq DUPF,DUPF,LIT,65,ONEMINUS,GTF
        dq ZBRANCH,DIG2
        dq LIT,65,MINUS,LIT,57,PLUS,ONEPLUS
        dq BRANCH,DIG3
DIG2:   dq DUPF,LIT,57,GTF
        dq ZBRANCH,DIG4
        dq DROP,LIT,0
DIG4:
DIG3:   dq LIT,48,MINUS,DUPF,FROMR,LTF
        dq ZBRANCH,DIG5
        dq DUPF,ONEPLUS,ZGT
        dq ZBRANCH,DIG7
        dq NIP,LIT,-1,BRANCH,DIG8
DIG7:   dq DROP,LIT,0
DIG8:
        dq BRANCH,DIG6
DIG5:   dq DROP,LIT,0
DIG6:   dq EXIT

defword ">NUMBER",7,0,TONUMBER
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
        dq TOR
TNUB:   dq RFETCH,ZGT
        dq ZBRANCH,TNU1
        dq DUPF,FETCHBYTE,BASE,FETCH,TODIGIT
        dq ZBRANCH,TNU3
        dq LIT,-1,BRANCH,TNU2
TNU3:   dq DROP,LIT,0,BRANCH,TNU2
TNU1:   dq LIT,0
TNU2:   dq ZBRANCH,TNUR
        dq SWAP,TOR,SWAP,BASE,FETCH
        dq UMMUL,DROP,ROT,BASE,FETCH
        dq UMMUL,DPLUS,FROMR,ONEPLUS
        dq FROMR,ONEMINUS,TOR
        dq BRANCH,TNUB
TNUR:   dq FROMR,EXIT

defword "ISFLOAT?",8,0,ISFLOATQ
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
        dq LIT,0,TOR
ISFB:   dq QDUP,ZBRANCH,ISFE
        dq OVER,FETCHBYTE,DUPF,TOR,LIT,-33,ANDF
        dq LIT,69,EQUAL
        dq RFETCH,DIGITQ,SWAP,DROP,ORF
        dq RFETCH,LIT,43,EQUAL,ORF
        dq RFETCH,LIT,45,EQUAL,ORF
        dq RFETCH,LIT,46,EQUAL,ORF
        dq NOTF,ZBRANCH,ISF1
        dq TWODROP,LIT,0,RDROP,RDROP,EXIT
ISF1:   dq FROMR,LIT,69,EQUAL,FROMR,ORF,TOR
        dq SWAP,ONEPLUS,SWAP,ONEMINUS
        dq BRANCH,ISFB
ISFE:   dq DROP,FROMR
        dq EXIT

defword "ISHEX?",6,0,ISHEXQ
; ( a u -- f )
; 2 > 
; IF ( a )
;   DUP C@ [CHAR] 0 = 
;   SWAP 1+ C@ [CHAR] x =
;   AND EXIT
; THEN DROP 0 ;
        dq LIT,2,GTF,ZBRANCH,ISH1
        dq DUPF,FETCHBYTE,LIT,48,EQUAL
        dq SWAP,ONEPLUS,FETCHBYTE,LIT,88,EQUAL
        dq ANDF,EXIT
ISH1:   dq DROP,LIT,0
        dq EXIT

defword "skip-digits",11,F_HIDDEN,skipDigs
; ( a u -- a u )
; BEGIN
;       OVER C@ DIGIT? SWAP DROP
; WHILE
;       SWAP 1+ SWAP 1-
; REPEAT
skipb:  dq OVER,FETCHBYTE,DIGITQ,SWAP,DROP
        dq ZBRANCH,skipe
        dq SWAP,ONEPLUS,SWAP,ONEMINUS
        dq BRANCH,skipb
skipe:  dq EXIT

defword "countDec",8,F_HIDDEN,cntDec
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
        dq skipDigs
        dq OVER,FETCHBYTE,LIT,46,EQUAL
        dq ZBRANCH,cdec1
        dq SWAP,ONEPLUS,SWAP,ONEMINUS
        dq LIT,0,TOR
cdecb:  dq OVER,FETCHBYTE,LIT,-33,ANDF,LIT,69,EQUAL,NOTF
        dq ZBRANCH,cdece
        dq FROMR,ONEPLUS,TOR
        dq SWAP,ONEPLUS,SWAP,ONEMINUS
        dq BRANCH,cdecb
cdece:  dq TWODROP,FROMR
        dq EXIT
cdec1:  dq TWODROP,LIT,0
        dq EXIT
        
; twop32:       .word 1106247680
; twop52:       .word 1127219200
; minus1:       .word 49136
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
        pop rax
        cvtsi2sd xmm0,rax
        ; sub DS, 8
        ; movsd [DS], xmm0
        FPUSH xmm0
        NEXT

defcode "D>F",3,0,DTOF
    ;cvtsi2sd xmm0, [DS]
    ; cvtsi2sd xmm1, 8[DS]
    pop rax ; hi
    pop rbx ; lo
    mov r8, rbx
    mov r9, rax
    
    xorpd xmm0, xmm0
    xorpd xmm1, xmm1
    xorpd xmm5, xmm5
    mov rcx, 0x00000000FFFFFFFF
    and r9, rcx
    mov rdx, [twop52]
    or rdx, r9
    push rdx
    movsd xmm3, [DS]
    pop rdx
    movsd xmm2, [twop52]
    subsd xmm3, xmm2
    addsd xmm5, xmm3
    
    mov rcx, 32
    shr rax, cl
    mov rdx, [twop52]
    or rdx, rax
    push rdx
    movsd xmm3, [DS]
    pop rdx
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
    push rdx
    movsd xmm3, [DS]
    pop rdx
    movsd xmm2, [twop52]
    subsd xmm3, xmm2
    addsd xmm0, xmm3
    
    mov rcx, 32
    shr rbx, cl
    mov rdx, [twop52]
    or rdx, rbx
    push rdx
    movsd xmm3, [DS]
    pop rdx
    subsd xmm3, xmm2
    addsd xmm1, xmm3
    movsd xmm2, [twop32]
    mulsd xmm1, xmm2
    addsd xmm0, xmm1
    
    addsd xmm0, xmm5
    FPUSH xmm0
    NEXT

; defcode "D>F",3,,DTOF
; ; ( d -- r )
        ; pop {r0}      ; hi
        ; pop {r1}      ; lo
        ; vmov s0,r0
        ; vcvt.f64.s32 d0,s0 ; d0 = hi = (a>>32)
        ; ldr r2,=twop32
        ; ldr r2, [r2]
        ; mov r3, #0
        ; vmov d1, r3, r2
        ; vmul.f64 d0, d0, d1 ; d0 = high = (a>>32)*twop32
        ; ldr r2,=twop52
        ; ldr r2,[r2]
        ; vmov d1, r3, r2   ; d1 = twop52
        ; eor r3, r3, r1
        ; vmov d2, r3, r2   ; d2 = low.d = twop52 | lo
        ; vsub.f64 d3, d0, d1
        ; vadd.f64 d1, d3, d2 ; d1 = (high - twop520 + low.d
        ; fpush d1
        ; NEXT

; .global gcvt
; .global d2fixed_buffered
; .global d2exp_buffered
; .global d2s_buffered
; defcode "(FE.)",5,,PARENFEDOT
; ; ( r -- )
        ; fpop d0
        ; pop {r1}
        ; mov r0,#8
        ; bl d2exp_buffered
        ; NEXT

defcode "(F.)",4,0,PARENFDOT
; ( r -- )
        ; fpop d0
        ; pop {r0}
        ; bl d2s_buffered
        ; NEXT
    
    FPOP    xmm0
    pop     r8
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

; defword "FE.",3,,FEDOT
        ; dq PAD,LIT,60,PLUS
        ; dq PARENFEDOT
        ; dq PAD,LIT,60,PLUS
        ; dq DUP,STRLEN,TYPE
        ; dq SPACE
        ; dq EXIT

defword "F.",2,0,FDOT
        dq PAD,LIT,60,PLUS
        dq PARENFDOT
        dq PAD,LIT,60,PLUS
        dq DUPF,STRLEN,TYPEF
        dq SPACE
        dq EXIT
    


defword "F.S",3,0,FDOTS
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
        
        dq LIT,60,EMIT
        dq FDEPTH,UDOT
        dq LIT,8,EMIT
        dq LIT,62,EMIT,SPACE
        dq F0,FETCH
FDSB:   dq DUPF,FSPFETCH
        dq GTF,ZBRANCH,FDSE
        dq DUPF,LIT,8,MINUS,FFETCH,FDOT
        dq LIT,8,MINUS
        dq BRANCH,FDSB
FDSE:   dq DROP,EXIT
        ; dq LIT,0
; FSBEG:  dq DUPF,FDEPTH
        ; dq LTF,ZBRANCH,FSEND
        ; dq DUPF,FLOATS,FSPFETCH
        ; dq PLUS
        ; dq FFETCH,FDOT
        ; dq LIT,1,PLUS
        ; dq BRANCH,FSBEG
; FSEND:  dq DROP,EXIT

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

defword ">FLOAT",6,0,TOFLOAT
        dq OVER,FETCHBYTE,LIT,45,EQUAL
        dq ZBRANCH,TFE
        dq ONEMINUS,SWAP,ONEPLUS,SWAP
        dq LIT,-1,TOR
        dq BRANCH,TFT
TFE:    dq LIT,1,TOR
TFT:    dq OVER,FETCHBYTE,LIT,43,EQUAL
        dq ZBRANCH,TF2
        dq ONEMINUS,SWAP,ONEPLUS,SWAP
TF2:    dq LIT,0,DUPF,TWOSWAP,TONUMBER
        dq TWOSWAP,TWOSWAP,OVER
        dq FETCHBYTE,LIT,46,EQUAL
        dq ZBRANCH,TFE3
        dq ONEMINUS,SWAP,ONEPLUS,SWAP
        dq DUPF,TOR,TONUMBER,DUPF,FROMR
        dq MINUS,TOR
        dq BRANCH,TF3
TFE3:   dq LIT,0,TOR
TF3:    dq TWOSWAP,DTOF,OVER,FETCHBYTE
        dq LIT,32,ORF,LIT,101,EQUAL
        dq ZBRANCH,TF4
        dq ONEMINUS,SWAP,ONEPLUS,SWAP
        dq OVER,FETCHBYTE,LIT,45,EQUAL
        dq ZBRANCH,TFE5
        dq ONEMINUS,SWAP,ONEPLUS,SWAP
        dq LIT,-1,TOR
        dq BRANCH,TF5
TFE5:   dq LIT,1,TOR
TF5:    dq OVER,FETCHBYTE,LIT,43,EQUAL
        dq ZBRANCH,TF6
        dq ONEMINUS,SWAP,ONEPLUS,SWAP
TF6:    dq LIT,0,DUPF,TWOSWAP,TONUMBER,TWOSWAP
        dq DROP,FROMR,MULF,FROMR,PLUS,TOR
TF4:    dq FROMR,DUPF,ZLT
        dq ZBRANCH,TFE7
        dq NEGATE,LIT,-1,TOR
        dq BRANCH,TF7
TFE7:   dq LIT,1,TOR
TF7:    dq LIT,1,STOF,LIT,0,SWAP
TFB:    dq DUPF,LIT,0,NEQUAL
        dq ZBRANCH,TFEND
        dq DUPF,LIT,1,ANDF
        dq ZBRANCH,TF8
        dq OVER,FLOATS,POWERSOFTEN
        dq PLUS,FFETCH,FMULF
TF8:    dq LIT,1,RSHIFT
        dq SWAP,ONEPLUS,SWAP
        dq BRANCH,TFB
TFEND:  dq TWODROP,FROMR,ZLT
        dq ZBRANCH,TFE9
        dq FDIVF
        dq BRANCH,TF9
TFE9:   dq FMULF
TF9:    dq FROMR,ZLT
        dq ZBRANCH,TF10
        dq FNEGATE
TF10:   dq TWODROP
        dq EXIT

defword "CONVERT",7,0,CONVERT
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
        dq TWODUP,ISFLOATQ,ZBRANCH,CON10
        dq TOFLOAT,FLITERAL,LIT,-1,EXIT
CON10:  dq BASE,FETCH,TOR
        dq TWODUP,TOR,TOR,OVER,FETCHBYTE,LIT,45,EQUAL
        dq ZBRANCH,CON1
        dq SWAP,ONEPLUS,SWAP,ONEMINUS,LIT,-1
        dq BRANCH,CON2
CON1:   dq LIT,1
CON2:   dq MROT,TWODUP,ISHEXQ,ZBRANCH,CON7
        dq SWAP,LIT,2,PLUS,SWAP,LIT,2,MINUS,HEX
CON7:   dq LIT,0,DUPF,TWOSWAP,TONUMBER,QDUP
        dq ZEQ,ZBRANCH,CON3
        dq DROP,DTOS,SWAP,MULF,LITERAL,LIT,-1,RDROP,RDROP
        dq FROMR,BASE,FSTORE
        dq BRANCH,CON4
CON3:   dq OVER,FETCHBYTE,LIT,46,EQUAL
        dq ZBRANCH,CON5
        dq SWAP,ONEPLUS,SWAP,ONEMINUS
        dq TONUMBER,TWODROP,ROT,ZLT,RDROP,RDROP
        dq FROMR,BASE,FSTORE
        dq ZBRANCH,CON6
        dq DNEGATE
CON6:   dq TWOLITERAL,LIT,-1,BRANCH,CON4
CON5:   dq TWODROP,TWODROP,DROP
        dq ABORT,FROMR,FROMR,LIT,0
        dq FROMR,BASE,FSTORE
CON4:   dq EXIT

defword "@EXECUTE",8,0,ATEXECUTE
; ( a -- )
; ; ?DUP IF EXECUTE THEN ;
        dq FETCH,QDUP,ZBRANCH,ATEX1
        dq EXECUTE
ATEX1:  dq EXIT

defword "KEY?",4,0,QKEY
; ( -- c T | F )
; '?KEY ;EXECUTE ;
        dq TICKQKEY,ATEXECUTE
        dq EXIT

defword "KEY",3,0,KEY
; ( -- c)
; BEGIN ?KEY UNTIL ;
KBEG:   dq QKEY
        dq ZBRANCH,KBEG
        dq EXIT

defword "EMIT",4,0,EMIT
; ( c -- )
; 'EMIT ; ;EXECUTE
        dq TICKEMIT,ATEXECUTE
        dq EXIT

defword "PACE",4,0,PACE
; ( -- )
; 11 EMIT
        dq LIT,11,EMIT
        dq EXIT

defword "SPACE",5,0,SPACE
; ( -- ) 
; BL EMIT
        dq BLF,EMIT
        dq EXIT

defword "SPACES",6,0,SPACES
; ( n -- )
; BEGIN DUP WHILE SPACE 1- REPEAT DROP ;
SPBEG:  dq DUPF
        dq ZBRANCH,SPEND
        dq SPACE,LIT,1,MINUS
        dq BRANCH,SPBEG
SPEND:  dq DROP,EXIT

defword "TYPE",4,0,TYPEF
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
        dq QDUP,ZBRANCH,TYP1
TYPBEG: dq DUPF
        dq ZBRANCH,TYPEND
        dq OVER,FETCHBYTE,EMIT
        dq SWAP,LIT,1,PLUS
        dq SWAP,LIT,1,MINUS
        dq BRANCH,TYPBEG
TYPEND: dq TWODROP,EXIT
TYP1:   dq DROP,EXIT

defword "SLITERAL",8,F_IMMED,SLITERAL 
; ( c u -- )
; STATE @ IF
;   [COMPILE] DO$ DUP DUP >R ,  
;   HERE SWAP CMOVE
;   HERE R> + ALIGNED DP !
; THEN ; IMMEDIATE
        dq STATE,FETCH,ZBRANCH,SLI1
        dq LIT,DOSTRING,COMMA
        dq DUPF,DUPF,TOR,COMMA
        dq HERE,SWAP,CMOVEF
        dq HERE,FROMR,PLUS,ALIGNED,FDP,FSTORE
SLI1:   dq EXIT

defword 'S"',2,F_IMMED,SQUOTE
; ( -- )
; STATE ; 
;   IF
;       34 PARSE SLITERAL
;   ELSE
;       34 PARSE PAD SWAP DUP >R CMOVE PAD R>
;   THEN ;
        dq STATE,FETCH,ZBRANCH,SQ1
        dq LIT,34,PARSE
        dq SLITERAL
        dq BRANCH,SQ2
SQ1:    dq LIT,34,PARSE,PAD,LIT,20,PLUS,SWAP,DUPF
        dq TOR,CMOVEF
        dq PAD,LIT,20,PLUS,FROMR
SQ2:    dq EXIT

defword 'C"',2,F_IMMED,CQUOTE
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
        dq STATE,FETCH,ZBRANCH,CQ1
        dq LIT,34,PARSE
        dq LIT,DOPSTRING,COMMA
        dq DUPF,DUPF,TOR,COMMABYTE
        dq HERE,SWAP,CMOVEF
        dq HERE,FROMR,PLUS,ALIGNED,FDP,FSTORE
        dq BRANCH,CQ2
CQ1:    dq LIT,34,PARSE,PAD,TWODUP
        dq STOREBYTE,ONEPLUS,SWAP,CMOVEF,PAD
CQ2:    dq EXIT


defword '."',2,F_IMMED,DOTQUOTE
; ( -- )
; POSTPONE S" POSTPONE TYPE ; IMMEDIATE
; [COMPILE] S" ['] TYPE , ; IMMEDIATE
        dq STATE,FETCH,ZBRANCH,DQ1
        dq SQUOTE
        dq LIT,TYPEF,COMMA
        dq BRANCH,DQ2
DQ1:    dq LIT,34,PARSE,TYPEF
DQ2:    dq EXIT

defword "^H",2,0,BKSP
; ( b b b -- b b b )
; >R OVER R> SWAP OVER XOR
; IF 8 'ECHO ;EXECUTE
;    32 'ECHO ;EXECUTE
;    8 'ECHO ;EXECUTE
; THEN ;
        dq TOR,OVER,FROMR,SWAP,OVER
        dq XORF,ZBRANCH,BKSP1
        dq LIT,8,TICKECHO,ATEXECUTE
;        dq LIT,127,TICKECHO,ATEXECUTE
        dq LIT,1,MINUS
        dq LIT,32,TICKECHO,ATEXECUTE
        dq LIT,8,TICKECHO,ATEXECUTE
BKSP1:  dq EXIT

defword "TAP",3,0,TAP
; ( bot eot cur c -- bot eot cur )
; DUP 'ECHO @EXECUTE OVER C! 1 + ;
        dq DUPF,TICKECHO,ATEXECUTE,OVER
        dq STOREBYTE,LIT,1,PLUS
        dq EXIT

defword "KTAP",4,0,KTAP
; ( bot eot cur c -- bot eot cur )
; DUP 13 XOR
; IF 8 XOR IF BL TAP ELSE ^H THEN EXIT
; THEN DROP SWAP DROP DUP ;
        dq DUPF,LIT,9,EQUAL
        dq ZBRANCH,kT3 ; if it's a tab then replace with space
        dq DROP,LIT,2,SPACES,BLF,TAP,EXIT
kT3:    dq DUPF,LIT,13,XORF
        dq ZBRANCH,kT1
        dq DUPF,LIT,8,XORF,SWAP
        dq LIT,127,XORF,ANDF,ZBRANCH,kT2
        dq BLF,TAP,EXIT
kT2:    dq BKSP,EXIT
kT1:    dq DROP,SWAP,DROP,DUPF
        dq EXIT

defword "ACCEPT",6,0,ACCEPT
; ( b u -- u )
; OVER + OVER
; BEGIN 2DUP XOR
; WHILE KEY DUP BL - 95 U<
;   IF TAP ELSE 'TAP @EXECUTE THEN
; REPEAT DROP OVER - ;
        dq OVER,PLUS,OVER
ACBEG:  dq TWODUP,XORF,ZBRANCH,ACEND
        dq KEY,DUPF
        dq BLF,LIT,127,WITHIN
        dq ZBRANCH,AC1
        dq TAP,BRANCH,AC2
AC1:    dq TICKTAP,ATEXECUTE
AC2:    dq BRANCH,ACBEG
ACEND:  dq DROP,OVER,MINUS
        dq SWAP,DROP
        dq EXIT

defword "EXPECT",6,0,EXPECT
; ( b u -- )
; 'EXPECT ;EXECUTE SPAN ! DROP ;
        dq TICKEXPECT,ATEXECUTE,SPAN
        dq FSTORE,EXIT

defword "QUERY",5,0,QUERY
; ( a u -- )
; TIB @ 130 'EXPECT @EXECUTE #TIB ! 0 >IN ! ;
      dq TIB,FETCH,LIT,130
      dq TICKEXPECT,ATEXECUTE
      dq NTIB,FSTORE
      dq LIT,0,TOIN,FSTORE
      dq EXIT

defword "REFILL",6,0,REFILL
; ( -- f )
; QUERY -1
        ;dq TIB,FETCH,LIT,80
    dq SOURCEID,FETCH,ZEQ,ZBRANCH,QU1
    dq QUERY,LIT,-1
QU1:
    dq SOURCEID,FETCH,ZGT,ZBRANCH,QU2
    dq TIB,FETCH,LIT,130,SOURCEID,FETCH
    dq READLINE
    dq THROW,SWAP,NTIB,FSTORE
    dq SOURCE,INTERPRET
QU2:
    dq EXIT

defword "SOURCE",6,0,SOURCE
; ( -- a u )
; TIB ; #TIB ; ;
        dq TIB,FETCH,NTIB,FETCH,EXIT
        ;dq TIB,NTIB,FETCH,EXIT

defword "CR",2,0,CR
; ( -- )
; 13 EMIT 10 EMIT ;
        dq LIT,13,EMIT,LIT,10,EMIT
        dq EXIT

defword "same?",5,F_HIDDEN,SAMEQ
; ( a a u -- f )
; swap >r
; begin dup
; while char- 2dup + c; over r; + c; xor
; until r> drop 2drop 0 exit ( no match )
; then r> drop 2drop -1 ; ( found )
        dq SWAP,TOR
SABEG:  dq DUPF,ZBRANCH,SATHE
        dq LIT,1,MINUS,TWODUP
        dq PLUS,FETCHBYTE,OVER
        dq RFETCH,PLUS,FETCHBYTE
        dq XORF,ZBRANCH,SABEG
SAUNT:  dq FROMR,DROP,TWODROP
        dq LIT,0,EXIT
SATHE:  dq FROMR,DROP,TWODROP
        dq LIT,-1,EXIT

defword "(FIND)",6,0,PARENSFIND
; ( voc a -- xt t | a f )
        dq COUNT,TOUPPER,SWAP,TOR,TOR
        dq FETCH,FETCH
FIBEG:  dq QDUP,ZBRANCH,FIEND                  ; keep searching until latest=0
        dq DUPF,LIT,8,PLUS,FETCHBYTE
        dq LIT,F_HIDDEN,LIT,F_LENMASK,ORF,ANDF   ; ( L n | a u )
        ;dq LIT,F_LENMASK,AND   ; ( L n | a u )
        dq DUPF,RFETCH,EQUAL,ZBRANCH,FI1
        ; ( L n | a u )
        dq OVER,LIT,9,PLUS,SWAP                ; ( L L+5 n | a u )
        ; ( now compare characters )
        dq FROMR,FROMR,TWODUP,TOR,TOR,SWAP,DROP  ; ( L L+5 n a | a u )
        dq SWAP                                  ; ( L L+5 a n | a u )
        dq SAMEQ                            ; ( L t/f | a u )
        dq ZBRANCH,FI3
        dq RDROP,RDROP                      ; ( L )
        dq DUPF                             ; ( L L )
        dq TCFA                             ; ( L cfa )
        dq SWAP,IMMEDQ,ZBRANCH,FI5
        dq LIT,1,BRANCH,FI4
FI5:    dq LIT,-1
FI4:    dq EXIT
FI3:    dq BRANCH,FI2  ; ( exit if )
FI1:    dq DROP                ; ( L n | a u : else branch - not equal length ) 
FI2:    
        dq FETCH,BRANCH,FIBEG
FIEND:  dq RDROP,FROMR
        dq LIT,1,MINUS       ; get back to c-addr
        dq LIT,0
        dq EXIT

defword "FIND",4,0,FIND
        dq TOR,CONTEXT
FFBEG:  dq DUPF,FETCH,ZBRANCH,FFEND
        dq DUPF
        dq RFETCH,PARENSFIND
        dq DUPF,ZBRANCH,FFF1
        dq ROT,DROP,RDROP,EXIT
FFF1:   dq TWODROP
        dq CELLPLUS,BRANCH,FFBEG
FFEND:  dq DROP,FROMR,LIT,0
        dq EXIT

defword ">CFA",4,0,TCFA
; ( a -- xt )
; DUP CELL + C@ F_LENMASK AND + CELL 1 + + ALIGNED ;
        dq DUPF,CELL,PLUS,FETCHBYTE
        dq LIT,F_LENMASK,ANDF
        dq CELL,LIT,1,PLUS,PLUS
        dq PLUS
        dq ALIGNED
        dq EXIT

defword ">DFA",4,0,TDFA
; ( a -- xt )
; >CFA CELL + ;
        dq TCFA,CELL,PLUS,EXIT

defword "CFA>",4,0,CFATO
; ( xt -- link )
; LATEST @ BEGIN
; ?DUP WHILE 2DUP SWAP < IF NIP EXIT THEN
; @ REPEAT DROP 0 ;
       ; dq LATEST,FETCH
        dq CURRENT,FETCH,FETCH
CFTB:   dq QDUP,ZBRANCH,CFT1
        dq TWODUP,SWAP,LTF,ZBRANCH,CFT2
        dq NIP,EXIT
CFT2:   dq FETCH,BRANCH,CFTB
CFT1:   dq DROP,LIT,0
        dq EXIT

defword ">BODY",5,0,TOBODY
; ( xt -- dfa )
; undefined if used on a word not created wth CREATE
; CELL+ ;
        dq LIT,2,CELLS,PLUS
        dq EXIT

defword "FORGET",6,,FORGET
; ( -- )
; BL WORD FIND IF
; CFA> DUP @ LATEST !
; DP ! 
; THEN ;

; FIX to go back to link field from cfa, i.e. xt
        dq BLF,WORDF,FIND
        dq ZBRANCH,FORG1
        dq CFATO,DUPF,FETCH;,DUPF
        ; dq LATEST,FSTORE
        dq CURRENT,FETCH,FSTORE
        dq FDP,FSTORE
FORG1:  dq EXIT

defword "HEADER,",7,0,HEADERCOMMA
; ( a u -- )
; TOUPPER LATEST @              ( get last word address )
; HERE LATEST !        ( store this word in the LATEST variable )
; ,                    ( store last word in link field )
; DUP DUP >R C,        ( a u  - store word length, save a copy on RS )
; HERE SWAP CMOVE      ( copy name to dictionary )
; HERE R> + ALIGNED DP !    ( add name length and align for padding )
        dq TOUPPER,CURRENT,FETCH,FETCH;LATEST,FETCH
       ; dq HERE,LATEST,FSTORE
        dq HERE,CURRENT,FETCH,FSTORE
        dq COMMA,DUPF,DUPF,TOR,COMMABYTE
        dq HERE,SWAP,CMOVEF
       ; align
        dq HERE,FROMR,PLUS,ALIGNED,FDP,FSTORE 
        dq EXIT

defword "TOUPPER",7,0,TOUPPER
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
        dq TWODUP
TUPB:   dq DUPF,ZBRANCH,TUPE
        dq OVER,FETCHBYTE,DUPF,DUPF,LIT,123
        dq LTF,SWAP,LIT,97,GTE,ANDF
        dq ZBRANCH,TUP1
        dq LIT,32,XORF
TUP1:   dq LIT,2,PICK,STOREBYTE
        dq LIT,1,MINUS,SWAP,LIT,1,PLUS,SWAP
        dq BRANCH,TUPB
TUPE:   dq TWODROP
        dq EXIT

defword "CREATE",6,0,CREATE
; ( -- )
; PARSE-NAME HEADER, COMPILE DOVAR 0 , ;
; Note that no EXIT is compiled
        dq PARSENAME,HEADERCOMMA
        dq LIT,DOVAR,COMMA
        dq LIT,0,COMMA
        dq EXIT

defword "DOES>",5,0,DOES
; ( -- )
; ['] DODOES LITERAL LATEST @ >CFA !
; R> LATEST @ >CFA >BODY ! ;
        ; dq LIT,DODOES,LATEST,FETCH,TCFA
        dq LIT,DODOES,CURRENT,FETCH,FETCH,TCFA
        dq FSTORE
        ; dq FROMR,LATEST,FETCH,TCFA,TOBODY
        dq FROMR,CURRENT,FETCH,FETCH,TDFA
        dq FSTORE
        dq EXIT

defword "VALUE",5,0,VALUE
; ( n -- )
; CREATE , DOES> @ ;   -- my old way!
; PARSE-NAME HEADER, COMPILE DOVAL , ;
        ;dq CREATE,COMMA,DOES,FETCH
        dq PARSENAME,HEADERCOMMA
        dq LIT,DOVAL,COMMA
        ;dq LIT,0,COMMA
        dq COMMA
        dq EXIT

defword "2VALUE",6,0,TWOVALUE
; ( n n -- )
; CREATE swap , , DOES> dup ; swap cell+ ; ; -- old way
        ;dq CREATE,SWAP,COMMA,COMMA,DOES
        ;dq DUPF,FETCH,SWAP,CELLPLUS,FETCH
        dq PARSENAME,HEADERCOMMA
        dq LIT,DO2VAL,COMMA
        ;dq LIT,0,COMMA
        dq COMMA,COMMA
        dq EXIT

defword "TO",2,F_IMMED,TOO
; ( x -- )
; ' >DFA STATE @
; IF
;   LITERAL [COMPILE] !
; ELSE
;   !
; THEN ;
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
TOX:    dq EXIT

defword "+TO",3,F_IMMED,PLUSTO
; ( x -- )
; ' >DFA STATE @
; IF
;   LITERAL [COMPILE] +!
; ELSE
;   +!
; THEN ;
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
PTOX:   dq EXIT

defword "VARIABLE",8,0,VARIABLE
; ( -- )
; CREATE 0 , ;
        dq CREATE,LIT,0,COMMA
        dq EXIT

defword "FVARIABLE",9,0,FVARIABLE
; ( -- )
; CREATE 0 , ;   -- floats are the same size as ints
        dq VARIABLE
        dq EXIT
    
defword "CONSTANT",8,0,CONSTANT
; ( n -- )
; CREATE , DOES> @ ; -- the old way
; VALUE
       ; dq CREATE,COMMA,DOES,FETCH
        dq PARSENAME,HEADERCOMMA
        dq LIT,DOCONST,COMMA,COMMA
        dq EXIT

defword "FCONSTANT",9,0,FCONSTANT
; ( n -- )
; CREATE , DOES> @ ;
        dq CONSTANT
        dq EXIT
    
defword "2VARIABLE",9,0,TWOVARIABLE
; ( -- )
; CREATE 0 ! 0 ! ;
        dq CREATE,LIT,0,COMMA,LIT,0,COMMA
        dq EXIT

defword "2CONSTANT",9,0,TWOCONSTANT
; ( d -- )
; CREATE SWAP , , DOES> DUP @ SWAP CELL+ @ ;
        dq CREATE,SWAP,COMMA,COMMA,DOES,DUPF,FETCH
        dq SWAP,CELLPLUS,FETCH
        dq EXIT

defword ",",1,0,COMMA
; ( n -- )
; ; HERE DUP CELL+ DP ! ! ;
; HERE ! 1 CELLS ALLOT ;
        dq HERE,DUPF,CELLPLUS
        dq FDP,FSTORE,FSTORE
       ; dq HERE,FSTORE,LIT,1,CELLS
       ; dq ALLOT
        dq EXIT

defword "[",1,F_IMMED,LBRAC
; ( -- )
;  0 STATE ! ;
        dq LIT,0,STATE,FSTORE
        dq EXIT

defword "]",1,0,RBRAC
; ( -- ) 
; 1 STATE ! ;
        dq LIT,1,STATE,FSTORE
        dq EXIT

defword "HIDDEN",6,0,HIDDEN
; ( a -- )
;  8 + DUP @ F_HIDDEN XOR SWAP !
        dq LIT,8,PLUS
        dq DUPF,FETCH
        ;dq LIT,F_HIDDEN,INVERT
        ;dq ANDF,SWAP,FSTORE
        dq LIT,F_HIDDEN
        dq XORF,SWAP,FSTORE
        dq EXIT

defword "UNHIDE",6,0,UNHIDE
; ( a -- )
;  8 + DUP @ F_HIDDEN XOR SWAP !
        dq LIT,8,PLUS
        dq DUPF,FETCH
        dq LIT,F_HIDDEN,INVERT
        dq ANDF,SWAP,FSTORE
        dq EXIT

defword ":",1,0,COLON
; ( -- )
; BL WORD COUNT HEADER,
; DOCOL , LATEST @ HIDDEN ] ;
        dq BLF,WORDF,COUNT
        dq HEADERCOMMA,LIT,DOCOL
        ;dq COMMA,LATEST,FETCH
        dq COMMA,CURRENT,FETCH,FETCH
        dq HIDDEN,RBRAC,EXIT

defword ";",1,F_IMMED,SEMICOLON
; ( -- )
; EXIT , LATEST @ HIDDEN ] ;
        dq STATE,FETCH,ZBRANCH,COLX
        dq LOCFLG,FETCH,ZBRANCH,COL1
        dq LIT,0,HERE
COLB:   dq CELLMINUS,DUPF,FETCH,LIT,DOCOL,NEQUAL,ZBRANCH,COLE
        dq SWAP,ONEPLUS,SWAP
        dq DUPF,FETCH,LIT,PARENSDO,EQUAL,ZBRANCH,COL2
        dq CELL,OVER,LIT,2,CELLS,PLUS,PLUSSTORE
COL2:   dq DUPF,FETCH,LIT,PARENSLOOP,EQUAL,ZBRANCH,COL3
        dq CELL,OVER,LIT,2,CELLS,PLUS,PLUSSTORE
COL3:   dq DUPF,FETCH,LIT,PARENSQDO,EQUAL,ZBRANCH,COL4
        dq CELL,OVER,LIT,2,CELLS,PLUS,PLUSSTORE
COL4:   dq DUPF,FETCH,LIT,PARENSPLUSLOOP,EQUAL,ZBRANCH,COL5
        dq CELL,OVER,LIT,2,CELLS,PLUS,PLUSSTORE
COL5:   dq DUPF,FETCH,LIT,BRANCH,EQUAL,ZBRANCH,COL6
        dq CELL,OVER,LIT,2,CELLS,PLUS,PLUSSTORE
COL6:   dq DUPF,FETCH,LIT,ZBRANCH,EQUAL,ZBRANCH,COL7
        dq CELL,OVER,LIT,2,CELLS,PLUS,PLUSSTORE
COL7:   dq DUPF,FETCH,OVER,CELLPLUS,FSTORE
        dq BRANCH,COLB
COLE:   dq CELLPLUS,LIT,LOCINIT,SWAP,FSTORE
        dq ZBRANCH,COL1,CELL,FDP,PLUSSTORE
COL1:   dq RESETLOCALS
        dq LIT,EXIT,COMMA
        ;dq LATEST,FETCH,HIDDEN
        dq CURRENT,FETCH,FETCH,UNHIDE;HIDDEN
        dq LBRAC
COLX:   dq EXIT

defword "EXIT",4,F_IMMED,LOCEXIT
        dq LOCFLG,FETCH,ZBRANCH,EXIT1
        dq COMPILE,LOCUNWIND
EXIT1:  dq COMPILE,EXIT
        dq COMPILE,EXIT
        dq EXIT

defword "'",1,0,TICK
; ( -- xt )
; BL WORD FIND 0= IF BYE THEN ;
        dq BLF,WORDF,FIND
        dq LIT,0,EQUAL
        dq ZBRANCH,TIC1
        dq COUNT,TYPEF,SPACE
        dq DOSTRING,9
        db 'NOT FOUND',0,0,0,0,0,0,0
        dq TYPEF
TIC1:   dq EXIT

defword "[']",3,F_IMMED,BRACTICK
; ( -- )
; ' LIT LIT , , ;
        dq TICK,LIT,LIT,COMMA,COMMA
        dq EXIT

defword "COMPILE",7,0,COMPILE
; ( -- )
; R> DUP CELL+ >R @ , ;
        dq FROMR,DUPF,CELLPLUS
        dq TOR,FETCH,COMMA
        dq EXIT

defword "[COMPILE]",9,F_IMMED,BRACCOMPILE
; ( -- )
; ' , ;
        dq TICK,COMMA
        dq EXIT

defword "COMPILE,",8,0,COMPILECOMMA
; ( -- )
; , ;
        dq COMMA
        dq EXIT

defword "RECURSE",7,F_IMMED,RECURSE
; ( -- )
; LATEST @ >CFA , ;
        ;dq LATEST,FETCH,ZBRANCH,RECUR1
        ;dq LATEST,FETCH,TCFA,COMMA
;        ;dq CR,LATEST,FETCH,TCFA,DOT
;        dq BRANCH,RECUR2
        dq LOCFLG,FETCH,ZBRANCH,RECUR1
        dq PREVIOUS,DEFINITIONS
RECUR1: dq CURRENT,FETCH,FETCH,TCFA,COMMA
        ;dq CR,CURRENT,FETCH,FETCH,TCFA,DOT
        dq LOCFLG,FETCH,ZBRANCH,RECUR2
        dq ALSO,LOCVOC,DEFINITIONS
RECUR2: dq EXIT

defword "RECURSIVE",9,F_IMMED,RECURSIVE
        dq LATEST,FETCH,ZBRANCH,RECUS1
        dq LATEST,FETCH,UNHIDE
        ;dq CR,LATEST,FETCH,DOT
        dq BRANCH,RECUS2
RECUS1: dq CURRENT,FETCH,FETCH,UNHIDE
        ;dq CR,CURRENT,FETCH,FETCH,DOT
RECUS2: dq EXIT

defword "WORDS",5,0,WORDS,rootlink
; ( -- )
        ;dq CR,LATEST,FETCH
        dq CR,CONTEXT,FETCH,FETCH
        dq CONSOLE,DROP,LIT,0,TWOTOR
WOBEG:  dq QDUP,ZBRANCH,WOEND
        dq DUPF,LIT,8,PLUS,FETCHBYTE
        dq LIT,F_HIDDEN,LIT,F_LENMASK,ORF,ANDF
        dq DUPF,LIT,33,LTF,ZBRANCH,WO1
        dq OVER,LIT,9,PLUS,SWAP
        dq DUPF,TWOFROMR,ROT,PLUS,ONEPLUS
        dq TWODUP,LTE,ZBRANCH,WO3
        dq CR,DROP,OVER
WO3:    dq TWOTOR,TYPEF,SPACE
        dq BRANCH,WO2
WO1:    dq DROP
WO2:    dq FETCH
        dq BRANCH,WOBEG
WOEND:  dq TWORDROP,CR,EXIT

defword "/STRING",7,0,SLASHSTRING
; ( a u n -- a u ) remove n chars from the start of string
; DUP >r - SWAP R> + SWAP ;
        dq DUPF,TOR,MINUS,SWAP
        dq FROMR,PLUS,SWAP
        dq EXIT

defword "PARSE-NAME",10,0,PARSENAME
; ( -- a u )
;  SOURCE >IN ; /STRING  ( caddr u )
;  DROP
;  0 \ counter ( caddr cntr )
;  BEGIN OVER C; BL = WHILE SWAP 1+ SWAP 1 >IN +! REPEAT
;  BEGIN OVER C; BL <> >IN ; #TIB ; < AND WHILE 1+ SWAP 1+ SWAP 1 >IN +! REPEAT
;  SWAP OVER - SWAP ;
        dq SOURCE,TOIN,FETCH,SLASHSTRING
        dq DROP,LIT,0
PNBEG1: dq OVER,FETCHBYTE,BLF,EQUAL
        dq ZBRANCH,PNEND1
        dq SWAP,LIT,1,PLUS,SWAP,LIT,1,TOIN,PLUSSTORE
        dq BRANCH,PNBEG1
PNEND1:
PNBEG2: dq OVER,FETCHBYTE,BLF,NEQUAL
        dq TOIN,FETCH,NTIB,FETCH,LTF,ANDF
        dq ZBRANCH,PNEND2
        dq LIT,1,PLUS,SWAP,LIT,1,PLUS,SWAP
        dq LIT,1,TOIN,PLUSSTORE
        dq BRANCH,PNBEG2
PNEND2: dq SWAP,OVER,MINUS,SWAP
        dq LIT,1,TOIN,FETCH,PLUS,NTIB,FETCH,MIN
        dq TOIN,FSTORE
        dq EXIT

defword "PARSE",5,0,PARSE
; ( c -- a u )
;  >R SOURCE >IN ; 1+ /STRING  ( caddr u )
;  DROP
;  0 \ counter ( caddr cntr )
;  \ BEGIN OVER C@ BL = WHILE SWAP 1+ SWAP 1 >IN +! REPEAT
;  BEGIN OVER C@ R@ <> >IN @ #TIB @ < AND WHILE 1+ SWAP 1+ SWAP 1 >IN +! REPEAT
;  SWAP OVER - SWAP RDROP
        dq TOR,SOURCE,TOIN,FETCH,SLASHSTRING
        dq DROP,LIT,0
PBEG2:  dq OVER,FETCHBYTE,RFETCH,NEQUAL
        dq TOIN,FETCH,NTIB,FETCH,LTF,ANDF
        dq ZBRANCH,PEND2
        dq LIT,1,PLUS,SWAP,LIT,1,PLUS,SWAP
        dq LIT,1,TOIN,PLUSSTORE
        dq BRANCH,PBEG2
PEND2:  dq SWAP,OVER,MINUS,SWAP
        dq RDROP
        dq LIT,1,TOIN,FETCH,PLUS,NTIB,FETCH,MIN
        dq TOIN,FSTORE
        dq EXIT

defword "CHAR",4,0,CHAR
; ( -- )
; BL WORD 1+ C; ;
        dq BLF,WORDF,LIT,1,PLUS,FETCHBYTE
        dq EXIT

defword "[CHAR]",6,F_IMMED,BRACCHAR
; ( -- )
; CHAR ['] LIT , , ;
        dq CHAR,LIT,LIT,COMMA,COMMA
        dq EXIT

defword "WORD",4,0,WORDF
; ( c -- a u )
;  >R SOURCE >IN ; /STRING  ( caddr u )
;  DROP
;  0 \ counter ( caddr cntr )
;  BEGIN OVER C; R; =  >IN ; #TIB ; 1- < AND WHILE SWAP 1+ SWAP 1 >IN +! REPEAT
;  BEGIN OVER C; R; <> >IN ; #TIB ; 1- < AND WHILE 1+ SWAP 1+ SWAP 1 >IN +! REPEAT
;  SWAP OVER - SWAP RDROP
;  1 >IN ; + #TIB ; MIN >IN !
;  DUP WBUF C! WBUF 1+ SWAP CMOVE ;
        dq TOR,SOURCE,TOIN,FETCH,SLASHSTRING
        dq DROP
WOR1:   dq LIT,0
WBEG1:  dq OVER,FETCHBYTE,RFETCH,EQUAL
        dq TOIN,FETCH,NTIB,FETCH,LTE,ANDF
        dq ZBRANCH,WEND1
        dq SWAP,LIT,1,PLUS,SWAP,LIT,1,TOIN,PLUSSTORE
        dq BRANCH,WBEG1
WEND1:
WBEG2:  dq OVER,FETCHBYTE,RFETCH,NEQUAL
        dq TOIN,FETCH,NTIB,FETCH,LTF,ANDF
        dq ZBRANCH,WEND2
        dq LIT,1,PLUS,SWAP,LIT,1,PLUS,SWAP
        dq LIT,1,TOIN,PLUSSTORE
        dq BRANCH,WBEG2
WEND2:  dq SWAP,OVER,MINUS,SWAP
        dq RDROP
        dq LIT,1,TOIN,FETCH,PLUS,NTIB,FETCH,MIN
        dq TOIN,FSTORE
        dq DUPF,WBUF,STOREBYTE
       ; dq LIT,666,DOT
        dq WBUF,LIT,1,PLUS,SWAP,CMOVEF
        dq WBUF
        dq EXIT

defword ".(",2,F_IMMED,DOTBRAC
; ( -- )
; 41 PARSE TYPE ; IMMEDIATE
                dq LIT,41,PARSE,TYPEF
                dq EXIT

defword "(",1,F_IMMED,BRAC
; ( -- )
; 41 PARSE 2DROP ; IMMEDIATE
                dq LIT,41,PARSE,TWODROP
                dq EXIT

defword "\",1,F_IMMED,COMMENTF
; ( -- )
; #TIB @ >IN ! ; IMMEDIATE
                dq NTIB,FETCH,TOIN,FSTORE
                dq EXIT

defword "IMMEDIATE?",10,0,IMMEDQ
; ( lfa -- f )
; 8 + C@ F_IMMED AND IF -1 ELSE 0 THEN ;
        dq LIT,8,PLUS,FETCHBYTE
        dq LIT,F_IMMED
        dq ANDF,ZBRANCH,IMM1
        dq LIT,-1,BRANCH,IMM2
IMM1:   dq LIT,0
IMM2:   dq EXIT

defword "IMMEDIATE",9,0,IMMEDIATE
               ; dq LATEST,FETCH,LIT,8,PLUS,DUPF
                dq CURRENT,FETCH,FETCH,LIT,8,PLUS,DUPF
                dq FETCHBYTE,LIT,F_IMMED,XORF
                dq SWAP,STOREBYTE
                dq EXIT

defword "LITERAL",7,F_IMMED,LITERAL
; ( x -- )
; STATE @ IF ['] LIT , , THEN ; IMMEDIATE
        dq STATE,FETCH,ZBRANCH,LIT1
        dq LIT,LIT,COMMA,COMMA
LIT1:   dq EXIT

defword "FLITERAL",8,F_IMMED,FLITERAL
; ( x -- )
; STATE @ IF ['] LIT , , THEN ; IMMEDIATE
        dq STATE,FETCH,ZBRANCH,FLIT1
        dq LIT,LIT,COMMA,COMMA
FLIT1:  dq EXIT

defword "2LITERAL",8,F_IMMED,TWOLITERAL
; ( x -- )
; STATE @ IF SWAP ['] LIT , , ['] LIT , , THEN ; IMMEDIATE
        dq STATE,FETCH,ZBRANCH,LIT2
        dq SWAP,LIT,LIT,COMMA,COMMA
        dq LIT,LIT,COMMA,COMMA
LIT2:   dq EXIT

defvar "(EXCEPTION-HANDLER)",19,0,EXCHANDLER,0

defword "CATCH",5,0,CATCH
; ( i*x xt -- j*x 0 | i*x n )
; SP@ >R (EXEPTION-HANDLER) @ >R
; RP@ (EXEPTION-HANDLER) !
; EXECUTE R> (EXEPTION-HANDLER) !
; R> DROP 0 ;
                dq DSPFETCH,TOR,EXCHANDLER,FETCH,TOR
                dq RSPFETCH,EXCHANDLER,FSTORE,EXECUTE
                dq FROMR,EXCHANDLER,FSTORE,FROMR,DROP
                dq LIT,0
                dq EXIT

defword "THROW",5,0,THROW
; ( k*x n -- k*x | i*x n )
; ?DUP IF (EXCEPTION-HANDLER) @ RP!
;       R> (EXCEPTION-HANDLER) ! 
;       R> SWAP >R SP! DROP R>
;      THEN ;
        dq EXCHANDLER,FETCH,ZEQ,ZBRANCH,TH2
        dq DUPF,LIT,-1,EQUAL,ZBRANCH,TH2
        dq QUIT
TH2:
TH3:    dq QDUP,ZBRANCH,TH1
        dq EXCHANDLER,FETCH,RSPSTORE
        dq FROMR,EXCHANDLER,FSTORE
        dq FROMR,SWAP,TOR,DSPSTORE,DROP,FROMR
TH1:    dq EXIT

defword "ABORT",5,0,ABORT
; ( i*x -- ) (R: j*x -- )
; -1 THROW ;
    dq LIT,-1,THROW
    dq EXIT

defword 'ABORT"',6,F_IMMED,QABORT
; ( ccc" | f -- )
; 34 PARSE SLITERAL POSTPONE IF POSTPONE LIT -2 , POSTPONE THROW 
; POSTPONE ELSE POSTPONE 2DROP POSTPONE THEN
; EXIT ; IMMEDIATE
        dq SQUOTE
        dq COMPILE,ROT,IFF
        dq    COMPILE,EXCHANDLER,COMPILE,FETCH
        dq    COMPILE,LIT,LIT,0,COMMA,COMPILE,EQUAL,IFF
        dq       COMPILE,TYPEF,COMPILE,QUIT
        dq    ELSEF,COMPILE,LIT
        dq       LIT,-2,COMMA,COMPILE,THROW
        dq THEN,THEN,COMPILE,TWODROP
        dq COMPILE,EXIT,EXIT

defword "SAYOK",5,0,SAYOK
; ( -- )
; LIT word COUNT TYPE ;
        dq LIT,okword,COUNT,TYPEF,EXIT

defword "PLACE",5,0,PLACE
; over over >r >r char+ swap chars cmove r> r> c! ;
        dq OVER,OVER,TOR,TOR,CHARPLUS,SWAP
        dq CHARS,CMOVEF,FROMR,FROMR,STOREBYTE
        dq EXIT

defword "INTERPRET",9,0,INTERPRET
; ( i*x c-addr u -- j*x )
; #TIB ! TIB ! 0 >IN !
; BEGIN
;   BL WORD DUP C@
; WHILE
;   FIND 
;   ?DUP IF
;       1+ STATE @ 0= OR
;       IF EXECUTE ELSE , THEN
;   ELSE
;       COUNT OVER C@ DIGIT? SWAP DROP
;       2 PICK C@ 45 = OR
;       IF
;           CONVERT NOT
;           IF
;               TYPE 3F EMIT SPACE ABORT QUIT
;           THEN
;       ELSE
;           TYPE 3F EMIT SPACE ABORT QUIT
;       THEN
;   THEN
; REPEAT DROP ;
        dq NTIB,FSTORE,TIB,FSTORE
        dq LIT,0,TOIN,FSTORE
;INBEG: dq BL,WORD,DUP,FETCHBYTE
INBEG:  dq PARSENAME,PAD,LIT,80,PLUS,PLACE
        ;dq BLF,WORDF,COUNT,PAD,LIT,80,PLUS,PLACE
        dq PAD,LIT,80,PLUS,DUPF,FETCHBYTE
        dq ZBRANCH,INEND
        dq FIND,QDUP,ZBRANCH,INT4
        dq LIT,1,PLUS,STATE,FETCH
        dq LIT,0,EQUAL,ORF
        dq ZBRANCH,INT2
        dq EXECUTE,BRANCH,INT03
INT2:   dq COMMA
INT03:  dq BRANCH,INT8
INT4:   dq COUNT,OVER,FETCHBYTE,DIGITQ,SWAP,DROP
        dq LIT,2,PICK,FETCHBYTE,LIT,45,EQUAL,ORF
INT13:  dq ZBRANCH,INT5
        dq CONVERT,NOTF,ZBRANCH,INT9
        dq TYPEF,LIT,63,EMIT,SPACE,ABORT,QUIT
INT9:   dq BRANCH,INT6
INT5:   dq TYPEF,LIT,63,EMIT,SPACE,ABORT,QUIT
INT6:
INT8:   dq BRANCH,INBEG
INEND:  dq DROP,EXIT

defword "EVALUATE",8,0,EVALUATE
; ( i*x caddr u -- j*x )
; TIB @ #TIB @ >R >R >IN @ >R
; INTERPRET
; R> >IN ! R> R> #TIB ! TIB ! ;
        dq SOURCEID,FETCH,TOR
        dq LIT,-1,SOURCEID,FSTORE
        dq TIB,FETCH,NTIB,FETCH
        dq TOR,TOR,TOIN,FETCH,TOR
        dq INTERPRET
        dq FROMR,TOIN,FSTORE,FROMR,FROMR
        dq NTIB,FSTORE,TIB,FSTORE
        dq FROMR,SOURCEID,FSTORE
        dq EXIT

defword "TRUE",4,0,FTRUE
; ( -- -1 )
                dq LIT,-1
                dq EXIT

defword "FALSE",5,0,FFALSE
; ( -- 0 )
                dq LIT,0
                dq EXIT

defword ">MARK",5,0,FMARK
; ( -- a )
; HERE 0 , ;
                dq HERE,LIT,0,COMMA,EXIT

defword ">RESOLVE",8,0,FRESOLVE
; ( a -- )
; HERE SWAP ! ;
                dq HERE,SWAP,FSTORE,EXIT

defword "<MARK",5,0,BMARK
; ( -- a )
; HERE ;
                dq HERE,EXIT

defword "<RESOLVE",8,0,BRESOLVE
; ( a -- )
; , ;
         dq COMMA,EXIT

;defword "FOR",3,F_IMMED,FOR
; ( -- a )
; COMPILE >R <MARK ; IMMEDIATE
;        dq COMPILE,TOR,BMARK,EXIT

defword "BEGIN",5,F_IMMED,BEGIN
; ( -- a )
; <MARK ; IMMEDIATE
         dq BMARK,EXIT

;defword "NEXT",4,F_IMMED,NEXT
; ( a -- ) 
; COMPILE NEXT <RESOLVE ; IMMEDIATE
;        dq COMPILE,NEXT,BRESOLVE,EXIT

defword "UNTIL",5,F_IMMED,UNTIL
; ( a -- )
; COMPILE ?BRANCH <RESOLVE ; IMMEDIATE
         ;dq COMPILE,ZBRANCH,BRESOLVE,EXIT
         dq LIT,ZBRANCH,COMMA,COMMA,EXIT

defword "AGAIN",5,F_IMMED,AGAIN
; ( a -- )
; COMPILE BRANCH <RESOLVE ; IMMEDIATE
                dq COMPILE,BRANCH,BRESOLVE,EXIT

defword "IF",2,F_IMMED,IFF
; ( -- A )
; COMPILE ?BRANCH >MARK ; IMMEDIATE
                dq COMPILE,ZBRANCH,FMARK,EXIT

defword "REPEAT",6,F_IMMED,REPEATF
; ( A a -- ) 
; [COMPILE] AGAIN >RESOLVE ; IMMEDIATE
                dq AGAIN,FRESOLVE,EXIT

defword "THEN",4,F_IMMED,THEN
; ( A -- ) 
; >RESOLVE ; IMMEDIATE
                dq FRESOLVE,EXIT

defword "AHEAD",5,F_IMMED,AHEAD
; ( -- A ) 
; COMPILE BRANCH >MARK ; IMMEDIATE
                dq COMPILE,BRANCH,FMARK,EXIT

defword "ELSE",4,F_IMMED,ELSEF
; ( A -- A ) 
; [COMPILE] AHEAD SWAP [COMPILE] THEN ; IMMEDIATE
                dq AHEAD,SWAP,THEN,EXIT

defword "WHILE",5,F_IMMED,WHILEF
; ( A -- A a ) 
; [COMPILE] IF SWAP ; IMMEDIATE
                dq IFF,SWAP,EXIT

; defword "(DO)",4,0,PARENSDO
; ( L I -- | RS: I L )
; R> ROT ROT >R >R >R ;
       ; dq FROMR,ROT,ROT,TOR,TOR
       ; dq TOR
       ; dq EXIT
; SWAP 0x80000000 - DUP ROT PLUS
; R> -ROT >R >R >R ;
        ; dq SWAP,LIT,0x8000000000000000,SWAP,MINUS
        ; dq DUPF,ROT,PLUS
        ; dq FROMR,MROT
        ; dq TOR,TOR,TOR
        ; dq EXIT

; defword "(+LOOP)",7,0,PARENSPLUSLOOP
; ( n -- )
; R> R> R> ROT >R ROT + 2DUP <=
; IF
;   DROP DROP R> 8 + >R EXIT
; THEN 
; R> ROT ROT >R >R >R ;
;        dq FROMR,FROMR,FROMR,ROT,TOR
;        dq ROT,PLUS,TWODUP;,XOR
;        dq SWAP,LT,ZBRANCH,LOP1
;        dq DROP,DROP,FROMR,LIT,8,PLUS,TOR,EXIT
;LOP1:   dq FROMR,ROT,ROT,TOR,TOR,TOR
;        dq EXIT
; R> R> R> ROT >R ROT OV+
; IF
;   DROP DROP R> 8 + >R EXIT
; THEN 
; R> ROT ROT >R >R >R ;
        ; dq FROMR,FROMR,FROMR,ROT,TOR
        ; dq ROT,OVPLUS
        ; dq ZBRANCH,LOP1
        ; dq DROP,DROP,FROMR,LIT,16,PLUS,TOR,EXIT
; LOP1:   dq FROMR,ROT,ROT,TOR,TOR,TOR
        ; dq EXIT

; defword "(LOOP)",6,0,PARENSLOOP
; ( -- )
; R> R> R> ROT >R 1+ 2DUP <=
; IF
  ; DROP DROP R> 8 + >R EXIT
; THEN 
; R> ROT ROT >R >R >R ;
       ; dq FROMR,FROMR,FROMR,ROT,TOR
       ; dq ONEPLUS,TWODUP
       ; dq LTE,ZBRANCH,LO1
       ; dq DROP,DROP,FROMR,LIT,8,PLUS,TOR,EXIT
; LO1:    dq FROMR,ROT,ROT,TOR,TOR,TOR
       ; dq EXIT
; R> R> R> ROT >R 1 OV+
; IF
  ; DROP DROP R> 8 + >R EXIT
; THEN 
; R> ROT ROT >R >R >R ;
        ; dq FROMR,FROMR,FROMR,ROT,TOR
        ; dq LIT,1,OVPLUS
        ; dq ZBRANCH,LO1
        ; dq DROP,DROP,FROMR,LIT,16,PLUS,TOR,EXIT
; LO1:    dq FROMR,ROT,ROT,TOR,TOR,TOR
        ; dq EXIT

defcode "(DO)",4,0,PARENSDO
    pop     rbx
    pop     rdx
    lodsq
    PUSHRSP rax ; put exit addr in RS and skip
    PUSHRSP rdx ; limit
    PUSHRSP rbx ; index
    NEXT

defcode "(?DO)",5,0,PARENSQDO
    pop     rbx
    pop     rdx
    lodsq
    cmp     rbx, rdx
    je      QDOEND
    PUSHRSP rax ; put exit addr in RS and skip
    PUSHRSP rdx ; limit
    PUSHRSP rbx ; index
    NEXT
QDOEND:
    mov     IP, rax ; skip over do loop
    NEXT
    
defcode "(LOOP)",6,0,PARENSLOOP
    POPRSP  rax
    POPRSP  rdx
    inc     rax
    cmp     rdx, rax
    je  LOO1
    PUSHRSP rdx
    PUSHRSP rax
    mov     rbx, [IP]
    mov     IP, rbx
    NEXT
LOO1:
    add     RS, 8 ; lose exit addr
    add     IP, 8 ; skip over loop addr
    ;lodsq
    NEXT

defcode "(+LOOP)",7,0,PARENSPLUSLOOP
    POPRSP  rax ; index
    POPRSP  rdx ; limit
    sub     rax, rdx ; index-limit
    pop     rbx ; n 
    add     rbx, rax ; (index-limit+n)
    xor     rax, rbx ; (index-limit) and (index-limit+n) diff sign?
    js  PLOO1
    add     rbx, rdx ; new index = index-limit+n+limit
    PUSHRSP rdx
    PUSHRSP rbx
    mov     rbx, [IP]
    mov     IP, rbx
    NEXT
PLOO1:
    add     RS, 8 ; lose exit addr
    add     IP, 8 ; skip over loop addr
    NEXT
   
defcode "(LEAVE)",7,0,PARENLEAVE
    add     RS, 16    ; pop limit, index
    mov     IP, [RS]  ; copy exit addr to IP
    add     RS, 8     ; clear RS
    NEXT

defword "DO",2,F_IMMED,DO
; ( -- )
; COMPILE (DO) <MARK ; IMMEDIATE
        ;dq COMPILE,PARENSDO,BMARK
        dq COMPILE,PARENSDO,FMARK
        dq EXIT

defword "?DO",3,F_IMMED,QDO
; ( -- )
        dq COMPILE,PARENSQDO,FMARK
        dq EXIT 

defword "LOOP",4,F_IMMED,LOOPF
; ( -- )
; COMPILE (LOOP) COMPILE BRANCH <RESOLVE ;
    ;dq COMPILE,PARENSLOOP,BRESOLVE
        dq COMPILE,PARENSLOOP,DUPF,LIT,8,PLUS
        dq BRESOLVE,FRESOLVE
        dq EXIT

defword "+LOOP",5,F_IMMED,PLUSLOOP
; ( -- )
; COMPILE (+LOOP) COMPILE BRANCH <RESOLVE ;
    ;dq COMPILE,PARENSPLUSLOOP,BRESOLVE
    dq COMPILE,PARENSPLUSLOOP,DUPF,LIT,8,PLUS
    dq BRESOLVE,FRESOLVE
        dq EXIT

defcode "I",1,0,I
    push qword [RS]
    NEXT

defcode "J",1,0,J
    push qword [RS+24]
    NEXT
 
defcode "K",1,0,K
    push qword [RS+40]
    NEXT

defcode "UNLOOP",6,0,UNLOOP
    add RS, 24
    NEXT
    
defword "LEAVE",5,F_IMMED,LEAVEF
    dq COMPILE,PARENLEAVE
    dq EXIT

defword "POSTPONE",8,F_IMMED,FPOSTPONE
; ( -- )
; BL WORD 
;  DUP COUNT PAD SWAP DUP >R CMOVE \ copy word into PAD
;  FIND DUP 0=             \ find word in dicionary
;  IF                              \ check for a number
;   DROP                           \ drop non-xt
;   PAD R> >NUMBER IF DROP ELSE ['] LIT , , THEN \ comma number if found
;  ELSE
;    DUP ?IMMEDIATE 0>
;    IF  ,
;    ELSE ['] LIT , , ['] , ,
;    THEN
;    R> DROP                \ drop length count of word
;  THEN
;  THEN
;; IMMEDIATE
        dq BLF,WORDF,DUPF,COUNT,PAD,SWAP,DUPF
        dq TOR,CMOVEF
        dq FIND,ZEQ
        dq ZBRANCH,PO1
; >number? ( a u -- n -1 | a 0 )
        dq DROP,PAD,FROMR,STONUMBERQ,DROP
        dq LIT,LIT,COMMA,COMMA
        dq BRANCH,PO2
PO1:    dq DUPF,IMMEDQ,LIT,0,LTF
        dq ZBRANCH,PO3
        dq COMMA,BRANCH,PO4
PO3:    dq LIT,LIT,COMMA,COMMA,LIT,COMMA,COMMA
PO4:    dq FROMR,DROP
PO2:    dq EXIT

; defword "UNUSED",6,0,UNUSED
; ( -- n )
; GET-BRK  ( get end of data segment according to the kernel )
; DP ;  ( get current position in data segment )
; -
; 4 /  ( returns number of cells )
;
        ; dq LIT,0,LIT,__NR_brk
        ; dq SYSCALL1
        ; dq HERE,MINUS,LIT,8,DIV
        ; dq EXIT

; defword "MORECORE",8,,MORECORE
;( cells -- )

        ; dq CELLS,LIT,0,LIT,__NR_brk
        ; dq SYSCALL1
        ; dq PLUS,LIT,__NR_brk
        ; dq SYSCALL1
        ; dq EXIT

defword "CSTRING",7,0,CSTRING
; ( a u -- c-addr )
; SWAP OVER PAD SWAP CMOVE
; PAD + 0 SWAP C! PAD
        dq SWAP,OVER,PAD,SWAP,CMOVEF
        dq PAD,PLUS,LIT,0,SWAP,STOREBYTE,PAD
        dq EXIT

defword "STRLEN",6,0,STRLEN
; ( str -- len )
; DUP  ( save start address )
; BEGIN
;  DUP C; 0<> ( zero byte found? )
; WHILE
;  1+
; REPEAT
; SWAP - ; ( calculate the length )
        dq DUPF
SLBEG:  dq DUPF,FETCHBYTE,ZNEQ,ZBRANCH,SLEND
        dq ONEPLUS,BRANCH,SLBEG
SLEND:  dq SWAP,MINUS
        dq EXIT

defword "ARGC",4,0,ARGC
; ( -- n )
; argc ;
        dq LIT,0
        dq EXIT

defword "ARGV",4,0,ARGV
; ( n -- a u )
; 1+ CELLS S0 ; + ; DUP STRLEN ;
        ; dq ONEPLUS,CELLS,S0,FETCH,PLUS
        ; dq FETCH,DUP,STRLEN
        dq LIT,0
        dq EXIT

defword "BUFFER:",7,0,BUFFERCOLON
; ( n ccccc -- )
; CREATE ALLOT ;
        dq CREATE,ALLOT
        dq EXIT

defword "R/O",3,0,RO
; ( -- fam )
; 0 ;
        dq LIT,31,EXIT

defword "W/O",3,0,WO
; ( -- fam )
; 1 ;
        dq LIT,30,EXIT

defword "R/W",3,0,FRW
; ( -- fam )
; 2 ;
        dq LIT,28,EXIT

defword "BIN",3,0,BIN
; ( fam -- fam )
; ;
        dq EXIT

defcode "CREATE-FILE",11,0,CREATEFILE
; ( a u fam -- fd t )
    mov     rdx, 1
    pop     rcx
    shl     rdx, cl ; set the acces mode e.g. R/O

    pop     rcx ; num of bytes
    pop     r10
    push    rsi ; save IP
    mov     rsi, r10 ; source address
    mov     rdi, [var_FDP]
    add     rdi, 80 ; set dest to PAD
    mov     r8, rdi ; save for later
    rep     movsb  ; move the string to PAD
    mov     byte [rdi], 0
    
    sub     rsp, 32 + 4*8
    mov     rcx, r8 ; filename
    ;mov     rdx, rax
    mov     r8, 1 ; file_shared_read
    mov     r9, 0 ; NULL
    mov     qword [rsp + 4*8], 2 ; create_always
    mov     qword [rsp + 5*8], 128 ; file_attribute_normal
    mov     qword [rsp + 6*8], 0; NULL 
    call    [CreateFileA]
    add     rsp, 32 + 4*8
    pop     rsi ; restore IP
    
    cmp     rax, 0
    je      crerr
    push    rax
    push    0
    NEXT
crerr:
    push 0
    push -1
    NEXT

defcode "OPEN-FILE",9,0,OPENFILE
; ( a u fam -- fd t )
    mov     rdx, 1
    pop     rcx
    shl     rdx, cl ; set the acces mode e.g. R/O

    pop     rcx ; num of bytes
    pop     r10
    push    rsi ; save IP
    mov     rsi, r10 ; source address
    mov     rdi, [var_FDP]
    add     rdi, 80 ; set dest to PAD
    mov     r8, rdi ; save for later
    rep     movsb  ; move the string to PAD
    mov     byte [rdi], 0
    
    mov     r15, rsp
    sub     rsp, 48 + 4*8
    and     spl, 0xF0
    mov     rcx, r8 ; filename
    ;mov     rdx, rax
    mov     r8, 1 ; file_shared_read
    mov     r9, 0 ; NULL
    mov     qword [rsp + 4*8], 3 ; open_existing
    mov     qword [rsp + 5*8], 128 ; file_attribute_normal
    mov     qword [rsp + 6*8], 0; NULL 
    call    [CreateFileA]
    mov     rsp, r15
    pop     rsi ; restore IP
    
    cmp     rax, 0
    jle     operr
    push    rax
    push    0
    NEXT
operr:
    push 0
    push -1
    NEXT
    

defcode "CLOSE-FILE",10,0,CLOSEFILE
; ( fd -- ior )
    pop     rcx
    ;mov rax, 1
    mov     r15, rsp
    sub     rsp, 48
    and     spl, 0xF0
    call    [CloseHandle]
    mov     rbx, 0
    mov     rcx, -1
    cmp     rax, 0
    cmovne  rbx, rcx
    mov     rsp, r15
    push    rbx
    NEXT


defcode "DELETE-FILE",11,0,DELETEFILE
; ( a u -- ior )
    pop     rcx ; num of bytes
    pop     r10 ; address
    push    rsi ; save IP
    mov     rsi, r10 ; source address
    mov     rdi, [var_FDP]
    add     rdi, 80 ; set dest to PAD
    mov     r8, rdi ; save for later
    rep     movsb  ; move the string to PAD
    mov     byte [rdi], 0
    
    sub     rsp, 32 + 4*8
    mov     rcx, r8 ; filename
    call    [DeleteFileA]
    add     rsp, 32 + 4*8
    pop     rsi ; restore IP
    
    cmp     rax,0
    je      dferr
    push    0
    NEXT
dferr:
    push    -1
    NEXT
    
defcode "READ-FILE",9,0,READFILE
; ( a u fd -- u2 ior )
    pop rcx
    pop r8
    pop rdx
    lea r9, [Written]
    mov r15, rsp
    sub rsp, 48 + 4*8
    and spl, 0xF0
    mov qword [rsp + 4*8], 0
    call    [ReadFile]
    cmp rax, 0
    je  rderr
    mov rax, [Written]
    mov rsp, r15
    push rax
    push 0
    NEXT
rderr:
    mov rsp, r15
    push 0
    push rax
    NEXT

defword "READ-LINE",9,0,READLINE
; ( a u1 fd -- u2 f ior )
 ; >r 0 -rot ( u2 a u1 )
 ; begin
   ; over 1 r@ read-file
   ; throw
   ; 0= if ( EOF ) rdrop 2drop 
    ; dup 0= if drop 0 0 0 EXIT else
     ; -1 0 EXIT then then
   ; over c@ dup 13 = 
    ; if rdrop 2drop drop -1 0 exit then
    ; 10 <> if
     ; rot 1+ -rot \ incr the count
     ; swap 1+ swap \ incr the buffer address
    ; then
   ; 1- dup 0= \ test if the buffer is full
 ; until
 ; rdrop 2drop -1 0
; ;
    dq TOR,LIT,0,MROT
RLBEG:
    dq OVER,LIT,1,RFETCH,READFILE
    dq THROW,ZEQ,ZBRANCH,RL1
    dq RDROP,TWODROP,DUPF,ZEQ,ZBRANCH,RL2
    dq DROP,LIT,0,LIT,0,LIT,0,EXIT
    dq BRANCH,RL1
RL2:
    dq LIT,-1,LIT,0,EXIT
RL1:
    dq OVER,FETCHBYTE,DUPF,LIT,13,EQUAL
    dq ZBRANCH,RL3
    dq RDROP,TWODROP,DROP,LIT,-1,LIT,0,EXIT
RL3:
    dq LIT,10,NEQUAL,ZBRANCH,RL4
    dq ROT,ONEPLUS,MROT,SWAP,ONEPLUS,SWAP
RL4:
    dq ONEMINUS,DUPF,ZEQ,ZBRANCH,RLBEG
    dq RDROP,TWODROP,LIT,-1,LIT,0
    dq EXIT

defcode "WRITE-FILE",10,0,WRITEFILE
; ( a u fd -- ior )
    pop rcx
    pop r8
    pop rdx
    lea r9, [Written]
    sub rsp, 32 + 2*8
    mov qword [rsp + 4*8], 0
    call    [WriteFile]
    add rsp, 32 + 2*8
    cmp rax,0
    jl  wferr
    push 0
    NEXT
 wferr:
    push -1
    NEXT
    
defword "WRITE-LINE",10,0,WRITELINE
; ( a u fd -- ior )
    dq DUPF,TOR,WRITEFILE,ZBRANCH,WL1
    dq LIT,-1,RDROP,EXIT
WL1:
    dq LITSTRING,2
    db 13,10,0,0,0,0,0,0
    dq FROMR,WRITEFILE
    dq EXIT
 
defcode "FILE-POSITION",13,0,FILEPOS
; ( fd -- d )
    pop     rcx
    sub     rsp,40
    mov     rdx,0
    lea     r8, [rsp + 4*8]
    mov     r9, 1
    call    [SetFilePointer]
    mov     rbx, [rsp + 4*8]
    add     rsp,40
    push    rax
    push    rbx
    cmp     rax,0
    jl      fperr
    push    0
    NEXT
fperr:
    push    -1
    NEXT

defcode "REPOSITION-FILE",15,0,REPOSFILE
; ( d fd -- f )
    pop     rcx
    pop     rdx
    pop     r8
    sub     rsp,32
    mov     r9, 0
    call    [SetFilePointer]
    add     rsp,32
    cmp     rax,0
    jl      rperr
    push    0
    NEXT
rperr:
    push    -1
    NEXT

defcode "FILE-SIZE",9,0,FILESIZE
; ( fd -- d ior )
    pop rcx
    sub rsp,40
    lea rdx, [rsp + 4*8]
    call [GetFileSize]
    mov rbx, [rsp + 4*8]
    add rsp,40
    push rax
    push rbx
    cmp rax,0
    jl  fserr
    push 0
    NEXT
fserr:
    push 1
    NEXT

defword "INCLUDE-FILE",12,0,INCLUDEFILE
; ( i*x fd -- j*x )
; >R begin pad 100 r@ read-line throw while
        dq SOURCEID,FETCH,TOR
        dq DUPF,SOURCEID,FSTORE
        dq TIB,FETCH,DUPF,TOR
        dq NTIB,FETCH,DUPF,TOR
        dq PLUS,TIB,FSTORE,LIT,0,NTIB,FSTORE
        dq TOIN,FETCH,TOR,LIT,0,TOIN,FSTORE
        dq TOR
INCB:   
        dq TIB,FETCH,LIT,130,RFETCH,READLINE
        dq THROW,SWAP,NTIB,FSTORE
        dq ZBRANCH,INC1
        dq LIT,0,TOIN,FSTORE
        dq SOURCE,INTERPRET
        dq BRANCH,INCB
INC1:   
        dq FROMR,CLOSEFILE,THROW
        dq FROMR,TOIN,FSTORE,FROMR,NTIB,FSTORE
        dq FROMR,TIB,FSTORE,FROMR,SOURCEID,FSTORE
        dq EXIT

defword "INCLUDED",8,0,INCLUDED
; ( i*x a u -- j*x )
; R/O OPEN-FILE THROW INCLUDE-FILE ;
        ;dq RO,OPENFILE,THROW,INCLUDEFILE
        dq TWODUP,TWOTOR,RO,OPENFILE,LIT,0,EQUAL,ZBRANCH,NOPE
        dq INCLUDEFILE,TWORDROP,BRANCH,YEP
NOPE:
        dq DROP
        dq DOSTRING,18
        db "CANNOT OPEN FILE: ",0,0,0,0,0,0
        dq TYPEF,TWOFROMR,TYPEF
YEP:
        dq EXIT

defword "INCLUDE",7,0,INCLUDEF
; ( i*x "name" -- j*x )
; PARSE-NAME INCLUDED ;
        dq PARSENAME,INCLUDED
        dq EXIT

defword "CASE",4,F_IMMED,CASE
; ( n -- )
; push 0 to mark the bottom of the stack
; 0 ; IMMEDIATE
        dq LIT,0,PARDEFAULT,FSTORE
        dq LIT,0,EXIT

defword "OF",2,F_IMMED,OF
; ( n t -- n ) 
; COMPILE OVER COMPILE = POSTPONE IF COMPILE DROP ; IMMEDIATE
        dq LIT,OVER,COMMA
        dq LIT,EQUAL,COMMA
        dq IFF
        dq LIT,DROP,COMMA
        dq EXIT

defword "ENDOF",5,F_IMMED,ENDOF
; ( )
; POSTPONE ELSE ; IMMEDIATE
        dq ELSEF
        dq EXIT

defword "DEFAULT:",8,F_IMMED,DEFAULTF
    dq LIT,DUPF,COMMA
    dq OF,LIT,-1,PARDEFAULT,FSTORE,EXIT
    
defword "ENDCASE",7,F_IMMED,ENDCASE
; ( )
; COMPILE DROP
; keep compiling then until the 0 marker is found
; BEGIN ?DUP WHILE POSTPONE THEN REPEAT ; IMMEDIATE
        dq PARDEFAULT,FETCH,ZBRANCH,EC1
        dq ENDOF
EC1:    dq LIT,DROP,COMMA
ECB:    dq QDUP,ZBRANCH,ECR
        dq THEN
        dq BRANCH,ECB
ECR:    dq LIT,0,PARDEFAULT,FSTORE
        dq EXIT
    
defcode "F@",2,0,FFETCH
; ( a -- r )
    pop rax
    movsd xmm0,[rax]
    FPUSH xmm0
    NEXT
  
defcode "F!",2,0,FLSTORE
; ( r a -- )
    pop rax
    FPOP xmm0
    movsd [rax],xmm0
    NEXT
   
defcode "FDUP",4,0,FDUP
; ( r -- r r )
    FPOP xmm0
    FPUSH xmm0
    FPUSH xmm0
    NEXT

defcode "FSWAP",5,0,FSWAP
; ( r1 r2 -- r2 r1 )
    FPOP xmm0
    FPOP xmm1
    FPUSH xmm1
    FPUSH xmm0
    NEXT

defcode "FDROP",5,0,FDROP
; ( r --  )
        FPOP xmm0
        NEXT

defcode "FOVER",5,0,FOVER
; ( r1 r2  -- r1 r2 r1 )
        FPOP xmm0
        FPOP xmm1
        FPUSH xmm1
        FPUSH xmm0
        FPUSH xmm1
        NEXT

defcode "FROT",4,0,FROT
; ( r1 r2 r3 -- r2 r3 r1 )
    FPOP xmm2
    FPOP xmm1
    FPOP xmm0
    FPUSH xmm1
    FPUSH xmm2
    FPUSH xmm0
    NEXT
    
defcode "F*",2,0,FMULF
; ( r r -- r )
        FPOP xmm0
        FPOP xmm1
        mulsd xmm0, xmm1
        FPUSH xmm0
        NEXT

defcode "F/",2,0,FDIVF
; ( r r -- r )
        FPOP xmm1
        FPOP xmm0
        divsd xmm0, xmm1
        FPUSH xmm0
        NEXT

defcode "F+",2,0,FPLUS
; ( r r -- r )
        FPOP xmm0
        FPOP xmm1
        addsd xmm0, xmm1
        FPUSH xmm0
        NEXT

defcode "F-",2,0,FMINUS
; ( r r -- r )
        FPOP xmm0
        FPOP xmm1
        subsd xmm0, xmm1
        FPUSH xmm0
        NEXT

defcode "FNEGATE",7,0,FNEGATE
; ( r -- -r )
    fld qword [FSP]
    fchs
    fst qword [FSP]
        NEXT

defcode "FROUND",6,0,FROUND
; ( r -- -r )
    FPOP xmm0
    roundsd xmm0, xmm0, 1
    FPUSH xmm0
        NEXT
    
defcode "F<",2,0,FLT
; ( -- f : r r --  )
    FPOP xmm0
        FPOP xmm1
    comisd xmm0,xmm1
        mov rax, -1
    mov rbx, 0
        cmovb rax, rbx
        push rax
        NEXT

defcode "F>",2,0,FGT
; ( -- f : r r --  )
    FPOP xmm0
        FPOP xmm1
    comisd xmm0,xmm1
        mov rax, -1
    mov rbx, 0
        cmova rax, rbx
        push rax
        NEXT

defcode "F0<",3,0,FZLT
; ( -- f : r --  )
    FPOP xmm0
        xorpd xmm1, xmm1
    comisd xmm0,xmm1
        mov rax, -1
    mov rbx, 0
        cmova rax, rbx
        push rax
        NEXT

defcode "F0=",3,0,FZEQ
; ( -- f : r --  )
    FPOP xmm0
        xorpd xmm1, xmm1
    comisd xmm0,xmm1
        mov rax, 0
    mov rbx, -1
        cmove rax, rbx
        push rax
        NEXT

defcode "FMAX",4,0,FMAX
; ( -- : r r -- r )
    FPOP xmm0
    FPOP xmm1
    maxsd xmm0,xmm1
    FPUSH xmm0
    NEXT
 
defcode "FMIN",4,0,FMIN
; ( -- : r r -- r )
    FPOP xmm0
    FPOP xmm1
    minsd xmm0,xmm1
    FPUSH xmm0
    NEXT
    
defcode "F>D",3,0,FTOD
; ( -- d : r -- )
    FPOP xmm0
    cvtsd2si rax, xmm0
    push rax
    push 0
    NEXT
    
defcode "FSQRT",5,,FSQRTF
; ( -- f : r --  )
    FPOP xmm0
    sqrtsd xmm0, xmm0
    FPUSH xmm0
    NEXT

defcode "FABS",4,0,FABSF
    FPOP xmm0
    mov r15, rsp
    sub rsp, 16
    and spl, 0xF0
    call [_fabs]
    mov rsp, r15
    FPUSH xmm0
    NEXT

defcode "FCOS",4,0,FCOSF
    FPOP xmm0
    mov r15, rsp
    sub rsp, 16
    and spl, 0xF0
    call [cos]
    mov rsp, r15
    FPUSH xmm0
    NEXT
 
 defcode "FCOSH",5,0,FCOSHF
        FPOP xmm0
    push rsi
    sub rsp,40
        call [cosh]
    add rsp,40
        FPUSH xmm0
    pop rsi
        NEXT
    
defcode "FACOS",5,0,FACOSF
        FPOP xmm0
    push rsi
    sub rsp,40
        call [acos]
    add rsp,40
        FPUSH xmm0
    pop rsi
        NEXT

 defcode "FACOSH",6,0,FACOSHF
        FPOP xmm0
    push rsi
    sub rsp,40
        call [acosh]
    add rsp,40
        FPUSH xmm0
    pop rsi
        NEXT
    
 defcode "FSIN",4,0,FSINF
        FPOP xmm0
    push rsi
    sub rsp,40
        call [sin]
    add rsp,40
        FPUSH xmm0
    pop rsi
        NEXT

 defcode "FSINH",5,0,FSINHF
        FPOP xmm0
    push rsi
    sub rsp,40
        call [sinh]
    add rsp,40
        FPUSH xmm0
    pop rsi
        NEXT
    
defcode "FASIN",5,0,FASINF
        FPOP xmm0
    push rsi
    sub rsp,40
        call [asin]
    add rsp,40
        FPUSH xmm0
    pop rsi
        NEXT

 defcode "FASINH",6,0,FASINHF
        FPOP xmm0
    push rsi
    sub rsp,40
        call [asinh]
    add rsp,40
        FPUSH xmm0
    pop rsi
        NEXT

 ; defcode "FSINCOS",7,0,FASINCOSF
        ; FPOP xmm0
    ; push rsi
    ; sub rsp,32
        ; call sincos
    ; add rsp,32
        ; FPUSH xmm0
    ; FPUSH xmm1
    ; pop rsi
        ; NEXT
    
 defcode "FTAN",4,0,FTANF
        FPOP xmm0
    push rsi
    sub rsp,40
        call [tan]
    add rsp,40
        FPUSH xmm0
    pop rsi
        NEXT

 defcode "FTANH",5,0,FTANHF
        FPOP xmm0
    push rsi
    sub rsp,40
        call [tanh]
    add rsp,40
        FPUSH xmm0
    pop rsi
        NEXT

 defcode "FATAN",5,0,FATANF
        FPOP xmm0
    push rsi
    sub rsp,40
        call [atan]
    add rsp,40
        FPUSH xmm0
    pop rsi
        NEXT
 
  defcode "FATANH",6,0,FATANHF
        FPOP xmm0
    push rsi
    sub rsp,40
        call [atanh]
    add rsp,40
        FPUSH xmm0
    pop rsi
        NEXT
    
 defcode "FATAN2",6,0,FATAN2F
        FPOP xmm1
    FPOP xmm0
    push rsi
    sub rsp,40
        call [atan2]
    add rsp,40
        FPUSH xmm0
    pop rsi
        NEXT
    
 defcode "FEXP",4,0,FEXPF
        FPOP xmm0
    push rsi
    sub rsp,40
        call [exp]
    add rsp,40
        FPUSH xmm0
    pop rsi
        NEXT

 defcode "F**",3,0,FPOWF
        FPOP xmm1
    FPOP xmm0
    push rsi
    sub rsp,40
        call [pow]
    add rsp,40
        FPUSH xmm0
    pop rsi
        NEXT
    
 defcode "FEXPM1",6,0,FEXPM1F
        FPOP xmm0
    push rsi
    sub rsp,40
        call [expm1]
    add rsp,40
        FPUSH xmm0
    pop rsi
        NEXT

 defcode "FLNP1",5,0,FLNP1F
        FPOP xmm0
    push rsi
    sub rsp,40
        call [log1p]
    add rsp,40
        FPUSH xmm0
    pop rsi
        NEXT
    
 defcode "FLN",3,0,FLNF
        FPOP xmm0
    push rsi
    sub rsp,40
        call [log]
    add rsp,40
        FPUSH xmm0
    pop rsi
        NEXT

 defcode "FLOG",4,0,FLOGF
        FPOP xmm0
    push rsi
    sub rsp,40
        call [log10]
    add rsp,40
        FPUSH xmm0
    pop rsi
        NEXT

 defcode "FLOOR",5,0,FLOORF
        FPOP xmm0
        push rsi
        sub rsp,40
        call [floor]
        add rsp,40
        FPUSH xmm0
        pop rsi
        NEXT

defcode "CEIL",4,0,CEILF
        FPOP xmm0
        push rsi
        sub rsp,40
        call [ceil]
        add rsp,40
        FPUSH xmm0
        pop rsi
        NEXT

defcode "FTRUNC",6,0,FTRUNCF
        FPOP xmm0
    push rsi
    sub rsp,40
        call [trunc]
    add rsp,40
        FPUSH xmm0
    pop rsi
        NEXT
    
defword "FDEPTH",6,0,FDEPTH
; ( -- n ) 
; FSP@ F0 @ SWAP - 1 FLOATS / ;
        dq FSPFETCH,F0,FETCH
        dq SWAP,MINUS,LIT,8
        dq DIVF
        dq EXIT

defword "VOCABULARY",10,0,VOCAB
; ( parse"a u -- )
; here dup
; create 0 ,     \ 0 for the word list 
; ,              \ save the link addr
; voc-link @ ,
; voc-link !
; does> context ! ;
        dq HERE,DUPF,CREATE,LIT,0,COMMA
        dq COMMA,VOCLINK,FETCH,COMMA
        dq VOCLINK,FSTORE
        dq DOES
VODOES: dq CONTEXT,FSTORE
        dq EXIT

defword "DEFINITIONS",11,0,DEFINITIONS
; ( -- )
;    context @
;    current ! ;
        dq CONTEXT,FETCH,CURRENT,FSTORE
        dq EXIT

defword "ORDER",5,0,ORDER,rootlink
; ( -- )
; cr ." context: " #vocs 0 do context i cells + @
; ?dup if cell+ @ cell+ count type space then loop
; cr ." current: " current @ cell+ @ cell+ count type cr ;
        dq CR,DOSTRING,9
        db "context: ",0,0,0,0,0,0,0
        dq TYPEF,NVOCS,LIT,0,PARENSDO,ORD1
ORD0:   dq CONTEXT,I,CELLS,PLUS,FETCH
        dq QDUP,ZBRANCH,ORD2
        dq CELLPLUS,FETCH,CELLPLUS
        dq COUNT,TYPEF,SPACE
ORD2:   dq PARENSLOOP,ORD0
ORD1:   dq CR,DOSTRING,9
        db "current: ",0,0,0,0,0,0,0
        dq TYPEF,CURRENT,FETCH,CELLPLUS
        dq FETCH,CELLPLUS,COUNT
        dq TYPEF,CR
        dq EXIT

defword "ALSO",4,0,ALSO
        dq LIT,1,NVOCS,LIT,2,MINUS
        dq PARENSDO,ALS1
ALS0:   dq CONTEXT,I,CELLS,PLUS,DUPF,CELLMINUS,FETCH
        dq SWAP,FSTORE,LIT,-1
        dq PARENSPLUSLOOP,ALS0
ALS1:   dq EXIT

defword "PREVIOUS",8,0,PREVIOUS
        dq LIT,0,CONTEXT,NVOCS,LIT,1,MINUS
        dq CELLS,PLUS,FSTORE
        dq NVOCS,LIT,1,MINUS,LIT,0
        dq PARENSDO,PREV1
PREV0:  dq CONTEXT,I,CELLS,PLUS,DUPF,CELLPLUS
        dq FETCH,SWAP,FSTORE
        dq PARENSLOOP,PREV0
PREV1:  dq EXIT

defword "ONLY",4,0,ONLY
        dq NVOCS,LIT,0
        dq PARENSDO,ONL1
ONL0:   dq LIT,0,CONTEXT,I,CELLS,PLUS,FSTORE
        dq PARENSLOOP,ONL0
ONL1:   dq ROOT,ALSO
        dq EXIT

defword "GET-ORDER",9,0,GETORDER
; ( -- voc1 ... vocn n )
        dq DEPTH,TOR,LIT,0,NVOCS,LIT,1,MINUS
        dq PARENSDO,GORD1
GORD0:  dq CONTEXT,I,CELLS,PLUS,FETCH
        dq QDUP,ZBRANCH,GORD2
        ;dq
GORD2:  dq LIT,-1,PARENSPLUSLOOP,GORD0
GORD1:  dq DEPTH,FROMR,MINUS
        dq EXIT

defword "GET-CURRENT",11,0,GETCURRENT
; ( -- voc )
        dq CURRENT,FETCH
        dq EXIT

defword ":NONAME",7,0,COLNONAME
; :noname here docol , ] ;
        dq HERE,LIT,DOCOL,COMMA,RBRAC
        dq EXIT

defword "DEFER",5,0,DEFER        
; DEFER ( "name" -- )
;   CREATE ['] ABORT ,
;DOES> ( ... -- ... )
;   @ EXECUTE ;
        dq CREATE,LIT,ABORT,COMMA
        dq DOES,FETCH,EXECUTE
        dq EXIT

defword "DEFER!",6,0,DEFERSTORE
; DEFER! ( xt2 xt1 -- )
;   >BODY ! ;
        dq TOBODY,FSTORE
        dq EXIT
        
defword "DEFER@",6,0,DEFERFETCH
; DEFER@ ( xt1 -- xt2 )
;   >BODY @ ;
        dq TOBODY,FETCH
        dq EXIT

defword "IS",2,F_IMMED,IS
; IS
;   STATE @ IF
;     POSTPONE ['] POSTPONE DEFER!
;   ELSE
;     ' DEFER!
;   THEN ; IMMEDIATE
        dq STATE,FETCH,ZBRANCH,IS1
        dq BRACTICK
        dq COMPILE,DEFERSTORE
        dq BRANCH,IS2
IS1:    dq TICK,DEFERSTORE
IS2:    dq EXIT

defcode "GO",2,0,INNER
        xor rax,rax
        mov rbx,100000
LL2:
        mov rcx,100000
LL1:
        add rax,1
        sub rcx,1
        jnz LL1
        sub rbx,1
        jnz LL2
        push rax
        NEXT

DllNotFound db "Cannot load library",0
FunctionNotFound db "Function not found",0

defcode "LOADLIBRARY",11,0,DLOPEN
        pop rcx           ; cstring dll filename
        mov r15, rsp      ; save r15
        sub rsp, 40
        and spl, 0xF0
        call [LoadLibraryA]
        mov rsp, r15
        push rax
        NEXT

defcode "GETPROCADDR",11,0,GETPROC
        pop rcx            ; hDLL
        pop rdx            ; cstring func name
        mov r15, rsp
        sub rsp, 40
        and spl, 0xF0
        call [GetProcAddress]
        mov rsp, r15
        push rax
        NEXT

defcode "0CALL",5,0,FCALL0
        pop r8
        mov r15, rsp
        sub rsp, 40
        and spl, 0xF0
        call r8
        mov rsp, r15
        push rax
        NEXT

defcode "1CALL",5,0,FCALL1
        pop r8
        pop rcx
        ;mov r15, rsp
        ;sub rsp, 40
        ;and spl, 0xF0
        call r8
        ;mov rsp, r15
        push rax
        NEXT

defcode "2CALL",5,0,FCALL2
        pop r8
        pop rdx
        pop rcx
        mov r15, rsp
        sub rsp, 32
        and spl, 0xF0
        call r8
        mov rsp, r15
        push rax
        NEXT

defcode "3CALL",5,0,FCALL3
        pop r9
        pop r8
        pop rdx
        pop rcx
        mov r15, rsp
        sub rsp, 32
        and spl, 0xF0
        call r8
        mov rsp, r15
        push rax
        NEXT

defcode "4CALL",5,0,FCALL4
        pop rbx
        pop r9
        pop r8
        pop rdx
        pop rcx
        mov r15, rsp
        sub rsp, 32
        and spl, 0xF0
        call r8
        mov rsp, r15
        push rax
        NEXT

defword "CODE",4,0,FCODE
       dq BLF,WORDF,COUNT
       dq HEADERCOMMA,HERE,CELLPLUS
       ;dq COMMA,LATEST,FETCH,HIDDEN
       dq COMMA,CURRENT,FETCH,FETCH,HIDDEN
       ;dq RBRAC
       dq ALIGNF
       dq EXIT

defword "NEXT",4,0,FNEXT
        dq LIT,0x48,COMMABYTE
        dq LIT,0xad,COMMABYTE
        dq LIT,0xff,COMMABYTE
        dq LIT,0x20,COMMABYTE
        dq EXIT

defword "END-CODE",8,0,FENDCODE
        ;dq LATEST,FETCH,HIDDEN
        dq CURRENT,FETCH,FETCH,UNHIDE
        ;dq LBRAC
        dq EXIT

; defword "@LOC0",5,0,ATLOC0
;         dq LOCBP,FETCH,LIT,1,CELLS
;         dq MINUS,FETCH
;         dq EXIT
defcode "@LOC0",5,0,ATLOC0
        push qword [LBP - 8]
        NEXT

; defword "@LOC1",5,0,ATLOC1
;         dq LOCBP,FETCH,LIT,2,CELLS
;         dq MINUS,FETCH
;         dq EXIT
defcode "@LOC1",5,0,ATLOC1
        push qword [LBP - 8*2]
        NEXT

defcode "@LOC2",5,0,ATLOC2
        push qword [LBP - 8*3]
        NEXT

defcode "@LOC3",5,0,ATLOC3
        push qword [LBP - 8*4]
        NEXT

defcode "@LOC4",5,0,ATLOC4
        push qword [LBP - 8*5]
        NEXT

defcode "@LOC5",5,0,ATLOC5
        push qword [LBP - 8*6]
        NEXT

defcode "@LOC6",5,0,ATLOC6
        push qword [LBP - 8*7]
        NEXT

defcode "@LOC7",5,0,ATLOC7
        push qword [LBP - 8*8]
        NEXT

defcode "LOC-SP",6,0,LOCSP
        push LSP
        NEXT

defcode "LOC-BP",6,0,LOCPB
        push LBP
        NEXT

; defword ">L",2,0,TOL
;         dq LIT,-8,LOCSP,PLUSSTORE,LOCSP
;         dq FETCH,FSTORE
;         dq EXIT
; 
; defword "<L",2,0,FROML
;         dq LOCSP,FETCH,FETCH
;         dq LIT,8,LOCSP,PLUSSTORE
;         dq EXIT

defcode ">L",2,0,TOL
        sub LSP, 8
        pop qword [LSP]
        NEXT

defcode "<L",2,0,FROML
        ;mov rax,[LSP]
        push qword [LSP]
        add LSP, 8
        NEXT

defcode ">L0",3,0,TOLOC0
        ;pop rax
        pop qword [LBP-8];, rax 
        NEXT

defcode ">L1",3,0,TOLOC1
        pop rax
        mov [LBP-2*8], rax 
        NEXT

defcode ">L2",3,0,TOLOC2
        pop rax
        mov [LBP-3*8], rax 
        NEXT

defcode ">L3",3,0,TOLOC3
        pop rax
        mov [LBP-4*8], rax 
        NEXT

defcode ">L4",3,0,TOLOC4
        pop rax
        mov [LBP-5*8], rax 
        NEXT

defcode ">L5",3,0,TOLOC5
        pop rax
        mov [LBP-6*8], rax 
        NEXT

defcode ">L6",3,0,TOLOC6
        pop rax
        mov [LBP-7*8], rax 
        NEXT

defcode ">L7",3,0,TOLOC7
        pop rax
        mov [LBP-8*8], rax 
        NEXT

; defword "LOC-UNWIND",10,0,LOCUNWIND
;         dq LOCBP,FETCH,LOCSP,FSTORE
;         dq FROML,LOCBP,FSTORE
;         dq EXIT
defcode "LOC-UNWIND",10,0,LOCUNWIND
  ; loc-bp @ loc-sp ! loc-sp @ @ loc-bp ! 8 loc-sp +! ; 
        mov LSP, LBP
        mov LBP, [LSP]
        add LSP, 8
        NEXT

; defword "LOC-INIT",8,0,LOCINIT
;         dq LOCBP,FETCH,TOL
;         dq LOCSP,FETCH,LOCBP,FSTORE
;         dq EXIT
defcode "LOC-INIT",8,0,LOCINIT
        sub LSP, 8
        mov [LSP], LBP
        mov LBP, LSP
        sub LSP, 8*16
        NEXT

defword "RESET-LOCALS",12,F_IMMED,RESETLOCALS
        dq LOCFLG,FETCH,ZBRANCH,resloc
        dq LIT,LOCUNWIND,COMMA
        dq LIT,0,LOCCNT,FSTORE
        dq LIT,0,LOCOFFSET,FSTORE
        dq FFALSE,LOCFLG,FSTORE
        dq FFALSE,LOCFRAME,FSTORE
        dq LIT,0,CONTEXT,FETCH,FSTORE
        dq LIT,0,LP,FSTORE
        dq PREVIOUS,DEFINITIONS
resloc: dq EXIT

defword "START-LOCALS",12,0,STARTLOCALS
        dq FTRUE,LOCFLG,FSTORE
        dq LOCFRAME,FETCH,ZEQ,ZBRANCH,staloc
        dq LOCDICT,LP,FSTORE
        dq ALSO,LOCVOC,DEFINITIONS
staloc: dq HERE,OLDDP,FSTORE
        dq LP,FETCH,FDP,FSTORE
        dq EXIT

defword "STOP-LOCALS",11,0,STOPLOCALS
        dq HERE,LP,FSTORE
        dq OLDDP,FETCH,FDP,FSTORE
        dq LOCFRAME,FETCH,ZEQ,ZBRANCH,stoloc
        ;dq LIT,LOCINIT,COMMA
        dq FTRUE,LOCFRAME,FSTORE
stoloc: dq LOCCNT,FETCH,LIT,0
        dq PARENSQDO,stoex
sto1:   dq LOCVEC,I,CELLS,PLUS,FETCH,COMMA
        dq PARENSLOOP,sto1
stoex:  dq EXIT

defword "CREATE-LOCAL",12,0,CREATELOCAL
        dq HEADERCOMMA,LIT,DOCOL2,COMMA
        dq LOCOFFSET,FETCH
        dq LIT,0,OVER,EQUAL,ZBRANCH,CL1
        dq DROP,COMPILE,ATLOC0,BRANCH,CLEX
CL1:    dq LIT,1,OVER,EQUAL,ZBRANCH,CL2
        dq DROP,COMPILE,ATLOC1,BRANCH,CLEX
CL2:    dq LIT,2,OVER,EQUAL,ZBRANCH,CL3
        dq DROP,COMPILE,ATLOC2,BRANCH,CLEX
CL3:    dq LIT,3,OVER,EQUAL,ZBRANCH,CL4
        dq DROP,COMPILE,ATLOC3,BRANCH,CLEX
CL4:    dq LIT,4,OVER,EQUAL,ZBRANCH,CL5
        dq DROP,COMPILE,ATLOC4,BRANCH,CLEX
CL5:    dq LIT,5,OVER,EQUAL,ZBRANCH,CL6
        dq DROP,COMPILE,ATLOC5,BRANCH,CLEX
CL6:    dq LIT,6,OVER,EQUAL,ZBRANCH,CL7
        dq DROP,COMPILE,ATLOC6,BRANCH,CLEX
CL7:    dq LIT,7,OVER,EQUAL,ZBRANCH,CLEX
        dq DROP,COMPILE,ATLOC7;,BRANCH,CLEX
CLEX:   dq LIT,1,LOCOFFSET,PLUSSTORE
        dq COMPILE,EXIT
        dq EXIT

defword "(LOCAL)",7,0,PARENSLOCAL
        dq TWODUP,LIT,0,LIT,0,DEQ,ZBRANCH,PLOC1
        dq TWODROP,FFALSE,LSTATE,FSTORE
        dq STOPLOCALS,LIT,0,LOCCNT,FSTORE,EXIT
PLOC1:  dq LSTATE,FETCH,ZEQ,ZBRANCH,PLOC2
        dq FTRUE,LSTATE,FSTORE,STARTLOCALS
PLOC2:  dq CREATELOCAL,EXIT

defword "LOCAL",5,0,LOCAL
        dq BLF,WORDF,COUNT,PARENSLOCAL
        dq EXIT

defword "{HELPER",7,0,BHELPER
        dq TOIN,FETCH,BLF,WORDF,COUNT,LIT,1,EQUAL
        dq SWAP,FETCHBYTE,LIT,125,EQUAL,ANDF
        dq ZBRANCH,HLP1
        dq DROP,TOIN,FETCH,BRANCH,HLP2
HLP1:   dq LIT,1,LOCCNT,PLUSSTORE,BHELPER
        dq SWAP,TOIN,FSTORE,LOCAL
HLP2:   dq EXIT

defword "{",1,F_IMMED,BRACE
        dq BHELPER,TOIN,FSTORE
        dq LIT,0,LIT,0,PARENSLOCAL
        dq EXIT

defcode "FOO",3,,FOO
        NEXT

defcode "SLEEP",5,0,SLEEP
        pop rcx
        mov r15, rsp
        sub rsp, 40
        call [Sleep]
        mov rsp, r15
        NEXT

defword "ISP",3,0,ISP
       dq LIT,1
ISP1:  dq ONEPLUS,TWODUP,MODF,ZEQ
       dq ZBRANCH,ISP2
       dq TWODROP,LIT,0,EXIT
ISP2:  dq TWODUP,DUPF,MULF,LTE
       dq ZBRANCH,ISP1
       dq TWODROP,LIT,-1
       dq EXIT

defcode "FIB",3,0,FFIB
        pop rcx
        call fib
        push rax
        NEXT
fib2:
        push rdi
        push rsi
        push rbx
        mov rax,1
        cmp rcx,2
        jle L1
        lea esi,[rcx-3]
        lea ebx,[rcx-1]
        xor rdi,rdi
        and rsi,1
        L3:
        mov rcx,rbx
        sub rbx,2
        call fib2
        add rdi,rax
        cmp rbx,rsi
        jne L3
        lea rax,[rdi+1]
        L1:
        pop rbx
        pop rsi
        pop rdi
        ret
fib:
    push rbx
    cmp rcx,2
    jle @f
    mov ebx,ecx
    lea ecx,[ebx-1]
    call fib
    lea ecx,[ebx-2]
    mov ebx,eax
    call fib
    add eax,ebx
    pop rbx
    ret
@@:
    mov rax,1
    pop rbx
    ret

defword "TEST",4,0,TESTF,rootlink
        dq LIT,1,DOT
        dq EXIT

defvoc "LOC-VOC",7,0,LOCVOC,0
defvoc "ROOT",4,0,ROOT,name_FORTH
defvoc "FORTH",5,0,FORTH,name_FDUMP,rootlink

defword "VOCABS",6,0,VOCABS
        dq VOCLINK
VBEG:   dq FETCH,QDUP
        dq ZBRANCH,VEND
        dq DUPF,CELLPLUS,COUNT,TYPEF,SPACE
        dq TCFA,LIT,4,CELLS,PLUS
        dq BRANCH,VBEG
VEND:   dq CR
        dq EXIT

defword "DUMP",4,0,FDUMP
; ( a u -- )
; : DUMP
; base @ >R
; 0 DO
    ; i 16 mod 0= IF cr dup i + 1 hex u.r decimal [char] : emit THEN
    ; space
    ; dup i + c@ dup 16 < IF [char] 0 emit THEN 1 hex .R decimal
    ; i 1+ 4 mod 0= IF space THEN
    ; i 1+ 16 mod 0= IF
      ; 15 0 do dup j + i + 15 - c@
      ; dup 32 128 within if emit else drop [char] . emit then loop
    ; THEN
; LOOP cr
; R> base ! DROP ;
        dq BASE,FETCH,TOR,LIT,0,TOR
        dq LIT,0,PARENSDO,DUMEX
DUM1:   dq I,LIT,16,MODF,ZEQ,ZBRANCH,DUM2
        dq CR,DUPF,I,PLUS,LIT,1,HEX,UDOTR,DECIMAL,LIT,58,EMIT
DUM2:   dq SPACE,DUPF,I,PLUS,FETCHBYTE,DUPF,LIT,16,LTF
        dq ZBRANCH,DUM3
        dq LIT,48,EMIT
DUM3:   dq LIT,1,HEX,DOTR,DECIMAL
        dq I,ONEPLUS,LIT,4,MODF,ZEQ,ZBRANCH,DUM4
        dq SPACE
DUM4:   dq I,ONEPLUS,LIT,16,MODF,ZEQ,ZBRANCH,DUM5
        dq SPACE,LIT,124,EMIT,LIT,16,LIT,0,PARENSDO,DUMEX2
DUM6:   dq DUPF,J,PLUS,I,PLUS,LIT,15,MINUS,FETCHBYTE
        dq DUPF,LIT,32,LIT,128,WITHIN,ZBRANCH,DUM7
        dq EMIT,BRANCH,DUM8
DUM7:   dq DROP,LIT,46,EMIT
DUM8:   dq PARENSLOOP,DUM6
DUMEX2:
        dq LIT,124,EMIT
DUM5:   dq PARENSLOOP,DUM1
DUMEX:  dq CR,RDROP,FROMR,BASE,FSTORE,DROP
        dq EXIT
ex_handl_end:
align 10h
expt_handler:
; RCX = ?
; RDX = ?
;  R8 = CONTEXT64
;  R9 = ?
        ;sub     rsp,8*(4+1)

;virtual at r8
;context     CONTEXT64
;end virtual

; Necessary to skip instruction causing exception. NTDLL.RtlRestoreContext will restore context after return from the exception handler.
; Opcode size is hardcoded here, but we can use routines from fdbg disasm engine to determine opcode size.
     ; add     [context.Rip],sizeog_buggy_instruction

 ; xor     r9,r9
 ;      xor     r8,r8
 ;      lea     rdx,[msg666]
 ;  xor     ecx,ecx
 ;    call    [MessageBoxA]

;if EXCEPTION_CONTINUE_SEARCH = 0
;       xor     eax,eax
;else
;     mov     eax,EXCEPTION_CONTINUE_SEARCH
;end if
;invoke ExitProcess,0
        mov eax, dword [rcx]
        push rax
        ;mov     rax, [fromError]
        lea     IP, [fromError]  ; now start FORTH from cold start
        ;mov     rbx, colde
        ;mov     IP, rbx
        NEXT

msg666         db      'An exception occured in the main application, trying to continue over it thanks to Exception Directory Entry.',0
msg_recovered  db      'The main application recovered from the exception successfully by skipping the instruction causing the exception.'
msg_empty      db      0

section '.pdata' readable writeable

data IMAGE_DIRECTORY_ENTRY_EXCEPTION
; typedef struct _RUNTIME_FUNCTION {
;     DWORD BeginAddress;
;     DWORD EndAddress;
;     DWORD UnwindData;
; } RUNTIME_FUNCTION, *PRUNTIME_FUNCTION;
; All three fields are RVAs (otherwise there wouldn't be dwords).
; BeginAddress   Points to the start address of the involved part of code.  
; EndAddress   Points to the end address of the same part of code.  
; UnwindData   Points to an UNWIND_INFO structure.   
     dd      RVA ex_handl_start
     dd      RVA ex_handl_end
     dd      RVA ex_handl_unwind
end data

ex_handl_unwind:
; The UNWIND_INFO structure tells how the portion of code should be handled. Here's the declaration I found on MSDN: 
; typedef union _UNWIND_CODE {
;     struct {
;         UBYTE CodeOffset;
;         UBYTE UnwindOp : 4;
;         UBYTE OpInfo   : 4;
;     };
;     USHORT FrameOffset;
; } UNWIND_CODE, *PUNWIND_CODE;
;
; typedef struct _UNWIND_INFO {
;     UBYTE Version       : 3;
;     UBYTE Flags         : 5;
;     UBYTE SizeOfProlog;
;     UBYTE CountOfCodes;
;     UBYTE FrameRegister : 4;
;     UBYTE FrameOffset   : 4;
;     UNWIND_CODE UnwindCode[1];
; /*  UNWIND_CODE MoreUnwindCode[((CountOfCodes + 1) & ~1) - 1];
; *   union {
; *       OPTIONAL ULONG ExceptionHandler;
; *       OPTIONAL ULONG FunctionEntry;
; *   };
; *   OPTIONAL ULONG ExceptionData[]; */
; } UNWIND_INFO, *PUNWIND_INFO;
; Here's the description of the UNWIND_INFO structure members taken directly from the MSDN:
; Version   Version number of the unwind data, currently 1.  
; Flags   Three flags are currently defined: 
; UNW_FLAG_EHANDLER The function has an exception handler that should be called when looking for functions that need to examine exceptions. 
; UNW_FLAG_UHANDLER The function has a termination handler that should be called when unwinding an exception. 
; UNW_FLAG_CHAININFO This unwind info structure is not the primary one for the procedure. Instead, the chained unwind info entry is the contents of a previous RUNTIME_FUNCTION entry. See the following text for an explanation of chained unwind info structures. If this flag is set, then the UNW_FLAG_EHANDLER and UNW_FLAG_UHANDLER flags must be cleared. Also, the frame register and fixed-stack allocation fields must have the same values as in the primary unwind info. 
; SizeOfProlog   Length of the function prolog in bytes.  
; CountOfCodes   This is the number of slots in the unwind codes array. Note that some unwind codes (for example, UWOP_SAVE_NONVOL) require more than one slot in the array.  
; FrameRegister    If nonzero, then the function uses a frame pointer, and this field is the number of the nonvolatile register used as the frame pointer, using the same encoding for the operation info field of UNWIND_CODE nodes.  
; FrameOffset    If the frame register field is nonzero, then this is the scaled offset from RSP that is applied to the FP reg when it is established. The actual FP reg is set to RSP + 16 * this number, allowing offsets from 0 to 240. This permits pointing the FP reg into the middle of the local stack allocation for dynamic stack frames, allowing better code density through shorter instructions (more instructions can use the 8-bit signed offset form).  
; UnwindCode   This is an array of items that explains the effect of the prolog on the nonvolatile registers and RSP. See the section on UNWIND_CODE for the meanings of individual items. For alignment purposes, this array will always have an even number of entries, with the final entry potentially unused (in which case the array will be one longer than indicated by the count of unwind codes field).  
; ExceptionHandler   This is an image-relative pointer to either the function's language-specific exception/termination handler (if flag UNW_FLAG_CHAININFO is clear and one of the flags UNW_FLAG_EHANDLER or UNW_FLAG_UHANDLER is set).  
; Language-specific handler data (ExceptionData)   This is the function's language-specific exception handler data. The format of this data is unspecified and completely determined by the specific exception handler in use.  
; Chained Unwind Info (ExceptionData)   If flag UNW_FLAG_CHAININFO is set then the UNWIND_INFO structure ends with three UWORDs. These UWORDs represent the RUNTIME_FUNCTION information for the function of the chained unwind.
; The possible values of the Flags field are:
; #define UNW_FLAG_EHANDLER  0x01
; #define UNW_FLAG_UHANDLER  0x02
; #define UNW_FLAG_CHAININFO 0x04
    db      19h,0,0,0
    dd      RVA expt_handler
    dd      0

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
     _gcvt,'_gcvt',\
     cos,'cos',\
     cosh,'cosh',\
     acos,'acos',\
     sin,'sin',\
     sinh,'sinh',\
     asin,'asin',\
     tan,'tan',\
     tanh,'tanh',\
     atan,'atan',\
     atan2,'atan2',\
     exp,'exp',\
     pow,'pow',\
     log,'log',\
     log10,'log10',\
     floor,'floor',\
     ceil,'ceil',\
     _fabs,'fabs',\
     puts,'puts',\
     printf,'printf'

  import ucrtbase,\
     atanh,'atanh',\
     acosh,'acosh',\
     asinh,'asinh',\
     log1p,'log1p',\
     expm1,'expm1',\
     trunc,'trunc'
     
  import shell32,\
      CommandLineToArgvW,  'CommandLineToArgvW'
