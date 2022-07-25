s" a(b|c)*d"
dup create re 1+ allot align
re pack$ count 
." re=" type

variable natom    0 natom !
variable nalt     0 nalt  !
variable pbuf     0 pbuf !
128 buffer: buf align

: pstack create 2* cells allot does> swap 2* cells + ;

: inc ( a -- ) 1 swap +! ;
: dec ( a -- ) -1 swap +! ;
: store-char buf pbuf @ + c! pbuf inc ;

128 pstack p 
variable pp   0 pp !
: p->nalt  pp @ p ;
: p->natom pp @ p cell+ ;
: nest nalt @ p->nalt !   \ push nalt and natom on stack
       natom @ p->natom ! \ and reset
       pp inc
       0 nalt ! 0 natom ! ;
: unnest pp dec           \ pop old values from stack
       p->nalt @ nalt !
       p->natom @ natom ! ;
: cat-atoms  \ output a . for each atom left in queue
    begin
      natom dec natom @ 0>
    while
      [char] . store-char
    repeat ;
: cat-unions   \ output a | for each union in queue
    begin 
      nalt @ 0>
    while
      [char] | store-char
      nalt dec
    repeat ;

: re2post ( a u -- a u ) \ convert to postfix
0 pbuf !
0 pp !
over + swap
do  \ loop over string - a to a+u
  i c@
case
  [char] ( of
    natom @ 1 > if natom dec [char] . store-char then
    nest
  endof

  [char] ) of
    cat-atoms
    cat-unions
    unnest
    natom inc
  endof

  [char] | of 
    cat-atoms
    nalt inc
  endof

  [char] * of
    i c@ store-char
  endof 
  [char] + of
    i c@ store-char
  endof
  [char] ? of
    i c@ store-char
  endof

  natom @ 1 > if natom dec [char] . store-char then 
  i c@ store-char
  natom inc
endcase
   
loop 

cat-atoms
cat-unions
;

256 value match
257 value split
variable nstate    0 nstate !
: state nstate inc here , , , , ; \ c, out, out1, lastlist
: frag here , , ; \ start, outlist
: list1 ( **outp -- *ptrlist ) dup 0 swap ! ;
: append ( *l1 *l2 -- *l )
  swap dup >r 
  begin
    dup @ 
  while
    @
  repeat 
   ! r> ;
: patch ( *ptrlist *state -- ) 
  >r
  begin
    dup @ 
  while
    dup @
    swap r@ swap !
  repeat rdrop ;

match 0 0 0 state value matchstate

cr re count type

cr re count re2post 
buf pbuf @ type
\ cr bye


