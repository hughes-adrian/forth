create loc-dict 128 cells allot
create loc-stack 128 cells allot
variable loc-sp loc-stack 128 cells + loc-sp !
loc-dict 128 cells 0 fill
loc-stack 28 cells 0 fill
variable loc-cnt 0 loc-cnt !
variable loc-offset 0 loc-offset !
variable loc-flg false loc-flg !
variable old-dp

: make-loc-voc
 loc-dict here create
 0 , , voc-link @ , voc-link !
 does> context ! ;

make-loc-voc loc-voc

: @loc0 loc-sp @ 0 cells + @ ; 
: @loc1 loc-sp @ 1 cells + @ ; 
: @loc2 loc-sp @ 2 cells + @ ; 
: @loc3 loc-sp @ 3 cells + @ ; 

: lp-1 1 cells loc-sp +! ; 
: lp-2 2 cells loc-sp +! ; 
: lp-3 3 cells loc-sp +! ; 
: lp-4 4 cells loc-sp +! ; 
\ : print-stack loc-sp 64 - loc-sp 1- do i @ . -8 +loop cr ;
: >l -8 loc-sp +! loc-sp @ ! ;
: @loc loc-sp @ @ ; \ loc-sp @ cells + @ ;
\ : <l 8 loc-sp @ +! ;
: reset-locals
   loc-flg @ if
     loc-cnt @ \ 0 ?do compile <l loop 
     case
       1 OF compile lp-1 ENDOF
       2 OF compile lp-2 ENDOF
       3 OF compile lp-3 ENDOF
       4 OF compile lp-4 ENDOF
     endcase
     0 loc-cnt !
     0 loc-offset !
     false loc-flg !
     previous definitions
     current @ @ latest !
   then
 ; immediate

: start-locals
  true loc-flg !
  here old-dp !
  loc-dict dp !
  also loc-voc definitions 
  0 dup current @ ! latest !
;
: stop-locals
   old-dp @ dp !
   \ also loc-voc 
   ." cnt = " loc-cnt @ .
   loc-cnt @ 0 ?do compile >l loop 
;


: create-local ( a u -- )
\  header,  DOCOL , compile @loc compile exit ;
  header, DOCOL , 
\   compile @loc
  loc-offset @ CASE
    0 OF compile @loc0 1 loc-offset +! ENDOF
    1 OF compile @loc1 1 loc-offset +! ENDOF
    2 OF compile @loc2 1 loc-offset +! ENDOF
    3 OF compile @loc3 1 loc-offset +! ENDOF
  ENDCASE
  compile EXIT ;



: (local) 
 2dup 0. D= IF 2DROP stop-locals EXIT THEN
 loc-flg @ not IF 
   \  ." first local " 
     start-locals
 THEN
 \ 2dup ." creating: " type space
\ 1 loc-cnt +!
 create-local ;

: local bl word count 2dup type space loc-cnt @ .  (local) ;

: {helper
  >in @ bl word count 1 =
  swap c@ [char] } = and
  IF
    drop >in @
  ELSE
    1 loc-cnt +!
    recurse
    swap >in ! local
  THEN ;

: { {helper >in ! 0 0 (local) ; immediate
 
\ : ; reset-locals ' ; , ] ; immediate 
: f { a b c } a b c . . . reset-locals ;
2 value foo
: ff foo { foo } foo . .  reset-locals ;
." compiled" cr
\ print-stack cr 
1 >l
2 >l
\ loc-stack 28 cells dump
loc-sp @ @ .
loc-sp @ cell+ @ .
loc-sp @ hex . decimal
 see f
 see ff
100 200 300 f 
666 ff
 cr bye
