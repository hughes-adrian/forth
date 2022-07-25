variable CURRENT
16 constant #VOCS
variable CONTEXT #vocs cells allot
variable voc-link 0 voc-link !

: vocabulary
    here dup
    create
    0 ,      \ for the linked list
    , \ save the link addr
    voc-link @ ,
    voc-link !
    does> context ! ;

: definitions
    context @
    current ! ;

vocabulary root
vocabulary forth
vocabulary poo

: also 1 #vocs 2 - do context i cells + dup cell- @ swap !  -1 +loop ;

: only #vocs 0 do 0 context i cells + ! loop
  \ root context @ context #vocs 1- cells + ! 
  root also forth definitions root ;


: order cr ." context: " #vocs 0 do context i cells + @
 ?dup if cell+ @ cell+ count type space then loop 
 cr ." current: " current @ 1 cells + @ cell+ count type cr ;

: print-vocs voc-link 
    begin 
      @ ?dup
    while 
      dup cell+ count type space
      >CFA 4 cells + 
    repeat 
    cr ; 
 
 only
 poo also
 forth definitions
 \ also

  \ order cr
  print-vocs
cr

 ' root 3 cells + .

\ bye
