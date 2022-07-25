: ':' [char] : ;
: '"' [char] " ;
: ';' [char] ; ;

: ID.
 cell +  ( skip over the link pointer )
 DUP C@  ( get the flags/length byte )
 F_LENMASK AND ( mask out the flags - just want the length )

 BEGIN
  DUP 0>  ( length > 0? )
 WHILE
  SWAP 1+  ( addr len -- len addr+1 )
  DUP C@  ( len addr -- len addr char | get the next character)
  EMIT  ( len addr char -- len addr | and print it)
  SWAP 1-  ( len addr -- addr len-1    | subtract one from length )
 REPEAT
 2DROP  ( len addr -- )
;

: done ." here "; immediate

: SEE
 BL WORD FIND ( find the dictionary entry to decompile )
 DROP CFA>
 \ Now we search again, looking for the next word in the dictionary.  
 \ This gives us
 \  the length of the word that we will be decompiling.  
 \ (Well, mostly it does).
 DP @  \ ( address of the end of the last compiled word )
 LATEST @ ( word last curr )
 BEGIN
  2 PICK  ( word last curr word )
  OVER  ( word last curr word curr )
  <>  ( word last curr word<>curr? )
 WHILE   ( word last curr )
  NIP  ( word curr )
  DUP @  ( word curr prev which becomes: word last curr )
 REPEAT
 DROP  ( at this point, the stack is: start-of-word end-of-word )
 SWAP  ( end-of-word start-of-word )
 cr ( begin the definition with : NAME [IMMEDIATE] )
 ':' EMIT SPACE DUP ID. SPACE
 DUP IMMEDIATE? IF ." IMMEDIATE " THEN

 >DFA \ ( get the data address, ie. points after DOCOL | 
 \ end-of-word start-of-data )

 ( now we start decompiling until we hit the end of the word )
 BEGIN  ( end start )
  2DUP >
 WHILE
  DUP @  ( end start codeword )

  CASE
  ['] LIT OF  ( is it LIT ? )
   CELL + DUP @  ( get next word which is the integer constant )
   .   ( and print it )
  ENDOF
  ['] LITSTRING OF  ( is it LITSTRING ? )
   [ CHAR S ] LITERAL EMIT '"' EMIT SPACE ( print S"<space> )
   CELL + DUP @  ( get the length word )
   SWAP CELL + SWAP  ( end start+4 length )
   2DUP TYPE  ( print the string )
   '"' EMIT SPACE  ( finish the string with a final quote )
   + ALIGNED  ( end start+4+len, aligned )
   CELL -   ( because we're about to add 4 below )
  ENDOF
  ['] 0BRANCH OF  ( is it 0BRANCH ? )
   ." 0BRANCH ( "
   CELL + DUP @  ( print the offset )
   .
   ." ) "
  ENDOF
  ['] BRANCH OF  ( is it BRANCH ? )
   ." BRANCH ( "
   CELL + DUP @  ( print the offset )
   .
   ." ) "
  ENDOF
  ['] ['] OF   ( is it ' TICK ? )
   [ CHAR ['] ] LITERAL EMIT SPACE
   CELL + DUP @  ( get the next codeword )
   CFA>   ( and force it to be printed as a dictionary entry )
   ID. SPACE
  ENDOF
  ['] EXIT OF  ( is it EXIT? )
   \ We expect the last word to be EXIT, and if it is 
   \ then we don't print it
   \  because EXIT is normally implied by ;. 
   \   EXIT can also appear in the middle
   \  of words, and then it needs to be printed. )
   2DUP   ( end start end start )
   CELL +   ( end start end start+4 )
   <> IF   ( end start | we're not at the end )
    ." EXIT "
   THEN
  ENDOF
     ( default case: )
   DUP   ( in the default case we always need to DUP before using )
   CFA>   ( look up the codeword to get the dictionary entry )
   ID. SPACE  ( and print it )
  ENDCASE

  CELL +  ( end start+4 )
 REPEAT

 ';' EMIT CR

 2DROP  ( restore stack )
;