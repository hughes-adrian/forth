: DEFER ( "name" -- )
 CREATE ['] ABORT ,
 DOES> ( ... -- ... )
 @ EXECUTE ;


: DEFER! ( xt2 xt1 -- )
   >BODY ! ;

: IS
 STATE @ IF
   POSTPONE ['] POSTPONE DEFER!
 ELSE
   ' DEFER!
 THEN ; IMMEDIATE

: :NONAME ( i*x -- i*x )
  0 0 HEADER,
  HERE DOCOL , ] ;


DEFER print
:NONAME ( n -- ) . ; IS print

