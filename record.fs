variable total.bytes
0 total.bytes !

: field 
    create
      total.bytes @ ,
      total.bytes +! 
    immediate
    does> @ +
         state @ if
             postpone literal
         then
;

: make.instance create allot immediate ;
: defines-type 
      create
        total.bytes @ , 
        0 total.bytes !
  does> @ make.instance ;

cell field .real
cell field .imag
defines-type complex 

complex x

: initialize 0 x .real ! 3 x .imag ! ;
\ see initialize
 initialize 
 x @ .
 x cell+ @ .
 x .imag @ .
cr  bye
