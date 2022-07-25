: f>s f>d d>s ;
: isprime ( n -- f )
 dup s>f fsqrt f>s
 1+ 2 do
   dup i mod 0= 
   if drop 0 unloop exit then
 loop
 drop -1
;

1000000 1 - constant m
variable lastfound

: main
  0 2 begin
    dup isprime
    if swap 1+ swap dup lastfound ! then
    over m <
  while
    1+
  repeat
  lastfound @ . 2drop ;

\ utime
main 
\ utime 2swap d- d>f 1000000e f/ f.
\ bye
