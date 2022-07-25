
DECIMAL

CREATE FLAGS 8190 ALLOT ALIGN
variable eflag

: PRIMES  ( -- n )  FLAGS 8190 1 FILL  0 3  EFLAG @ FLAGS
  DO   I C@
       IF  DUP I + DUP EFLAG @ <
           IF    EFLAG @ SWAP
                 DO  0 I C! DUP  +LOOP
           ELSE  DROP  THEN  SWAP 1+ SWAP
           THEN  2 +
       LOOP  DROP ;

: BENCHMARK  0 1000 0 DO  PRIMES NIP  LOOP ;

: main 
 FLAGS 8190 + eflag !
 benchmark ( . ) drop
;
