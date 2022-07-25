: fib dup 2 <= if drop 1 exit then dup 2 - recurse swap 1- recurse + ;

: doit 100 0 do 31 fib cr . loop ;

doit bye
