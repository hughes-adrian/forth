variable c-lib
: c-library ( a u -- )
  parse-name 2dup type cstring loadlibrary c-lib ! ;

: end-library 0 c-lib ! ;

create c-func-calls ' 0call , ' 1call , ' 2call , ' 3call , ' 4call ,

: c-function:
  parse-name header, docol ,
  parse-name cstring c-lib @ getprocaddr
  compile LIT ,
  0 begin bl parse drop c@ [char] - <> while 1+ repeat
  cells c-func-calls + @ , 
  0 begin parse-name swap drop 0<> while 1+ repeat
  0= IF compile drop THEN 
  compile exit ;

