! Copyright (C) 2014 Your name.
! See http://factorcode.org/license.txt for BSD license.
USING: fry kernel io math math.parser sequences ;
IN: hanoi

<PRIVATE

: move ( a b -- str ) [ number>string ] bi@ " vers " glue ;

: other ( a b -- o )  bitxor ;

: partial ( a b -- a b' ) over other  ;

PRIVATE>

: hanoi ( d a n -- )
    dup 1 = [
        drop move print
    ] [
        3dup 1 - 2over partial pick
        hanoi
        drop move print
        1 - -rot swap partial swap rot
        hanoi
    ] if ;
