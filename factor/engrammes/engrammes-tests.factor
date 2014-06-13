! Copyright (C) 2014 Your name.
! See http://factorcode.org/license.txt for BSD license.
USING: engrammes kernel io.streams.string tools.test ;
IN: engrammes.tests

! [ 3  ""       ] [ "" 2  2 engrammefill ] unit-test
! [ 5  "0"      ] [ "" 2  3 engrammefill ] unit-test
! [ 7  "00"     ] [ "" 2  5 engrammefill ] unit-test
! [ 11 "000"    ] [ "" 2  7 engrammefill ] unit-test
! [ 13 "0000"   ] [ "" 2 11 engrammefill ] unit-test
! [ 17 "00000"  ] [ "" 2 13 engrammefill ] unit-test
! [ 19 "000000" ] [ "" 2 17 engrammefill ] unit-test


[ "0"          ] [ 0    >engramme ] unit-test
[ "1"          ] [ 1    >engramme ] unit-test
[ "(1)"        ] [ 2    >engramme ] unit-test
[ "(01)"       ] [ 3    >engramme ] unit-test
[ "((1))"      ] [ 4    >engramme ] unit-test
[ "(001)"      ] [ 5    >engramme ] unit-test
[ "(11)"       ] [ 6    >engramme ] unit-test
[ "(0001)"     ] [ 7    >engramme ] unit-test
[ "((01))"     ] [ 8    >engramme ] unit-test
[ "(0(1))"     ] [ 9    >engramme ] unit-test
[ "(101)"      ] [ 10   >engramme ] unit-test
[ "((1)(1))"   ] [ 36   >engramme ] unit-test
[ "((11))"     ] [ 64   >engramme ] unit-test
[ "((1)(1)101)" ] [ 1980 >engramme ] unit-test
[ "(0010000000000000000000000000000000000000000000000000000000000000000000000000001)"]
    [ 2005 >engramme ] unit-test
