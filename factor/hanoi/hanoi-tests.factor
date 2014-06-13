! Copyright (C) 2014 Your name.
! See http://factorcode.org/license.txt for BSD license.
USING: hanoi kernel io.streams.string tools.test ;
IN: hanoi.tests

! <PRIVATE
! [ t ] [ 1 2 move "1 vers 2" = ] unit-test
! [ t ] [ 0 1 move "0 vers 1" = ] unit-test
! [ t ] [ 2 -1 move "2 vers -1" = ] unit-test
! [ t ] [ 3 -2 move "3 vers -2" = ] unit-test
!
!
! [ t ] [ 1 2 other 3 = ] unit-test
! [ t ] [ 2 3 other 1 = ] unit-test
! [ t ] [ 1 3 other 2 = ] unit-test
! [ t ] [ 2 1 other 3 = ] unit-test
! [ t ] [ 3 2 other 1 = ] unit-test
! [ t ] [ 3 1 other 2 = ] unit-test
!
! [ t ] [ 1 2 partial 3 = swap 1 = and ] unit-test
! [ t ] [ 1 3 partial 2 = swap 1 = and ] unit-test
! [ t ] [ 2 3 partial 1 = swap 2 = and ] unit-test
!
! PRIVATE>

[ "1 vers 3\n1 vers 2\n3 vers 2\n1 vers 3\n2 vers 1\n2 vers 3\n1 vers 3\n" ]
    [ [ 1 3 3 hanoi ] with-string-writer ] unit-test
[ "1 vers 2\n1 vers 3\n2 vers 3\n" ]
    [ [ 1 3 2 hanoi ] with-string-writer ] unit-test
[ "1 vers 3\n" ] [ [ 1 3 1 hanoi ] with-string-writer ] unit-test
