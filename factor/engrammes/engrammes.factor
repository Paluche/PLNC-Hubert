! Copyright (C) 2014 Your name.
! See http://factorcode.org/license.txt for BSD license.
USING: math math.primes math.primes.factors kernel sequences peg.ebnf combinators ;
IN: engrammes


! This is the work I've been working all day long, it's an attempt to factorize
! the funciton >engramme. But the algorithm is worse and I think I've lost
! enough time on this exercise, I also have a project that doesn't go ahead for
! the same reasons,  I don't know the language and I lose a lot of time on
! looking on how to do what I want to do. Not succeeding and losing that much
! time just gets me angry, so before I kill someone I'll stop there.

! : compose ( primeref seq -- str )
!     over ! primeref seq primeref
!     [
!         dup             ! primeref seq seq
!         first           ! primeref seq factor
!         dup             ! primeref seq factor factor
!         first           ! primeref seq factor prime
!     ] dip               ! primeref seq factor prime primeref
!     2dup = [
!                         ! primeref seq factor prime primeref
!         2drop last nip  ! primeref exp
!         >engramme       ! primeref str
!         nip             ! str
!     ] [
!         > [
!                         ! primeref seq factor
!             3drop "0"   ! str
!         ] [
!                         ! primeref seq factor
!             drop        ! primeref seq
!             rest        ! primeref rest
!             compose     ! str
!         ]
!     ] ;
!
! : >engramme ( x -- str ) {
!     { 0 [ "0" ] }
!     { 1 [ "1" ] }
!     [
!         group-factors  ! {factSeq}
!         dup last first ! {factSeq} primeMax
!         primes-upto    ! {factSeq} {primeSeq}
!         swap           ! {primeSeq} {factSeq}
!         ! For each term of PrimeSeq, if the first first term of the seq is the
!         ! equal to the current prime number, we launch >engramme on it then we
!         ! remove the head of seq and we append the result string to the current one
!         ! else we had "0" to the current number, and we do it with the next number.
!
!         [
!             over ! primeref factSeq primeref
!             [
!                 dup             ! primeref seq seq
!                 first           ! primeref seq factor
!                 dup             ! primeref seq factor factor
!                 first           ! primeref seq factor prime
!             ] dip               ! primeref seq factor prime primeref
!             2dup = [
!                                 ! primeref seq factor prime primeref
!                 2drop last nip  ! primeref exp
!                 >engramme       ! primeref str
!                 nip             ! str
!             ] [
!                 > [
!                                 ! primeref seq factor
!                     3drop "0"   ! str
!                 ] [
!                                 ! primeref seq factor
!                     drop        ! primeref seq
!                     rest        ! primeref rest
!                     compose     ! str
!                 ]
!             ]
!         ] curry
!
!         [ append ]
!
!         map-reduce
!
!         ! Add parenthesis
!         nip "(" ")" surround
!     ]
! } case ;
<PRIVATE
: engrammefill ( str start end  -- newstart str )
    2dup  ! str start end start end
    =     ! str start end start=end
    not   ! str start end start!=end
    [
        "0"             ! str start end "0"
        pick next-prime ! str start end "0" start+1
        pick            ! str start end "0" start+1 end
        engrammefill    ! str start end newstart "0"++
        swapd nip       ! str start newstart "0"++
        swapd nip       ! str end "0"++
        swap            ! str "0"++ end
        [ append ] dip  ! str"0"++ end
        swap            ! end str"0"++
    ] [
        ! start = end
        drop next-prime swap
    ] if ;

: seq>engramme ( str start seq -- str )
    dup    ! str start seq seq
    empty? ! str start seq bool
    [
        ! drop the variables use for the function
        2drop
        ! Add parenthesis
        "(" ")" surround
    ] [
        ! Generate the number
        ! Retrieve the prime number and it exposant we will use
        unclip     ! str start rest first

        ! We must fill with zeros the positions that are not used
        rot          ! str rest first start
        "" swap      ! str rest first "" start
        pick         ! str rest first "" start first
        first        ! str rest first "" start end
        engrammefill ! str rest first newstart str2

        ! Now we must determinate the number to put inside the parenthesis
        rot     ! str rest newstart str2 first
        last    ! str rest newstart str2 pwr

        ! We must not put parenthesis if the number is 1
        dup 1 = [
            drop "1"
        ] [
            ! We cannot use the function >engramme since it's not yet defined
            group-factors "" swap 2 swap
            seq>engramme
        ] if            ! str rest newstart str2 str3
        append          ! str rest newstart str2++
        -rot            ! str str2++ rest newstart
        [ append ] 2dip ! str++ rest newstart
        swap            ! str++ newstart rest
        seq>engramme    ! (str+++)
    ] if ;
PRIVATE>

: >engramme ( x -- str ) {
    { 0 [ "0" ] }
    { 1 [ "1" ] }
    [
      ! Generate the arguments
        group-factors "" swap 2 swap
        seq>engramme
    ]
} case ;

: <engramme ( str -- x )
{
    { "0" [ 0 ] }
    { "1" [ 1 ] }
    [
        ! TODO
        ! I need to parse the string by location
        ! example (1110(110)) => { "1" "1" "1" "0" "110" }
    ! And for each part of the vector we apply
        drop "COmplex number"
        ]
} case ;
