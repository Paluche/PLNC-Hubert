! Copyright (C) 2014 Your name.
! See http://factorcode.org/license.txt for BSD license.
USING: math math.primes math.primes.factors kernel sequences ;
IN: engrammes

! TODO rot and -rot words are deprecated.

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


: >engramme ( x -- str )
    dup 2 < [
        1 = [
            "1"
        ] [
            "0"
        ] if
    ] [
        ! Generate the arguments
        group-factors "" swap 2 swap
        seq>engramme
    ] if ;
