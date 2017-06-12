*+RD_REAL
        subroutine RD_REAL( string, value, flag )

        implicit none

        character*( * ) string
        real value
        integer flag

*       Description
*         Given a string that contains a number in I, F or E format,
*         this subroutine reads the number out as real number
*
*       Arguments
*         string        (i) : Input string
*         value         (o) : Real number
*         flag          (o) : negative if error occured;
*                             1 if integer is read,
*                             2 if F format real is read
*                             3 if E format real is read
*
*       Dependencies
*        None
*
*       Origin
*         Created by the KM, who feels he's reinventing the wheel.
*
*       Author
*         Koji Mukai     (1993 Jan 27), original version
*         Koji Mukai     1993 March, corrected a bug for E format without "."
*-RD_REAL
        integer total, dot, expnt, l, beg, int1, m, n
        character one
        character*8 formt

        integer LENTRIM

        flag = 0

*       First of all, find out the positions of the first and the last
*       non-blank characters in the string
        total = LENTRIM( string )
        if( total .lt. 0 ) then
          flag = -1
          return
        end if
        l = 1
        one = string( l: l )
        do while( one .le. ' ' )
          l = l + 1
          one = string( l: l )
        end do
        beg = l

*       Then start from the last character, see if the string is
*       consistent with an integer
        dot = 0
        expnt = 0
        l = total
        one = string( l: l )
        if( one .eq. '.' ) goto 100
        if( one .lt. '0' .or. one .gt. '9' ) goto 300
        l = l - 1
        do while( l .ge. beg )
          one = string( l: l )
          if( one .eq. '.' ) then
            goto 100
          else if( one .eq. '-' .or. one .eq. '+' ) then
            l = l - 1
            if( l .ge. beg ) then
              one = string( l: l )
              if( one .eq. 'e' .or. one .eq. 'E'
     &                .or. one .eq. 'd' .or. one .eq. 'D' ) then
                goto 200
              else
                goto 300
              end if
            end if
          else if( one .eq. 'e' .or. one .eq. 'E'
     &                .or. one .eq. 'd' .or. one .eq. 'D' ) then
            goto 200
          else if( one .lt. '0' .or. one .gt. '9' ) then
            goto 300
          end if
          l = l - 1
        end do

*       The string indeed appears to be an integer.  Let's read it.
        if( total - beg + 1 .ge. 10 ) then
          write( formt, '(''(i'',i2,'')'')' ) total - beg + 1
        else
          write( formt, '(''(i'',i1,'')'')' ) total - beg + 1
        end if
*        print *, 'Integer found, format should be ', formt
        read( string( beg: total ), formt, err = 400 ) int1
        value = real( int1 )
        flag = 1
        return

100     continue
*       A '.' is found in the string.  See if this is consistent with being
*       an F format
        dot = l
        l = l - 1
        do while( l .gt. beg )
          one = string( l: l )
          if( one .lt. '0' .or. one .gt. '9' ) goto 300
          l = l - 1
        end do
*         (first letter can be the sign, otherwise any non-digits means error)
        one = string( beg: beg )
        if( one .ne. '-' .and. one .ne. '+' .and. one .ne. '.'
     &                   .and. ( one .lt. '0' .or. one .gt. '9' ) ) then
          goto 300
        end if
        
*       Found an F format real, let's read it.
        m = total - beg + 1
        n = total - dot
        if( m .ge. 10 ) then
          if( n .ge. 10 ) then
            write( formt, '(''(f'',i2,''.'',i2,'')'')' ) m, n
          else
            write( formt, '(''(f'',i2,''.'',i1,'')'')' ) m, n
          end if
        else
          write( formt, '(''(f'',i1,''.'',i1,'')'')' ) m, n
        end if
*        print *, 'F-float found, format should be ', formt
        read( string( beg: total ), formt, err = 400 ) value
        flag = 2
        return

200     continue
*       Exponent-looking structure found, see if it's an E or D format
*       real number.
        expnt = l
        l = l - 1
        do while( l .gt. beg )
          one = string( l: l )
          if( one .eq. '.' ) then
            if( dot .ne. 0 ) goto 300
*                (that was the second '.' --- illegal!)
            dot = l
          else if( one .lt. '0' .or. one .gt. '9' ) then
            goto 300
          end if
          l = l - 1
        end do
        one = string( l: l )
        if( one .eq. '.' ) then
          if( dot .ne. 0 ) goto 300
          dot = l
        else if( ( one .lt. '0' .and. ( one .ne. '-' .and. one .ne. '+'
     &                ) ) .or. one .gt. '9' ) then
          goto 300
        end if

*       E format real number found.  Let's read it
        m = total - beg + 1
        if( dot .eq. 0 ) then
          n = 0
        else
          n = expnt - dot - 1
        end if
        if( m .ge. 10 ) then
          if( n .ge. 10 ) then
            write( formt, '(''(e'',i2,''.'',i2,'')'')' ) m, n
          else
            write( formt, '(''(e'',i2,''.'',i1,'')'')' ) m, n
          end if
        else
          write( formt, '(''(e'',i1,''.'',i1,'')'')' ) m, n
        end if
*        print *, 'E-float found, format should be ', formt
        read( string( beg: total ), formt, err = 400 ) value
        flag = 3
        return

300     continue
*       Non-conforming character found in string
        flag = -1
        return

400     continue
*       No strange characters found, but still error condition
*       occurred during actual read --- maybe too big?
        flag = -2
        return

        end
