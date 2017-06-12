*+RD_ASTRO
        subroutine RD_ASTRO( in_line, code, value, flag )

        implicit none

        character*( * ) in_line
        character code
        double precision value
        integer flag

*       Description:
*         RD_ASTRO reads astronomical coordinates into double precision VALUE
*         accepting character*(*) in_line containing either nn,mm,ss.s
*         (or nn mm ss.s or similar variations thereof; hms format)
*         or dd.ddd (decimal degree format)
*         for code=R (for RA, in which case dd.ddd would be divided by 15)
*         and code=D (for DEC), with appropriate checking of ranges.
*
*       Arguments:
*         in_line  (i) : Input string
*         code     (i) : Input flag R (RA) or D (Dec)
*         value    (o) : Double precision value
*         flag     (o) : 0 if no errors
*
*       Dependencies:
*         Uses LENTRIM for string length
*
*       Origin:
*         Created by KM for use with varios practical astronomy routines
*
*       Author:
*         Koji Mukai, 1993 Feb 03, First official version
*         Koji Mukai, 1993 Feb 24, Now allowing hh mm.m format
*-RD_ASTRO

        logical fiften, on, dot, minus, comma
        character*6 formt
        character one
        integer len, lbeg, l, k
        integer beg( 3 ), fin( 3 ), done( 3 )
        integer hh, mm, ssint
        real realnm, ss, temp

        integer LENTRIM

        flag = 0

        if( code .eq. 'R' .or. code .eq. 'r' ) then
          fiften = .true.
        else if( code .eq. 'D' .or. code .eq. 'd' ) then
          fiften = .false.
        else
          flag = -99
          goto 900
        end if

*       Find out effective length of the input line
        len = LENTRIM( in_line )
        if( len .eq. 0 ) then
          flag = -1
          goto 900
        end if

        l = 1
        do while( in_line( l: l ) .le. ' ' )
          l = l + 1
        end do

        lbeg = l
        do k = 1, 3
          beg( k ) = 0
          fin( k ) = 0
          done( k ) = 0
        end do
        k = 1
        on = .false.
        dot = .false.
        minus = .false.
        comma = .false.
        do l = lbeg, len
          one = in_line( l: l )
          if( one .eq. '-' ) then
            if( on ) then
              flag = -2
              goto 900
            end if
            minus = .true.
            on = .true.
            beg( k ) = l + 1
            done( k ) = 1
          else if( one .eq. '+' ) then
            if( on .or. fiften ) then
              flag = -2
              goto 900
            end if
            minus = .false.
            on = .true.
            beg( k ) = l + 1
            done( k ) = 1
          else if( one .eq. '.' ) then
            if( dot ) then
              flag = -3
              goto 900
            end if
            if( .not. on ) then
              on = .true.
              beg( k ) = l
            end if
            dot = .true.
            done( k ) = 2
          else if( one .ge. '0' .and. one .le. '9' ) then
            if( .not. on ) then
              on = .true.
              beg( k ) = l
              done( k ) = 1
            end if
          else if( one .eq. ',' ) then
            if( on ) then
              fin( k ) = l - 1
              k = k + 1
              if( k .eq. 4 ) then
                flag = -6
                goto 900
              end if
              on = .false.
              dot = .false.
            else
              if( comma ) then
                flag = -4
                goto 900
              end if
            end if
            comma = .true.
          else if( one .eq. ' ' ) then
            if( on ) then
              fin( k ) = l - 1
              k = k + 1
              if( k .eq. 4 ) then
                flag = -6
                goto 900
              end if
              on = .false.
              comma = .false.
              dot = .false.
            end if
          else
            flag = -5
            goto 900
          end if
        end do
        if( .not. on ) then
          flag = -7
          goto 900
*        else if( k .eq. 2 ) then
*          flag = -8
*          goto 900
        end if
        fin( k ) = len

        if( k .eq. 1 ) then
*                           ! dd.ddd format
          if( fin( 1 ) - beg( 1 ) .ge. 10 ) then
            flag = -9
            goto 900
          else if( done( 1 ) .eq. 1 ) then
            flag = -12
            goto 900
          end if
          write( formt, '(''(f'',i1,''.1)'')' ) fin( 1 ) - beg( 1 ) + 1
          read( in_line( beg( 1 ): fin( 1 ) ), formt ) realnm
          if( fiften ) then
            if( minus ) then
              flag = -10
              goto 900
            else if( realnm .gt. 360.0 ) then
              flag = -10
              goto 900
            end if
            value = DBLE( realnm / 15.0 )
          else
            if( minus ) realnm = -realnm
            if( realnm .lt. -90.0 .or. realnm .gt. +90.0 ) then
              flag = -10
              goto 900
            end if
            value = DBLE( realnm )
          end if
        else
*            !HH MM SS.S or HH MM.M format
          if( done( 1 ) .eq. 2 ) then
            flag = -11
            goto 900
          else if( fin( 1 ) - beg( 1 ) .gt. 1 ) then
            flag = -9
            goto 900
          end if
          write( formt, '(''(i'',i1,'')'')' ) fin( 1 ) - beg( 1 ) + 1
          read( in_line( beg( 1 ): fin( 1 ) ), formt ) hh

          if( k .eq. 2 ) then
*             ! HH MM.M format
            if( done( 2 ) .eq. 1 ) then
              if( fin( 2 ) - beg( 2 ) .gt. 1 ) then
                flag = -9
                goto 900
              end if
              write( formt, '(''(i'',i1,'')'')' )
     &                                        fin( 2 ) - beg( 2 ) + 1
              read( in_line( beg( 2 ): fin( 2 ) ), formt ) ssint
              ss = REAL( ssint )
            else
              if( fin( 2 ) - beg( 2 ) .ge. 10 ) then
                flag = -9
                goto 900
              end if
              write( formt, '(''(f'',i1,''.1)'')' )
     &                                       fin( 2 ) - beg( 2 ) + 1
              read( in_line( beg( 2 ): fin( 2 ) ), formt ) ss
            end if
            if( ss .lt. 0.0 .or. ss .ge. 60.0 ) then
              flag = -10
              goto 900
            end if

            temp = real( hh ) + ss / 60.0

          else
*             ! HH MM SS.S format
            if( done( 2 ) .eq. 2 ) then
              flag = -11
              goto 900
            else if( fin( 2 ) - beg( 2 ) .gt. 1 ) then
              flag = -9
              goto 900
            end if
            write( formt, '(''(i'',i1,'')'')' ) fin( 2 ) - beg( 2 ) + 1
            read( in_line( beg( 2 ): fin( 2 ) ), formt ) mm
            if( mm .lt. 0 .or. mm .ge. 60 ) then
              flag = -10
              goto 900
            end if

            if( done( 3 ) .eq. 1 ) then
              if( fin( 3 ) - beg( 3 ) .gt. 1 ) then
                flag = -9
                goto 900
              end if
              write( formt, '(''(i'',i1,'')'')' )
     &                                        fin( 3 ) - beg( 3 ) + 1
              read( in_line( beg( 3 ): fin( 3 ) ), formt ) ssint
              ss = REAL( ssint )
            else
              if( fin( 3 ) - beg( 3 ) .ge. 10 ) then
                flag = -9
                goto 900
              end if
              write( formt, '(''(f'',i1,''.1)'')' )
     &                                       fin( 3 ) - beg( 3 ) + 1
              read( in_line( beg( 3 ): fin( 3 ) ), formt ) ss
            end if
            if( ss .lt. 0.0 .or. ss .ge. 60.0 ) then
              flag = -10
              goto 900
            end if

            temp = real( hh ) + ( real( mm ) + ss / 60.0 ) / 60.0
          end if
*         Both HH MM.M and HH MM SS.S join here.

          if( fiften ) then
            if( minus ) then
              flag = -10
              goto 900
            else
              if( temp .gt. 24.0 ) then
                flag = -10
                goto 900
              end if
            end if
            value = DBLE( temp )
          else
            if( minus ) temp = -temp
            if( temp .lt. -90.0 .or. temp .gt. +90.0 ) then
              flag = -10
              goto 900
            end if
            value = DBLE( temp )
          end if
        end if

 900    continue

* FLAG<0 signifies error conditions.
* -1: empty string; -2: "-" out of place, or "+" in RA;
* -3: too many "." in one number;
* -4: too many "," all at once; -5: unrecognized character;
* -6: too many numbers; -7: string ends with a ",";
* -9: number string too long; -10: value out of range;
* -11: real found where integer is expected; -12: integer found, real expected;
* -99: unknown code

        end
