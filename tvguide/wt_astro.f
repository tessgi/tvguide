*+WT_ASTRO
        subroutine WT_ASTRO( value, code, out_line, flag )

        implicit none

        double precision value
        integer code
        character*( * ) out_line
        integer flag

*       Description:
*         WT_ASTRO reads astronomical coordinates from double precision VALUE
*         writing to character*(*) out_line containing in nn mm ss.s format.
*         Does not care if it's RA or DEC, since it does not deal with decimal
*         degrees, unlike the closely related RD_ASTRO routine.
*
*       Arguments:
*         value    (i) : Input double precision value
*         code     (i) : Number of digits, plus if there is to be a sign.
*                        +4 means hh mm;      -4 means sdd mm
*                        +5 means hh mm.m;    -5 means sdd mm.m
*                        +6 means hh mm ss;   -6 means sdd mm ss
*                        +7 means hh mm ss.s; -7 means sdd mm ss.s
*         out_line (o) : Output string
*         flag     (o) : positive if no errors, in which case
*                        returned value is the actual length of string;
*                        negative values indicate errors
*
*       Dependencies:
*         None
*
*       Origin:
*         Created by KM for use with varios practical astronomy routines
*
*       Author:
*         Koji Mukai, 1993 Feb 09, original version
*         Koji Mukai, 1993 Feb 25, Now behaves correctly when last
*                                  quantity, written as real, is rounded
*                                  up to 60.0
*-WT_ASTRO

        double precision value2
        integer last, n_digit, hh, mm, labour, lngth, s_len
        character*6 formt

*       First, check code and output string length; deal with sign if necessary
        lngth = LEN( out_line )
        n_digit = abs( code )
        if( n_digit .le. 3 ) then
          flag = -1
*           too few digits
          goto 900
        else if( n_digit .gt. 9 ) then
          flag = -2
*           too many digits
          goto 900
        end if
        if( code .lt. 0 ) then
*         sign requested
          if( value .ge. 0.0d+00 ) then
            out_line = '+'
          else
            out_line = '-'
          end if
          last = 1
        else
          out_line = ' '
          last = 0
        end if
        if( n_digit .le. 4 ) then
*         format nn mm, possibly with sign, which we can account for using last
          flag = last + n_digit + 1
        else if( n_digit .le. 6 ) then
*         format nn mm.m or nn mm ss, 2 (+last) places more required than
*         n_digit (now you see why I used that funny form in the n_digit=4 case)
          flag = last + n_digit + 2
        else 
*         format nn mm ss.ssssssss
          flag = last + n_digit + 3
        end if
        if( lngth .lt. flag ) then
          flag = -3
*           Output string too short
          goto 900
        end if
*       flag now is the actual number of characters to be used
        value2 = abs( value )

*       Now do dd/hh; since abs(code) is restricted, this is always an integer
        hh = int( value2 )
        write( out_line( last + 1: last + 2 ), '(i2.2)' ) hh
        last = last + 3

*       Now do mm.m if n_digit=5, mm otherwise
        value2 = ( value2 - hh ) * 6.0d+01
        if( n_digit .eq. 4 ) then
*         then round rather than truncate and deal with consequences
          mm = nint( value2 )
          if( mm .eq. 60 ) then
*           60 is ugly, so change to 0 and add 1 to degrees/hours, rewrite that.
            out_line( last + 1: last + 2 ) = '00'
            last = last - 3
            hh = hh + 1
            write( out_line( last + 1: last + 2 ), '(i2.2)' ) hh
          else
            write( out_line( last + 1: last + 2 ), '(i2.2)' ) mm
          end if
          goto 900
        else if( n_digit .eq. 5 ) then
          write( out_line( last + 1: last + 4 ), '(f4.1)' ) value2
          if( value2 .lt. 1.0d+01 ) then
*           f4.1 format can produce a blank of the number is less than 10,
*           force to have a 0 (i2.2 does this automatically for integers)
            out_line( last + 1: last + 1 ) = '0'
          else if( out_line( last + 1: last + 2 ) .eq. '60' ) then
*           60 is ugly, so change to 0 and add 1 to degrees/hours, rewrite that.
            out_line( last + 1: last + 2 ) = '00'
            last = last - 3
            hh = hh + 1
            write( out_line( last + 1: last + 2 ), '(i2.2)' ) hh
          end if
          goto 900
        else
          mm = int( value2 )
          write( out_line( last + 1: last + 2 ), '(i2.2)' ) mm
        end if
        last = last + 3

*       Now do ss if n_digit=6, ss.s otherwise (depends)
        value2 = ( value2 - mm ) * 6.0d+01
        if( n_digit .eq. 6 ) then
          labour = nint( value2 )
          if( labour .eq. 60 ) then
*           Take care of rounding up the minutes etc.
            out_line( last + 1: last + 2 ) = '00'
            last = last - 3
            mm = mm + 1
            if( mm .eq. 60 ) then
              out_line( last + 1: last + 2 ) = '00'
              last = last - 1
              hh = hh + 1
              write( out_line( last + 1: last + 2 ), '(i2.2)' ) hh
            else
              write( out_line( last + 1: last + 2 ), '(i2.2)' ) mm
            end if
          else
            write( out_line( last + 1: last + 2 ), '(i2.2)' ) labour
          end if
        else
          s_len = n_digit - 3
          write( formt, '(''(f'',i1,''.'',i1,'')'')' )
     &                                            s_len, n_digit - 6
          write( out_line( last + 1: last + s_len ), formt ) value2
          if( value2 .lt. 1.0d+01 ) then
            out_line( last + 1: last + 1 ) = '0'
          else if( out_line( last + 1: last + 2 ) .eq. '60' ) then
*           Take care of rounding up the minutes etc.
            out_line( last + 1: last + 2 ) = '00'
            last = last - 3
            mm = mm + 1
            if( mm .eq. 60 ) then
              out_line( last + 1: last + 2 ) = '00'
              last = last - 1
              hh = hh + 1
              write( out_line( last + 1: last + 2 ), '(i2.2)' ) hh
            else
              write( out_line( last + 1: last + 2 ), '(i2.2)' ) mm
            end if
          end if
        end if

900     continue

        end
