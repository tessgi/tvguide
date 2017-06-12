*+DCD_DATE
        subroutine DCD_DATE( string, year, month, day, status )

        implicit none

        character*(*) string
        integer year, month, day
        integer status

*       Description
*         This routine decodes a string, either of the form mm/dd/yy or
*         dd-MMM-(yy)yy, and put into integer variables year, month and date.
*         The year need not be present in the string, in which case there
*         is no guarantee that the variable "year" has a sensible value.
*         Interpretation of two digit YY is correct for the period
*         1950-2049.
*
*       Arguments:
*         string    (i) : Input string
*         year      (o) : Year, full 4 digits
*         month     (o) : Month
*         day       (o) : Day, after some sanity check
*         status    (o) : 0 if no errors were detected
*
*       Dependencies:
*         LOCASE, a XANLIB routine, for case conversion.
*
*       Origin:
*         Conceived by KM for user interface of VIEWING
*
*       Author:
*         Koji Mukai, 1993 Jan 30, original version
*-DCD_DATE

        character*32 buffer
        integer length, kstart, k, m, labour
        character one

        integer LENTRIM

        character*9 mnname( 12 )
        integer mnlimt( 12 )
        data mnname / 'january', 'february', 'march', 'april', 'may',
     &                'june', 'july', 'august', 'september',
     &                'october', 'november', 'december' /
        data mnlimt / 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /

        status = 0

        length = LENTRIM( string )
        if( length .gt. 32 ) then
          status = -99
*         the string appears to be too long
          goto 900
        else
          buffer = string( : length )
        end if
        call LOCASE( buffer )

        k = 1
        do while( buffer( k: k ) .le. ' ' .and. k .le. length )
          k = k + 1
        end do
        if( k .eq. length ) then
*         string was empty
          status = -1
          goto 900
        end if

        kstart = k
        do while( k .le. length )
          one = buffer( k: k )
          if( one .lt. '0' .or. one .gt. '9' ) goto 110
          k = k + 1
        end do
*       string consisted entirely of digits
        status = -2
        goto 900

 110    continue
*       Found the first non-digit character --- read the first integer
        call DD_PCKUP( buffer, kstart, k, labour, status )
        if( status .lt. 0 ) goto 900
        if( one .eq. '/' ) then
*         Okay, now assume mm/dd/yy format
          if( labour .ge. 1 .and. labour .le. 12 ) then
            month = labour
          else
            status = -12
            goto 900
          end if
          kstart = k + 1
          k = kstart
          do while( k .le. length )
            one = buffer( k: k )
            if( one .lt. '0' .or. one .gt. '9' ) goto 121
            k = k + 1
          end do
*         end of string before second "/" --- user input mm/dd
          call DD_PCKUP( buffer, kstart, k, day, status )
          if( status .lt. 0 ) goto 900
          goto 800

 121      continue
          if( one .eq. '/' ) then
            call DD_PCKUP( buffer, kstart, k, day, status )
            if( status .lt. 0 ) goto 900
            kstart = k + 1
            k = kstart
            do while( k .le. length )
              one = buffer( k: k )
              if( one .lt. '0' .or. one .gt. '9' ) goto 122
              k = k + 1
            end do
 122        continue
            call DD_PCKUP( buffer, kstart, k, year, status )
            if( status .lt. 0 ) goto 900
            if( year .ge. 50 ) then
              year = year + 1900
            else
              year = year + 2000
            end if
          else
            status = -5
            goto 900
          end if


        else if( one .eq. '-' ) then
*         Assume dd-MMM-yy format
          day = labour
          kstart = k + 1
          k = kstart
          do while( k .le. length )
            one = buffer( k: k )
            if( one .lt. 'a' .or. one .gt. 'z' ) then
              goto 131
            end if
            k = k + 1
          end do
 131      continue
          if( k .le. kstart + 2 .or. k .gt. kstart + 10 ) then
            status = -4
            goto 900
          else
            month = 0
            do m = 1, 12
              if( buffer( kstart: k - 1 ) .eq.
     &              mnname( m )( 1: k - kstart ) ) month = m
            end do
            if( month .eq. 0 ) then
              status = -4
              goto 900
            end if
            one = buffer( k: k )
            if( one .eq. '-' ) then
              kstart = k + 1
              k = kstart
              do while( k .le. length )
                one = buffer( k: k )
                if( one .lt. '0' .or. one .gt. '9' ) goto 132
                k = k + 1
              end do
 132          continue
              if( k .eq. kstart + 2 ) then
                read( buffer( kstart: kstart + 1 ), '(i2)' ) year
                if( year .ge. 50 ) then
                  year = year + 1900
                else
                  year = year + 2000
                end if
              else if( k .eq. kstart + 4 ) then
                read( buffer( kstart: kstart + 3 ), '(i4)' ) year
              else if( k .eq. kstart ) then
                continue
              else
                status = -13
                goto 900
              end if
            else if( one .ne. ' ' ) then
              status = -4
              goto 900
            end if
          end if
        else
          status = -5
          goto 900
        end if

*       Sanity check --- is "day" sensible (not checked for leap year
*       if it is claimed to be Feb 29th)
 800    continue
        if( day .lt. 1 .or. day .gt. mnlimt( month ) ) then
          status = -11
        end if

 900    continue

        end


*+DD_PCKUP
        subroutine DD_PCKUP( string, k1, k2, number, status )

        implicit none

        character*( * ) string
        integer k1, k2, number, status

*       Description:
*         Use internal I/O to read 1 or 2 digit numbers from string
*
*       Arguments:
*         string    (i) : Input string
*         k1, k2    (i) : Location of number within string
*         number    (o) : The actual number read
*         status    (o) : 0 if no errors detected
*
*       Dependencies:
*         None
*
*       Origin:
*         Created by KM for internal use by DCD_DATE
*
*       Author
*         Koji Mukai, 1993 Jan 29, original version
*-DD_PCKUP

        if( k2 .eq. k1 ) then
          status = -3
        else if( k2 .eq. k1 + 1 ) then
          read( string( k1: k1 ), '(i1)' ) number
        else if( k2 .eq. k1 + 2 ) then
          read( string( k1: k1 + 1 ), '(i2)' ) number
        else
          status = -3
        end if

        end
