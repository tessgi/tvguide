*+JULIAN
        integer function JULIAN( year, month, day )

        implicit none

        integer year, month, day

*       Description:
*         Given a calendar year, month, day, this function returns the
*         integer part of unmodified julian date.  The result is exactly
*         correct for 12:00 UT on that day.  Guaranteed only for modern
*         astronomical use (probably okay for present+/-several centuries,
*         this requires inputs in Gregorian system)
*
*       Arguments:
*         year, month, day   (i) : Input calendar date
*         <julian>           (r) : JD for 12:00 UT, as integer
*
*       Dependencies:
*         None
*
*       Origin:
*         Lost in the mist of time --- probably the Monroe book on Fortran 77
*
*       Author:
*         Koji Mukai, 1993 Feb 3, first official version
*-JULIAN

        integer yy, mm, dd

        if( month .eq. 1 .or. month .eq. 2 ) then
          mm = month + 9
          yy = year - 1
        else if( month .ge. 3 ..and. month .le. 12 ) then
          mm = month - 3
          yy = year
        else
          stop 'ERROR:: non-existent month passed to JULIAN'
        end if

        dd = day + ( yy + 4712 ) * 1461 / 4
     &           + ( mm * 306 + 5 ) / 10 + 59
        dd = dd - ( yy / 100 + 49 ) * 3 / 4 + 38

        JULIAN = dd

        end
