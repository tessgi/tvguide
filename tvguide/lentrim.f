*+LENTRIM
        integer function LENTRIM( string )

        implicit none

        character*( * ) string

*       Description:
*         Emulates the Fortran 90 intrinsic function of the same name
*         (length of the string without trailing blanks)
*
*       Arguments:
*         string          (i) : Input string of unknown length
*         <LENTRIM>       (r) : Length without trailing blanks
*
*       Dependencies:
*         None
*
*       Origin:
*         Description of Fortran 90 intrinsic in the Metcalf book
*
*       Author:
*         Koji Mukai,  1992 December, original version
*-LENTRIM

        integer l, lmax, lmax2

        lmax = LEN( string )
        lmax2 = 0
        do l = 1, lmax
          if( string( l: l ) .gt. ' ' ) lmax2 = l
        end do
        LENTRIM = lmax2

        end
