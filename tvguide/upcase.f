*+UPCASE
        subroutine UPCASE( string )

        implicit none

        character*( * ) string

*       Description:
*         Converts a character string to upper case
*
*       Argument:
*         string   (i/o) : string in question
*
*       Dependencies:
*         Assumes an Ascii architecture.  Also needs LENTRIM
*
*       Origin:
*         Created by KM to highlight mission names etc., to compliment
*         LOCASE
*
*       Author:
*         Koji Mukai (1993 Mar 18) Original version
*-UPCASE

        integer len, j
        integer LENTRIM

        len = LENTRIM( string )
        do j = 1, len
          if( string( j: j ) .ge. 'a'
     &                       .and. string( j: j ) .le. 'z' ) then
            string( j: j ) = char( ichar( string( j: j ) ) - 32 )
          end if
        end do

        end


*+LOCASE
        subroutine LOCASE( string )

        implicit none

        character*( * ) string

*       Description:
*         Converts a character string to lower case
*
*       Argument:
*         string   (i/o) : string in question
*
*       Dependencies:
*         Assumes an Ascii architecture.  Also needs LENTRIM
*
*       Origin:
*         Created by KM to duplicate XANADU LOCASE
*
*       Author:
*         Koji Mukai (1994 Dec) Original version
*-LOCASE

        integer len, j
        integer LENTRIM

        len = LENTRIM( string )
        do j = 1, len
          if( string( j: j ) .ge. 'A'
     &                       .and. string( j: j ) .le. 'Z' ) then
            string( j: j ) = char( ichar( string( j: j ) ) + 32 )
          end if
        end do

        end
