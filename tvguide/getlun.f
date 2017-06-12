*+GETLUN

        subroutine GETLUN( lun )

        implicit none

        integer lun

*       Returns a free unit number
*-GETLUN
        logical there

        lun = 11
        inquire( unit = lun, opened = there )
        do while( there )
          lun = lun + 1
          inquire( unit = lun, opened = there )
        end do

        end
