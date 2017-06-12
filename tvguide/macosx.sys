        subroutine arkopn(iunit, dirname, file, filex, stat, pos, form,
     &  acces, irecl, flag )
              
c       Opens a file with a machine independent call.  This is the SUN
c       Version.
                
c       Returned values.
c       The number of the unit opened.
        integer iunit
                     
c       Input values.
c       The file name, and the default extension to be used.
        character*(*) dirname, file, filex
c       The status to be used (OLD, NEW or UNKNOWN)
        character*(*) stat
c       The position to start writing into the file (APPEND, OVERWRITE,
c       READONLY).
        character*(*) pos
c       The form of the file (FORMATTED, UNFORMATTED or TAPE)
        character*(*) form
c       The file access (DIRECT or SEQUENTIAL), need only be supplied if
c       form is UNFORMATTED.
        character*(*) acces
c       The record length (the BLOCKSIZE is
c       set to the input record length).  Only used if access is 'DIRECT'.
        integer irecl

c       negative returned if error occurred
        integer flag
                     
c       Locals.
c       All the above strings translated to upper case.
        character*256 ufile1, ufile
        character*40 ustat, upos, uform, uacces
c       The error flag from UNIX_EXTEND
        integer iflag
	integer ldir
c       The VAX default file extension simulator.
        character*256 addon
        integer LENTRIM
                          
        flag = 0

c       Copy the strings into local variables.
	ldir = LENTRIM( dirname )
	if( ldir .eq. 0 ) then
	  ufile1 = file
	else
	  ufile1 = dirname( : ldir ) // '/' // file
	end if
        call unix_extend( ufile1, ufile, iflag)
        if( iflag .lt. 0 ) goto 900

        ufile=addon(ufile, filex)
        ustat  = stat
        upos   = pos
        uform  = form
c       Make all the open keywords upper case.
        call upcase(ustat)
        call upcase(upos)
        call upcase(uform)
        uacces  = 'SEQUENTIAL'
        if (uform .eq. 'UNFORMATTED') uacces = acces
        call upcase(uacces)
c       We had better check that pos has a legal value, as the operating
c       system won't tell us!
        if (upos  .eq. 'APPEND') then
          uacces = 'APPEND'
        else if (upos.ne.'READONLY' .and. upos.ne.'OVERWRITE') then
c          print*, 'ARK S/R ARKOPN called in illegal value for POS.'
c          print*, 'POS was "'//pos(1:len(pos))//'"'
          goto 900
        end if
              
c       Get a free unit.
        call GETLUN( iunit )
                             
c       This is a UNIX system which will crash if it attempts to open
c       an extant file with status='new', so cure the problem if the
c       user has specified 'OVERWRITE'.
        if (ustat.eq.'NEW' .and. upos.eq.'OVERWRITE') ustat='UNKNOWN'
                                                                     
c       The hard work is done here.
        if (uform .eq. 'TAPE') then
          open (unit=iunit, status='OLD', form='UNFORMATTED',
     &    access='SEQUENTIAL', file=ufile, err=900)
        else if (uacces .ne. 'DIRECT') then
          open (unit=iunit, status=ustat, form=uform, access=uacces,
     &      file=ufile, err=900)
        else
          open (unit=iunit, status=ustat, form=uform, access=uacces,
     &      recl=irecl, file=ufile, err=900)
        end if

        return
              
900     continue

        flag = -99

        end



	Subroutine UNIX_EXTEND( o_Name, n_Name, Flag )
C
C	Looks through the file name as given, and expands any
C	environment name (e.g., $ARKINFO) into full pathname.
C
	Character*( * ) o_Name, n_Name
	Integer Flag
C
	Integer L, M, o_Len, n_Len, Colon, Tilde, Slash, Dollar
	Character One, Work*7
C
        Flag = 1
	o_Len = Len( o_Name )
	n_Len = Len( n_Name )
	M = 0
	Colon = 0
	Tilde = 0
	Slash = 0
        Dollar = 0
	Do L = 1, o_Len
	  One = o_Name( L: L )
	  If( One .eq. ':' ) Then
	    Colon = L
	    M = L
	  Else If( One .eq. '~' ) Then
	    Tilde = L
	    M = L
	  Else If( One .eq. '/' ) Then
	    If( Slash .eq. 0 .and.
     1	      ( Colon.gt.0 .or. Tilde.gt.0 .or. Dollar.gt.0) ) Then
	      Slash = L
	    End If
	    M = L
          Else If( One .eq. '$' ) Then
            Dollar = L
            M = L
	  Else If( One .gt. ' ' ) Then
	    M = L
	  End If
	End Do
	o_Len = M
C
	If( Colon.gt.0 .or. Tilde.gt.0 .or. Dollar.gt.0) Then
	  If(Colon .eq. 8) Then
            Work=o_name(1:Colon-1)
            call upcase(Work)
            If (Work.eq.'ARKTEST' .or.
     &          Work.eq.'ARKHELP' .or.
     &          Work.eq.'ARKDATA') Then
c             Env name found, which matches an ARK one.
	      Call GETENV( Work, n_Name )
	      Slash = Colon
            End if
	  Else If( Tilde .gt. 0 ) Then
c					! ~ used
	    If( Slash .ne. Tilde + 1 ) Then
	      Flag = -11
	      Write( *, 120 )
120	      Format( ' ERROR:: Cannot expand other user''s home' )
	      Return
	    Else If( Tilde .ne. 1 ) Then
	      Flag = 0
	      Write( *, 125 )
125	      Format( ' ERROR:: UNIX file name syntax error' )
	    End If
	    Call GETENV( 'HOME', n_Name )
          Else If( Dollar .gt. 0) Then
            Call GETENV( o_Name( Dollar + 1: Slash - 1 ), n_Name )
	  End If
	  M = 0
	  Do L = 1, n_Len
	    If( n_Name( L: L ) .gt. ' ' ) M = L
	  End Do
	  If( M .eq. 0 ) Then
	    Flag = -12
	    Write( *, 130 )
130	    Format( ' ERROR:: Non-existent environment name' )
	    Return
	  Else If( M + o_Len - Slash + 1 .gt. n_Len ) Then
	    Flag = -13
	    Write( *, 140 )
140	    Format( ' ERROR:: Running out of file name buffer' )
	    Return
	  End If
	  n_Name = n_Name( 1: M ) //'/'//o_Name( Slash+1: o_Len )
	Else
	  If( o_Len .gt. n_Len ) Then
	    Flag = -13
	    Write( *, 140 )
	    Return
	  End If
	  n_Name = o_Name
	End If
C
	End
    


        subroutine PWRITE( string )

        integer pw_lun
        logical pw_open
        common / pw_cmmn / pw_lun, pw_open

        character*( * ) string

        integer LENTRIM

        write( *, '(a)' ) string( : LENTRIM( string ) )
        if( pw_open ) write( pw_lun, '(a)', err=900 )
     &                                     string( : LENTRIM( string ) )
        return

 900    continue
        print *, 'ERROR writing to log file, closing log'
        close( pw_lun )
        pw_open = .false.

        end



        subroutine PWRITEL( string )

        integer pw_lun
        logical pw_open
        common / pw_cmmn / pw_lun, pw_open

        character*( * ) string

        integer LENTRIM

        if( pw_open ) write( pw_lun, '(a)', err=900 )
     &                                     string( : LENTRIM( string ) )
        return

 900    continue
        print *, 'ERROR writing to log file, closing log'
        close( pw_lun )
        pw_open = .false.

        end



        subroutine PWOPEN( name )

        integer pw_lun
        logical pw_open
        common / pw_cmmn / pw_lun, pw_open

        character*( * ) name

        integer flag

        if( pw_open ) then
          print *, 'Log file already open'
          return
        end if

        call ARKOPN( pw_lun, ' ', name, 'log', 'NEW', 'APPEND',
     &               'FORMATTED', 'SEQUENTIAL', 1, flag )

        if( flag .lt. 0 ) then
          print *, 'ERROR:: failed to open the log file'
        else
          pw_open = .true.
        end if

        end



        subroutine PWCLOS( )

        integer pw_lun
        logical pw_open
        common / pw_cmmn / pw_lun, pw_open

        close( pw_lun )
        pw_open = .false.

        end



        block data PWCOMMN

        integer pw_lun
        logical pw_open
        common / pw_cmmn / pw_lun, pw_open

        data pw_lun / 99 /
        data pw_open / .false. /

        end



        subroutine WRITEN( string )

        implicit none

        character*( * ) string

        integer LENTRIM

        write( *, 100 ) string( : LENTRIM( string ) )
100     format( a, ' ', $ )

        end



c--------------------------------------------------------------------------
                                                                           
        character*(*) function addon(ifname, ext)
                                                 
        character*(*) ifname, ext
                                 
                                 
	Integer Last, Dot, K, Len_1, Len_2, One
                                        
	Len_1 = Len( ifname )
	Len_2 = Len( Ext )
	Last = 0
	Dot = 0
c	Semi = 0
	Do K = 1, Len_1
	  One = Ichar( ifname( K: K ) )
	  If( One .eq. 47 ) Then
c					! '/' End of Dir specification
	    Dot = 0
	  Else If( One .eq. 46 ) Then
c					! '.' Extension if not Dir
	    Dot = K
	  Else If( One .gt. 32 ) Then
c					! .not. ( ' ' End of file name )
	    Last = K
	  End If
	End Do
C
        addon=ifname
	if (Dot.eq.0 .and. ext(1:1).ne.' ' .and. ext(1:1).ne.char(0))
     &  addon = ifname( 1: Last ) // '.' // Ext
                                               
        end



        subroutine SPAWN( cline )

        implicit none

        character*( * ) cline

        call SYSTEM( cline )

        end



        subroutine ARKGCL( cbuf )

        character cbuf*( * )
                          
c       Returns the entire command line in cbuf.
c       Based on the XANADU subroutine rdforn.
                                              
        integer narg, iargc, i, j, lbuf
                                       
        narg=iargc()
        cbuf=' '
        lbuf=0
        if (narg .gt. 0) then
          do 190 i=1, narg
            call getarg(i, cbuf(lbuf+1:len(cbuf)))
c           Add a space between arguments, and update lbuf.
            do 170 j=len(cbuf), 1, -1
              if(cbuf(j:j).ne.' ' .and. cbuf(j:j).ne.char(0)) then
                cbuf(j+1:j+1)=' '
                lbuf=j+1
                goto 190
              end if
170         continue
190       continue
        end if
              
        end


C ** For Linux, replaced by ark_date.c ** JRG **
C
C*+ARK_DATE
C        subroutine ARK_DATE( year, month, day )
C
C        implicit none
C
C        integer year, month, day
C
C*       Calls system-dependent routines and returns the dates as 3 integers
C*       May have limitations due to the nature of the system routine
C*       such as a limited shelf life (cf. Y2K problem).
C*-ARK_DATE
C
C        integer array( 3 )
C        external IDATE
C
C        call IDATE( array )
C        year = array( 3 )
C        month = array( 2 )
C        day = array( 1 )
C
C        end
