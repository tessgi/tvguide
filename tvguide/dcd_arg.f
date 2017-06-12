*+DCD_ARG
        subroutine DCD_ARG( string, names, types, prompts, n_list,
     &                      values, status )

        implicit none

        integer n_word
        parameter( n_word = 64 )

        character*(*) string
        integer n_list
        character*(*) names( n_list )
        integer types( n_list )
        character*(*) prompts( n_list )
        character*(*) values( n_list )
        integer status

*       Description
*         This routine decodes a string containing command line arguments,
*         compares it with the master list and returns the list of
*         arguments provided.  Arguments may be given by position,
*         by name=value scheme, or by both to some degree.
*
*       Arguments:
*         string    (i) : Input string
*         names     (i) : List of parameter names
*         types     (i) : 0 if compulsory and 1 if optional/hidden
*         prompts   (i) : Prompt string for compulsory parameters
*         n_list    (i) : Number of parameters
*         values    (o) : Parameter values
*         status    (o) : 0 if no errors were detected
*                         positive if there were undecoded bits
*                         negative if there were outright errors
*
*       Dependencies:
*         DCD_ARG specific subroutines (included in this file), LENTRIM
*
*       Origin:
*         Conceived by KM for user interface of VIEWING
*
*       Author:
*         Koji Mukai, 1995 Aug, original version
*-DCD_ARG

        integer done( n_word )
*               done=type if not specified, done=2 if specified
        integer beg1, beg2, len1, len2
        integer k, l, len, w_status
        logical equal, quote, dquote
        integer LENTRIM
        character*256 word1, word2
        character one

*       Sanity check first
        if( n_list .gt. n_word ) then
          status = -1
          goto 900
        end if

*       Initialize some variables
        status = 0
        do k = 1, n_list
          values( k ) = ' '
          done( k ) = types( k )
        end do
        len = LENTRIM( string )
        equal = .false.

*       Go through the input line and decode it
        l = 1
*       Look for the beginning of the first word
        call DA_FNDWD( string, len, equal, l, w_status )
        if( w_status .lt. 0 ) then
          status = -99
          goto 900
        end if
        do while( w_status .gt. 0 )
*         Let's find the end of the first word
          one = string( l: l )
          if( one .eq. '''' ) then
            quote = .true.
            dquote = .false.
            beg1 = l + 1
          else if( one .eq. '"' ) then
            quote = .false.
            dquote = .true.
            beg1 = l + 1
          else
            quote = .false.
            dquote = .false.
            beg1 = l
          end if
          l = l + 1
          do while( l .le. len )
            one = string( l: l )
            if( quote ) then
              if( one .eq. '''' ) then
                len1 = l - beg1
                quote = .false.
                goto 200
              end if
            else if( dquote ) then
              if( one .eq. '"' ) then
                len1 = l - beg1
                dquote = .false.
                goto 200
              end if
            else
              if( one .eq. '=' ) then
                equal = .true.
                len1 = l - beg1
                goto 200
              else if( one .eq. ',' ) then
                len1 = l - beg1
                word1 = string( beg1: beg1 + len1 - 1 )
                goto 300
              else if( one .eq. ' ' ) then
                len1 = l - beg1
                goto 200
              end if
            end if
            l = l + 1
          end do
          if( quote .or. dquote ) then
            status = -98
            goto 900
          end if
          len1 = l - beg1 + 1
200       continue
          word1 = string( beg1: beg1 + len1 - 1 )
*         Now look if there is another word on the command line
          l = l + 1
          call DA_FNDWD( string, len, equal, l, w_status )
          if( w_status .lt. 0 ) then
            status = -99
            goto 900
          end if
*         Ah but did it see an '=' sign there, in which case
*         we are looking at a name=value construct...
300       continue
          if( equal .or. ( w_status .eq. 2 ) ) then
            one = string( l: l )
            if( one .eq. '''' ) then
              quote = .true.
              dquote = .false.
              beg2 = l + 1
            else if( one .eq. '"' ) then
              quote = .false.
              dquote = .true.
              beg2 = l + 1
            else
              quote = .false.
              dquote = .false.
              beg2 = l
            end if
            l = l + 1
            do while( l .le. len )
              one = string( l: l )
              if( quote ) then
                if( one .eq. '''' ) then
                  len2 = l - beg2
                  quote = .false.
                  goto 400
                end if
              else if( dquote ) then
                if( one .eq. '"' ) then
                  len2 = l - beg2
                  dquote = .false.
                  goto 400
                end if
              else
                if( one .eq. '=' ) then
                  status = -99
                  goto 900
                else if( one .eq. ',' ) then
                  len2 = l - beg2
                  goto 400
                else if( one .eq. ' ' ) then
                  len2 = l - beg2
                  goto 400
                end if
              end if
              l = l + 1
            end do
            if( quote .or. dquote ) then
              status = -98
              goto 900
            end if
            len2 = l - beg2 + 1
400         continue
            word2 = string( beg2: beg2 + len2 - 1 )
            call DA_EXPLC( word1, len1, word2, len2, names,
     &                                    n_list, done, values, status )
            if( status .lt. 0 ) goto 900
            equal = .false.
            l = l + 1
            call DA_FNDWD( string, len, equal, l, w_status )
            if( w_status .lt. 0 ) then
              status = -99
              goto 900
            end if
          else
            call DA_BYPOS( word1, len1, n_list, done, values, status )
            if( status .lt. 0 ) goto 900
          end if
        end do

500     continue
*       Final pass --- if there are parameters not entered, ask for them
        do k = 1, n_list
          do while( done( k ) .eq. 0 )
            call WRITEN( prompts( k ) )
            read '(a)', values( k )
            if( values( k ) .gt. ' ' ) done( k ) = 1
          end do
        end do
        return

900     continue
*       Write error messages before returning
        if( status .eq. -1 ) then
          call PWRITE( 'Error in DCD_ARG:: Too many options' )
        else if( status .eq. -98 ) then
          call PWRITE( 'Error in DCD_ARG:: unmatched quotation' )
        else if( status .eq. -99 ) then
          call PWRITE( 'Error in DCD_ARG:: illiegal equal' )
        end if

        end



*+DA_EXPLC
        subroutine DA_EXPLC( word1, len1, word2, len2, names,
     &                                    n_list, done, values, status )

        implicit none

        character*(*) word1, word2
        integer len1, len2
        integer n_list
        character*(*) names( n_list )
        integer done( n_list )
        character*(*) values( n_list )
        integer status

*       Description
*         Given a word1=word2 type specification against list of parameter
*         names, this routine tries to figure out what the user meant.
*
*       Arguments:
*         word1     (i) : Apparent parameter name
*         len1      (i) : Length of above
*         word2     (i) : Apparent parameter value
*         len2      (i) : Length of above
*         names     (i) : List of valid parameter names
*         n_list    (i) : Number of possible parameters
*         done      (i) : Which have already been decoded.
*         values    (o) : Parameter values
*         status    (o) : 0 if no errors were detected
*                         positive if there were undecoded bits
*                         negative if there were outright errors
*
*       Dependencies:
*         LOCASE for case conversion.
*
*       Origin:
*         Conceived by KM for user interface of VIEWING
*
*       Author:
*         Koji Mukai, 1995 Aug, original version
*-DA_EXPLCT

        integer match
        integer len_v
        integer m
        character*256 buff1, buff2

        if( len1 .gt. 256 .or. len2 .gt. 256 ) then
          status = -21
          return
        end if
        buff1 = word1
        call LOCASE( buff1 )

        match = 0
        do m = 1, n_list
          buff2 = names( m )
          call LOCASE( buff2 )
          if( buff1( : len1 ) .eq. buff2( : len1 ) ) then
*           A match found
            if( match .gt. 0 ) then
*             This is the second match, i.e., the name string was ambiguous
              status = -22
              return
            else if( done( m ) .gt. 1 ) then
*             Matched a name which already has a value
              status = -23
            end if
            match = m
            len_v = len( values( m ) )
            if( len2 .gt. len_v ) then
              status = -23
              return
            end if
          end if
        end do

*       Was there a match?
        if( match .eq. 0 ) then
          status = -24
        else
          values( match ) = word2( : len2 )
          done( match ) = 2
        end if

        end



*+DA_BYPOS
        subroutine DA_BYPOS( word1, len1, n_list, done, values, status )

        implicit none

        character*(*) word1
        integer len1
        integer n_list
        integer done( n_list )
        character*(*) values( n_list )
        integer status

*       Description
*         Given a parameter specification by position,
*         this routine tries to figure out what the user meant.
*
*       Arguments:
*         word1     (i) : Apparent parameter value
*         len1      (i) : Length of above
*         n_list    (i) : Number of possible parameters
*         done      (i) : Which have already been decoded.
*         values    (o) : Parameter values
*         status    (o) : 0 if no errors were detected
*                         positive if there were undecoded bits
*                         negative if there were outright errors
*
*       Dependencies:
*         None
*
*       Origin:
*         Conceived by KM for user interface of VIEWING
*
*       Author:
*         Koji Mukai, 1995 Aug, original version
*-DA_BYPOS

        integer m
        integer len_v

        do m = 1, n_list
          if( done( m ) .le. 1 ) then
            len_v = len( values( m ) )
            if( len1 .gt. len_v ) then
              status = -23
            else
              values( m ) = word1( : len1 )
              done( m ) = 2
            end if
            return
          end if
        end do

        status = -24

        end



*+DA_FNDWD
        subroutine DA_FNDWD( string, len, o_equal, l, status )

        implicit none

        integer len
        character*(*) string
        logical o_equal
        integer l
        integer status

*       Description
*         Given a string (length len) and positioned at lth character,
*         finds the beginning of the next word.
*
*       Arguments:
*         string    (i) : Input string
*         len       (i) : Length of above
*         o_equal   (i) : Whether to expect equal sign or not
*         l        (i/o): Current position
*         status    (o) : 2 if equal and next word found;
*                         1 if next word found;
*                         0 if EOF;
*                         -1 for unexpected equal;
*
*       Dependencies:
*         None
*
*       Origin:
*         Conceived by KM for user interface of VIEWING
*
*       Author:
*         Koji Mukai, 1995 Aug, original version
*-DA_FNDWD

        character one

        status = 0
        one = string( l: l )
        if( one .eq. '=' ) then
          if( o_equal ) then
            status = -1
            return
          else
            status = 1
          end if
        end if

        do while( one .le. ' ' )
          l = l + 1
          if( l .gt. len ) then
            return
          end if
          one = string( l: l )
          if( one .eq. '=' ) then
            if( o_equal .or. status .eq. 1 ) then
              status = -1
              return
            else
              status = 1
            end if
          end if
        end do

        status = status + 1

        end
