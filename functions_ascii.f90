MODULE functions_ascii
!=====#################################################################
! NAME:
!		functions_ascii.f90
! PURPOSE:
! 		Module with several function to handle Asciis
! CALLING SEQUENCE:
!		-
! NEEDS:
!		kinds.f90
! INCLUDES:
!		ascii_get_column_string
!		ascii_get_column_float
!		ascii_get_nlines
!		int2str
!		str2int
!		parse
! REVISION HISTORY:
! 		written by Miriam Kosmale 2018
!=====#################################################################
USE kinds
IMPLICIT NONE 
CONTAINS
  
!=====#################################################################
SUBROUTINE ascii_get_column_string(filename,line,column,valuestr)
! PURPOSE:
! 		get an value from an ascii
!		by providing the line and column number
! INPUTS:
!		filename	:: the name of the file
!		line		:: the line where the data starts (counting from 1)
!		column		:: the column where the data is written (counting from 1)
! CALLING SEQUENCE:
!		filename='file.txt'
!		line=2
!		column=4
!		CALL ascii_get_column_string(filename,line,column,valuestr)
!		print*,valuestr
! NOTE:
!		no parallel processing possible here!!!
!		because of creating a temporary file with awk
! REVISION HISTORY:
! 		written by Miriam Kosmale 2015
!----------------------------------------------------------------------
    CHARACTER(LEN=c_len),INTENT(in)			:: filename
	INTEGER(KIND=p_short),INTENT(in)		:: line
	INTEGER(KIND=p_short),INTENT(in)		:: column
	
    CHARACTER(LEN=c_len),ALLOCATABLE,INTENT(out)	:: valuestr(:)
	
    INTEGER(KIND=p_short)					:: i,j
    INTEGER(KIND=p_short)					:: nlines,arrlen
    INTEGER(KIND=p_short)					:: status,status2,ios
	
	CHARACTER (LEN=10)						:: icolumn_str,line0_str	
    CHARACTER(LEN=c_len)					:: command
	
    command	= 'cat '//trim(filename)//' | wc -l > nlines.txt'
    status	= system(command)
    open (10, file='nlines.txt')
           read (10,*,iostat=ios) nlines
    close(10)
    command	= 'rm nlines.txt'
    status	= system(command)
	
	arrlen	= nlines-line+1
	allocate(valuestr(arrlen), stat=status2)

    Write( icolumn_str, '(i4)' ) column
    Write( line0_str, '(i4)' ) line
    command="awk 'NR>="//trim(line0_str)//"' "//trim(filename)//&
		" | awk '{print $"//trim(icolumn_str)//"}' > column.txt"
    status	= system(command)
    open (10, file='column.txt')
    read (10,*) valuestr
    close(10)
    command	= 'rm column.txt'
    status	= system(command)


END SUBROUTINE ascii_get_column_string
!=====#################################################################
SUBROUTINE ascii_get_column_float(filename,line,column,valuefloat)
! PURPOSE:
! 		get an value from an ascii
!		by providing the line and column number
! INPUTS:
!		filename	:: the name of the file
!		line		:: the line where the data starts (counting from 1)
!		column		:: the column where the data is written (counting from 1)
! CALLING SEQUENCE:
!		filename='file.txt'
!		line=2
!		column=4
!		CALL ascii_get_column_float(filename,line,column,valuefloat)
!		print*,valuefloat
! NOTE:
!		no parallel processing possible here!!!
!		because of creating a temporary file with awk
! REVISION HISTORY:
! 		written by Miriam Kosmale 2015
!----------------------------------------------------------------------
    CHARACTER(LEN=c_len),INTENT(in)			:: filename
	INTEGER(KIND=p_short),INTENT(in)		:: line
	INTEGER(KIND=p_short),INTENT(in)		:: column
	
    REAL(KIND=p_double),ALLOCATABLE,INTENT(out)	:: valuefloat(:)
	
    INTEGER(KIND=p_short)					:: i,j
    INTEGER(KIND=p_short)					:: nlines,arrlen
    INTEGER(KIND=p_short)					:: status,status2,ios
	
	CHARACTER (LEN=10)						:: icolumn_str,line0_str	
    CHARACTER(LEN=c_len)					:: command
	
    command	= 'cat '//trim(filename)//' | wc -l > nlines.txt'
    status	= system(command)
    open (10, file='nlines.txt')
           read (10,*,iostat=ios) nlines
    close(10)
    command	= 'rm nlines.txt'
    status	= system(command)
	
	arrlen	= nlines-line+1
	allocate(valuefloat(arrlen), stat=status2)

    Write( icolumn_str, '(i4)' ) column
    Write( line0_str, '(i4)' ) line
    command="awk 'NR>="//trim(line0_str)//"' "//trim(filename)//&
		" | awk '{print $"//trim(icolumn_str)//"}' > column.txt"
    status	= system(command)
    open (10, file='column.txt')
    read (10,*) valuefloat
    close(10)
    command	= 'rm column.txt'
    status	= system(command)


END SUBROUTINE ascii_get_column_float
!=====#################################################################
SUBROUTINE ascii_get_nlines(filename,nlines)
! PURPOSE:
! 		get the number of lines from an ascii-file
! INPUTS:
!		filename	:: the name of the file
! CALLING SEQUENCE:
!		filename='file.txt'
!		CALL ascii_get_nlines(filename,nlines)
!		print*,nlines
! NOTE:
!		no parallel processing possible here!!!
!		because of creating a temporary file with awk
! REVISION HISTORY:
! 		written by Miriam Kosmale 2015
!----------------------------------------------------------------------
    CHARACTER(LEN=c_len),INTENT(in)			:: filename
	INTEGER(KIND=p_short),INTENT(out)		:: nlines
	
    CHARACTER(LEN=c_len)					:: command
    INTEGER(KIND=p_short)					:: ios, whatever,status
	
    command	= 'cat '//trim(filename)//' | wc -l > nlines.txt'
    open (10, file='nlines.txt')
           read (10,*,iostat=ios) nlines
    close(10)
    status	= system(command)
    command	= 'rm nlines.txt'
    status	= system(command)


END SUBROUTINE ascii_get_nlines
!=====#################################################################
SUBROUTINE int2str(int,str)
! PURPOSE:
! 		converts an integer value to a string
! INPUTS:
!		int			:: the integer value
! CALLING SEQUENCE:
!		CALL int2str(int,str)
!		print*,str
! REVISION HISTORY:
! 		written by Miriam Kosmale 2015
!----------------------------------------------------------------------
    CHARACTER(len=*),intent(out)			:: str
    INTEGER,intent(in)						:: int

	WRITE( str, '(i10)' ) int

END SUBROUTINE int2str
!----------------------------------------------------------------------
SUBROUTINE str2int(str,int,stat)
! PURPOSE:
! 		converts a string to an integer value
! INPUTS:
!		str			:: the string value
! CALLING SEQUENCE:
!		CALL str2int(str,int,stat)
!		print*,int
! REVISION HISTORY:
! 		written by Miriam Kosmale 2015
!----------------------------------------------------------------------
    CHARACTER(len=*),intent(in) :: str
    INTEGER,intent(out)         :: int
    INTEGER,intent(out)         :: stat

	READ(str,*,iostat=stat)  int

END SUBROUTINE str2int
!=====#################################################################
! PURPOSE:
! 		divides a string to its values separated by blanks
! INPUTS:
!		c			:: the string line, with values separated by blanks
! OUTPUTS:
!		v			:: the array with the values
! CALLING SEQUENCE:
!		CALL parse(c,v)
!		print*,v
! REVISION HISTORY:
! 		written by Florian Streicher 2013
!----------------------------------------------------------------------
SUBROUTINE parse(c,v)
	character(*) c
	integer, allocatable :: v(:)
	integer i,j,l,n,int,stat
	l = len_trim(c)
	n = 1
	do i=1,l
		if( c(i:i) == ' ' ) n = n+1
	enddo
	if ( allocated(v) ) deallocate(v)
	allocate(v(n))
	n = 0
	j = 1
	do i=1,l
		if( c(i:i) == ' ' .or. i==l ) then
			n = n+1
			CALL str2int(trim(c(j:i)),int,stat)
			v(n) = int
			j=i+1
		endif
	enddo
END SUBROUTINE parse
!=====#################################################################
END MODULE functions_ascii
