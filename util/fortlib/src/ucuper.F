cUCUPER   ECSECT
c***********************************************************************
c*
c* -----------------------------------
c*  Subroutine UCUPER( LENGTH, STRING )
c* -----------------------------------
c*
c* (Function)
c*    Translate lower case character to upper case.
c*
c* (Input)
c*    LENGTH  : Length of input character in byte.
c*    STRING  : Character array.
c*
c* (Output)
c*    STRING  :
c*
c* (Author)
c*    A. Miyamoto  24-May-1986
c*    A. Miyamoto   4-Apr-1992 Prepared FORTRAN version for HP Unix.
c*
c***********************************************************************
 
      subroutine ucuper(length, string)
      character*(*) string
      DO 100 I = 1, LENGTH
         inchar = ichar(string(i:i))
         if( inchar.ge.97.and.inchar.le.122 ) then
            string(i:i) = char(inchar - 32)
         endif
 100   continue
       return
       end
 
