cUCUPER   ECSECT
c***********************************************************************
c*
c* -----------------------------------
c*  Subroutine UCLOWR( LENGTH, STRING )
c* -----------------------------------
c*
c* (Function)
c*    Translate upper case character to lower case.
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
 
      subroutine uclowr(length, string)
      character*(*) string
      DO 100 I = 1, LENGTH
         inchar = ichar(string(i:i))
         if( inchar.ge.65.and.inchar.le.90 ) then
            string(i:i) = char(inchar + 32)
         endif
 100   continue
       return
       end
 
