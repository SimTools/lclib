C   12/11/86 611121936  MEMBER NAME  COMHELPO (FORT)     M  FORTRAN
C+++++ This is COMHELP.FOR             B. Gabioud Nov 1982
C
       SUBROUTINE COMHLP(NVAL,NAMES,HELP)
       CHARACTER*(*) NAMES(NVAL), HELP(NVAL)
c___   include 'pep4_com_lib(comode)'
       include 'comode.inc'
C@@@   EXTERNAL HELP_LINE, LIB$GET_INPUT
C
C..... TYPE HELP INFORMATION
C
       IF (HELP(1)(1:1).EQ.'$') THEN
         PRINT *, '%COM-F-HLPFIL, External Help is not supported yet.'
       ELSE
         DO 10 J = 1,NVAL
           LH = LENRD(HELP(J))
           IF (NAMES(J).NE.' ' .OR. NAMES(J-1).NE.' ' .OR.
     &           HELP(J).NE.' ' .OR. HELP(J-1).NE.' ')
     &         WRITE (LUNCOM,104) NAMES(J),HELP(J)(1:LH)
10       CONTINUE
       END IF
       WRITE (LUNCOM,*)
104    FORMAT (1XA,2XA)
       RETURN
       END
