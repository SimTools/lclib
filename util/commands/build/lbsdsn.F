C* 
C**********************************************************************
C* 
C* --------------------------===========================
C* Subroutine LBSDSN( FULDSN, DSN, MEMBER ,KDSN, KMEM )
C* --------------------------===========================
C* 
C*(Function)
C*   Interprete the fully qualified data set name, and returns
C*  the data set name and the member name.  If any bra, or ket is
C*  not contained in the FULDSN, is is assumed to be PS file,
C*  and MEMBER string is returned with null .
C* 
C*(Input)
C*     FULDSN  ... Character*(*) string of fully qualified file name.
C* 
C*(Output)
C*     DSN     ... Character*(*) array of data set name.
C*     MEMBER  ... Character*(*) array of member name.
C*     KDSN    ... Length of the data set name.
C*     KMEM    ... Length of the member name.
C*
C*(Author) A.Miyamoto  2-May-1994
C*     Modified for use on UNIX system.
C*     File name is devided by last "/"into directory name and file name.
C*      
C***********************************************************************
C* 
      SUBROUTINE LBSDSN( FULDSN, DSN, MEMBER ,KDSN, KMEM)
C  
      CHARACTER*(*) FULDSN, DSN, MEMBER
      INTEGER*4     RINDEX,GETCWD,LNBLNK
C  
      LFULL = LEN(FULDSN)
      LFULL = LNBLNK(FULDSN(:LFULL))
      LDSN  = LEN(DSN)
      LMEM  = LEN(MEMBER)
C
      LRSL  = RINDEX(FULDSN(:LFULL),'/')
      DSN(:LDSN) = ' '
      IF( LRSL .EQ. 0 ) THEN     
#if defined(HP) || (defined(MACHTEN_PPC) && !defined(G77_521_OR_GREATER))
         IR = getcwd( DSN , LDSN )  ! for  HP unix
         DSN(:LDSN) = DSN(:(INDEX(DSN,' ')-1)) 
#else 
         IR = getcwd( DSN )
         IF( IR .NE. 0 ) THEN 
            PRINT *,'%Error..LBSDSN can not get Path of Current ',
     >              'Working Directory.'
            STOP
         ENDIF
#endif
         KDSN = LNBLNK( DSN )
         DSN(KDSN+1:KDSN+1) = '/'
         KDSN = KDSN + 1
         MEMBER = FULDSN(:LFULL)
         KMEM = LFULL
      ELSE
         DSN(:LDSN) = FULDSN(:LRSL)
         KDSN = LRSL
         MEMBER = FULDSN(LRSL+1:LFULL)
         KMEM = LFULL - LRSL
      ENDIF
C
      RETURN
      END
