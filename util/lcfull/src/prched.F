CC********************************************************************CC
C*                                                                    *C
C*==================================                                  *C
C* Subroutine PRCHED(IDREC,BNKNAM)                                    *C
C*==================================                                  *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*     Dump out calibration header.                                   *C
C* (Inputs)                                                           *C
C*        IDREC   : calibration record ID.                            *C
C*        BNKNAM  : calibration header bank name.                     *C
C* (Output)                                                           *C
C*        None                                                        *C
C* (Relation)                                                         *C
C*     Called by run initialization routines.                         *C
C*     Calls TBGET.                                                   *C
C* (Update Record)                                                    *C
C*    6/15/87  K.Fujii            Original version.                   *C
C*                                                                    *C
CC********************************************************************CC
 
      SUBROUTINE PRCHED(IDREC,BNKNAM)
 
      CHARACTER*64 BNKNAM
      CHARACTER*44 CALNAM
      INTEGER*4    IHEAD(20)
 
C
C========< Entry Point >================================================
C
C--
C  TBGET calibration header.
C--
      CALL TBGET(IDREC,BNKNAM,1,NW,IHEAD,IRT)
      IF ( IRT.LT.0 )                            GO TO 9999
C--
C  Print out calibration header.
C--
      WRITE(CALNAM,'(11A4)') (IHEAD(I),I=5,15)
      PRINT *, ' ***** ',BNKNAM,' ***** '
      PRINT *, '   Creation Date      = ',IHEAD(1)
      PRINT *, '   Creation Time      = ',IHEAD(2)
      PRINT *, '   Original file name = ',CALNAM
      PRINT *, '   To be used from Exp#',IHEAD(3),' Run#',IHEAD(4),' on'
C--
C  Tally up errors and return.
C--
C --- Normal end.
C
      RETURN
C
C --- Error.
C
9999  PRINT *, '     Could not find ',BNKNAM
      RETURN
      END
