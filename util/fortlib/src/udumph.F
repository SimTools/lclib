CC********************************************************************CC
C*                                                                    *C
C*=========================================                           *C
C*  Subroutine UDUMPH(NWORDS,IWORK,ITYPE)                             *C
C*=========================================                           *C
C*                                                                    *C
C* 1) Purpose                                                         *C
C*     Dumps INTEGER*2 array.                                         *C
C* 2) Inputs                                                          *C
C*      NWORDS = # words in I*2 unit.                                 *C
C*      IWORK  = I*2 array.                                           *C
C*      ITYPE  = 1 ; I format.                                        *C
C*             = 2 ; Z format.                                        *C
C*             = 3 ; both I and Z formats.                            *C
C* 3) Outputs                                                         *C
C*      None                                                          *C
C* 4) Relation                                                        *C
C*      Called by many subroutines for debugging.                     *C
C*      Calls no subroutines.                                         *C
C* 4) Update record                                                   *C
C*    11/14/85  K. Fujii  Cloned from TKSF.S#RDAT.FORT(DUMPH).        *C
C*                                                                    *C
CC********************************************************************CC
C  
C ============================================
      SUBROUTINE UDUMPH(NWORDS,IWORK,ITYPE)
C ============================================
C  
      INTEGER*2 IWORK(1)
      LOGICAL*1 IIFLAG,IZFLAG
C  
      IF ( ITYPE.EQ.1 ) THEN
        IIFLAG = .TRUE.
        IZFLAG = .FALSE.
      ELSE IF ( ITYPE.EQ.2 ) THEN
        IIFLAG = .FALSE.
        IZFLAG = .TRUE.
      ELSE IF ( ITYPE.EQ.3 ) THEN
        IIFLAG = .TRUE.
        IZFLAG = .TRUE.
      ELSE
        GO TO 50
      ENDIF
C  
      NTIMS  = NWORDS/15+1
      IST    = 1
C  
      DO 40 I=1,NTIMS
        IWRDS = MIN(15,NWORDS-IST+1)
        LST = 15*(I-1)+IWRDS
        IF ( IIFLAG ) WRITE(6,600) (IWORK(J),J=IST,LST)
        IF ( IZFLAG ) WRITE(6,610) (IWORK(J),J=IST,LST)
 600    FORMAT(10X,15I7)
 610    FORMAT(10X,15Z7)
        IST = IST+15
 40   CONTINUE
C  
 50   RETURN
      END
