CC*********************************************************************C
C*                                                                     *
C*====================                                                 *
C* Subroutine PRPEVT(NRECID, NRET)                                     *
C*====================                                                 *
C*(Purpose)                                                            *
C*   Output data to the file.                                          *
C*(Input)                                                              *
C*   NRECID  : Output record. ID.                                      *
C*(Output)                                                             *
C*   NRET    : Return flag.                                            *
C*         > 0 Normal                                                  *
C*         =-2000 Output data size file.                               *
C*         <    0 TBWRIT Error.                                        *
C*(Relation)                                                           *
C*   Called from PRCEVT
C*(Author)                                                             *
C*   A. Miyamoto  27-Apr-1987  Original version.                       *
C*                                                                     *
C*   Temporary modification for the beam DST production.               *
C*   A. Miyamoto  03-Jun-1987  Save output size.                       *
C*                                                                     *
C*                22-Aug-1988  Call PRPEVO, for user option before     *
C*                             output record.                          *
C*                                                                     *
CC**********************************************************************
C
       SUBROUTINE PRPEVT(NRECID, NRET)
C
#include "prfctl.inc"
C
#include "prdstr.inc"
#include "prouts.inc"
#include "prjpr1.inc"
#ifdef  UNIX
#include "prdyna.inc"
#endif
 
C
      CHARACTER*80    MESAGE
      COMMON /SSWORK/ MESAGE
      DATA    OUTTPP, OUTDSP/0., 0./
      DATA    KOUTBG/0/, KOUTEV/0/
 
#ifdef  UNIX
      INTEGER*4   NOPNFL
#endif
#ifdef  UNIX
      DATA  NOPNFL/0/
#endif
#ifdef  UNIX
      SAVE  NOPNFL
#endif
#ifdef G77_521_OR_GREATER 
      LOGICAL IEXIST
#endif

C===========================<< Entry point >>==========================
C
C----------------------------------------------------------------------
C (1) Output data.
C----------------------------------------------------------------------
C
      NRET = 0
C
      CALL PRPEVO( NOTUNI, NRECID, IRUSER )
      IF( IRUSER .LT. 0 ) THEN
        NRET = -2000
        WRITE(6,110) IRUSER
110     FORMAT(//,
     >' ************************************************************',/,
     >' *   Job will terminate due to the resuest by user routine. *',/,
     >' *   PRPEVO, whose return code was ',I6,'                   *',/,
     >' ************************************************************',/,
     >   //)
        RETURN
      ENDIF
C
C
C----------------------------------------------------------------------
C (1.1) If NALLWT .EQ. 0 does not output Begin_Run and End_Run records
C      when no event records are written.
C----------------------------------------------------------------------
C
      IF( NALLWT .EQ. 0 ) THEN
        IF( NRECID .EQ. 2 ) THEN
          KOUTBG = 1
          KOUTEV = 0
          RETURN
        ELSEIF( NRECID .EQ. 8 ) THEN
          IF( KOUTEV .LE. 0 ) THEN
            KOUTEV = 0
            KOUTBG = 0
            RETURN
          ELSE
            GO TO 150
          ENDIF
        ELSE
          IF( KOUTEV .EQ. 0 .AND. KOUTBG .EQ. 1 ) THEN
            CALL TBSQEZ( 2, IRET )
            CALL PROBNK( 2, ' ', ' ')
C900213            CALL SSDBNK( 2, ' ', ' ')
            CALL PRDBNK( 2, ' ', ' ')
#ifdef  UNIX
            IF( NOPNFL.EQ.0 .AND. NOFALL.NE.0  ) THEN
               NOPNFL = NOPNFL + 1
#ifdef G77_521_OR_GREATER 
               INQUIRE(FILE=OUTFIL(NOPNFL),EXIST=IEXIST)
               IF( IEXIST) GO TO 9010
               OPEN(NOTUNI,FILE=OUTFIL(NOPNFL),FORM='UNFORMATTED',
     >              ERR=9010)
c  status=new is forced in the case of g77 to aviod trouble.
#else
               OPEN(NOTUNI,FILE=OUTFIL(NOPNFL),FORM='UNFORMATTED')
#endif
               CALL TBWRIT( NOTUNI, 2, NRET )
            ELSE
               NRET = 0
            ENDIF
#endif
#ifdef  MSP
            CALL TBWRIT( NOTUNI, 2, NRET )
#endif
            IF( NRET .LT. 0 ) THEN
              PRINT *,'%Error in PRPEVT... ',
     >                ' Error to TBWRIT Begin_Run record.'
              RETURN
            ENDIF
            KOUTBG = 2
            KOUTEV = KOUTEV + 1
          ENDIF
        ENDIF
      ENDIF
 
150   CONTINUE
      CALL TBSQEZ(NRECID, IRET)
      CALL PROBNK(NRECID, ' ', ' ')
C900213      CALL SSDBNK(NRECID, ' ', ' ')
      CALL PRDBNK(NRECID, ' ', ' ')
#ifdef  UNIX
          IF( NOFALL.NE.0 ) THEN
             IF(NOPNFL.EQ.0 ) THEN
               NOPNFL = NOPNFL + 1
#ifdef G77_521_OR_GREATER 
               INQUIRE(FILE=OUTFIL(NOPNFL),EXIST=IEXIST)
               IF( IEXIST) GO TO 9010
               OPEN(NOTUNI,FILE=OUTFIL(NOPNFL),FORM='UNFORMATTED',
     >              ERR=9010)
c  status=new is forced in the case of g77 to aviod trouble.
#else
               OPEN(NOTUNI,FILE=OUTFIL(NOPNFL),FORM='UNFORMATTED')
#endif
             ENDIF
             CALL TBWRIT( NOTUNI, NRECID, NRET )
          ELSE
             NRET = 0
          ENDIF
#endif
#ifdef  MSP
      CALL TBWRIT(NOTUNI, NRECID, NRET)
#endif
 
      IF( NRET .LT. 0) RETURN
 
C
C----------------------------------------------------------------------
C (2) Countup output data size, if output data size is over max,
C     prepare next file.
C----------------------------------------------------------------------
C
      CALL UCTLSZ(NRET, TAPSIZ, DSKSIZ)
C>>>> TAYM.
      DSTSIZ = TAPSIZ
      OUTTAP = OUTTPP + TAPSIZ
      OUTDSK = OUTDSP + DSKSIZ
C
      IF(   XOSIZE .LT. 0. ) RETURN
      IF( ( NOTDEV .EQ. 0 .AND. DSKSIZ  .LT. XOSIZE ) .OR.
     >    ( NOTDEV .EQ. 1 .AND. TAPSIZ  .LT. XOSIZE ) ) RETURN
 
      IOTFIL = IOTFIL + 1
 
      IF(IOTFIL .GT. NOTFIL ) THEN
        NRET = -2000
        WRITE(6,210) TAPSIZ
210     FORMAT(//,
     >' ************************************************************',/,
     >' *   Job will terminate due to output data size over        *',/,
     >' *   Output size of last file is ',F10.2,' feet.            *',/,
     >' ************************************************************',/,
     >   //)
        RETURN
      ENDIF
 
#ifdef  MSP
      ENDFILE NOTUNI
#endif
#ifdef  UNIX
            IF( NOFALL.EQ.0 ) THEN
               ENDFILE NOTUNI
            ELSEIF( NOPNFL .LT. NOFALL ) THEN
#ifdef G77_521_OR_GREATER 
               call closelcfull(notuni)
#endif
               CLOSE(NOTUNI)
               NOPNFL = NOPNFL + 1
#ifdef G77_521_OR_GREATER 
               INQUIRE(FILE=OUTFIL(NOPNFL),EXIST=IEXIST)
               IF( IEXIST) GO TO 9010
               OPEN(NOTUNI,FILE=OUTFIL(NOPNFL),FORM='UNFORMATTED',
     >              STATUS='NEW',ERR=9010)
c  status=new is forced in the case of g77 to aviod trouble.
#else
               OPEN(NOTUNI,FILE=OUTFIL(NOPNFL),FORM='UNFORMATTED')
#endif
            ELSE
               PRINT *,'In PRPEVT .. no more output file space.'
               NRET = -2100
            ENDIF
#endif
 
      OUTTPP = OUTTAP
      OUTDSP = OUTDSK
 
      CALL UCTLSZ(-1, TAPSIZ, DSKSIZ)
 
      RETURN
#ifdef G77_521_OR_GREATER 
 9010 continue
      print *,'The job terminated since the output file '
      print *,'   ', OUTFIL(NOPNFL)(:LNBLNK(OUTFIL(NOPNFL)))
      print *,'can not be opened.  It may be exists already.'
      stop
#endif
      END
