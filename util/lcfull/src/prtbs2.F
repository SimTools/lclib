CC*********************************************************************C
C*                                                                     *
C*====================                                                 *
C* Subroutine PRTBS2(NRET)                                             *
C*====================                                                 *
C*  (Purpose)                                                          *
C*       To declare DBS share with event records.                      *
C*  (Relation)                                                         *
C*       Called by                                                     *
C*           PRJOBI : Production job initialization.                   *
C*       Call the following subroutines                                *
C*           TBSIZE : TBS Initialize.                                  *
C*           TBALOC : Allocate DSB.                                    *
C*  (Author)                                                           *
C*       A. Miyamoto    26-Aug-1985                                    *
C*       A. Miyamoto    29-Jan-1986  Use TBSHAR option.                *
C*                                   All DSB is shared, Except ENVMONcn*
C*       K. Fujii       03-Jun-1987  Now sizes of TBSBUF and SSWORK are*
C*                                   determined by this routine.       *
C*       K. Fujii       20-Oct-1987  Define IDREC=10/11.               *
C*       A. Miyamoto     8-Sep-1989  To FULLnize simulator.            *
C*                                   TBALOC Event when GEN. mode.      *
C*       A. Miyamoto    14-Sep-1989  Copied from PRTBSI                *
C*                                                                     *
CC**********************************************************************
C
      SUBROUTINE PRTBS2(NRET)
C
C---------------------- Parameter for the banks.
C
      PARAMETER    ( NUMDSB = 10 )
      CHARACTER*64   DSBNAM(NUMDSB)
      CHARACTER*80   MESAGE
      INTEGER*4      IDDSB(NUMDSB)
      DATA  (IDDSB(I),DSBNAM(I),I=1,NUMDSB)/
     >   1, 'Event',
     >   2, 'Begin_Run',
     >   3, 'System_Configuration',
     >   4, 'Environment_Monitor',
     >   6, 'Luminosity_Monitor',
     >   7, 'Status_Information',
     >   8, 'End_Run',
     >   9, 'Format_Error',
     >  10, 'Begin_Separator',
     >  11, 'End_Separator'/
C
C==================<<  Entry Point  >>================================
C
C
C   Allocate Shared DSB.
C
        DO 100 I = 1, NUMDSB
           IF( IDDSB(I) .EQ. 1 ) GO TO 100
           IF( IDDSB(I) .EQ. 2 ) GO TO 100
           IF( IDDSB(I) .EQ. 8 ) GO TO 100
           CALL TBSHAR(1, DSBNAM(I), IDDSB(I), IRET)
           IF(IRET.LT.0) THEN
             ILDSB = MAX(INDEX(DSBNAM(I),' ')-1,1)
             WRITE(MESAGE,'(''Error to TBSHAR '',A,'' .. IRET='',I5)')
     >                      DSBNAM(I)(:ILDSB), IRET
             GO TO 9000
           ENDIF
100     CONTINUE
        RETURN
C
C
C (9) Error stop.
C
9000  CONTINUE
      CALL UERROR(3,DUMMY,1,'PRTBS2',MESAGE)
      NRET = -9999
      RETURN
      END
