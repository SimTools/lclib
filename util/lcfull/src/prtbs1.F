CC*********************************************************************C
C*                                                                     *
C*====================                                                 *
C* Subroutine PRTBS1(NRET)                                             *
C*====================                                                 *
C*  (Purpose)                                                          *
C*       Initialize TBS, Bank System.                                  *
C*       Does TBS initialize and allocate Begin_Run, Event and End_Run *
C*       records.
C*  (Author)                                                           *
C*       A. Miyamoto    26-Aug-1985                                    *
C*       A. Miyamoto    29-Jan-1986  Use TBSHAR option.                *
C*                                   All DSB is shared, Except ENVMONcn*
C*       K. Fujii       03-Jun-1987  Now sizes of TBSBUF and SSWORK are*
C*                                   determined by this routine.       *
C*       K. Fujii       20-Oct-1987  Define IDREC=10/11.               *
C*       A. Miyamoto    14-Sep-1989  Copied from PRTBSI for FULL sim.  *
C*       A. Miyamoto     9-Jun-1990  Size of MXxTBS increased.         *
C*                                                                     *
CC**********************************************************************
C
      SUBROUTINE PRTBS1(NRET)
C
C>>>
C=EXPAND 'T#PR.P#STER.FORT(@MXxTBS)'
C
C      COMMON /TBSBUF/ IBUFF( MXxTBS)
C      COMMON /SSWORK/ KBUFF( MXxSSW)
C       PARAMETER( MXxTBS = 600000, MXxSSW = 200000 )
C940211      PARAMETER( MXxTBS = 700000, MXxSSW = 200000 )
      PARAMETER( MXxTBS = 900000, MXxSSW = 200000 )
      COMMON /TBSBUF/ IBUFF( MXxTBS)
      COMMON /SSWORK/ KBUFF( MXxSSW)
C>>>
C
C==================<<  Entry Point  >>================================
C
C
C---------------------- TBS Initialize.
C
#ifdef  MSP
       CALL TBSIZE(MXxTBS, MXxSSW)
#endif
#ifdef  UNIX
      CALL TBSIZEx(MXxTBS, MXxSSW)
#endif
CCC   CALL TBWARN( 11)
      NRET = 0
C
C---------------------- Allocate DSB.
C
C
      CALL TBALOC('Begin_Run', 2, 15, 40,10000, IRET )
      IF(IRET.LT.0) THEN
         WRITE(MESAGE,'(''Error to TBALOC "Begin_run"...IRET='',
     >                    I5,)') IRET
         GO TO 9000
      ENDIF
C
      CALL TBALOC('End_Run', 8,10,  20,5000, IRET )
      IF(IRET.LT.0) THEN
         WRITE(MESAGE,'(''Error to TBALOC "End_run"...IRET='',
     >                    I5,)') IRET
         GO TO 9000
      ENDIF
C
      CALL TBALOC('Event',   1, 75, 2400, 400000, IRET)
      IF(IRET.LT.0) THEN
         WRITE(MESAGE,'(''Error to TBALOC "Event"...IRET='',
     >                    I5,)') IRET
         GO TO 9000
      ENDIF
C
      RETURN
C
C (9) Error stop.
C
9000  CONTINUE
      CALL UERROR(3,DUMMY,1,'PRTBS1',MESAGE)
      NRET = -9999
      RETURN
      END
