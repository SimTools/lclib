C***********************************************************************
C*
C*=======================================----===
C* Subroutine SMREVT(IRECID,LEVEL,IDEBUG,IRET)
C*=======================================----===
C*
C* (Purpose)
C*     Smear Generator:Particle_List.
C* (Inputs)
C*        IDREC    : (I*4) :  record ID.
C*        LEVEL    : (I*4) :  level flag.
C*        IDEBUG   : (I*4) :  debug flag.
C* (Output)
C*        IRET     : (I*4) :  return flag.
C* (Relation)
C*     Called by steering.
C*     Calls the following routines;
C*        SMRTBG, SMRCHT, SMREMC, SMRHDC.
C* (Updaterecord)
C*     4/18/90  K.Fujii           Original version.
C*     2/04/91  K.Fujii           Added VTX smearing.
C*
C***********************************************************************
 
      SUBROUTINE SMREVT(IRECID,LEVEL,IDEBUG,IRET)
 
C>>>>
#ifdef TIME
      PARAMETER ( MXxTIM = 20 )
#endif 
#ifdef TIME
      COMMON /TIMER/ TIMER(MXxTIM)
#endif 
C>>>>
C
C========< Entry Point >================================================
C
C--
C  Check record ID.
C--
      IRET = 0
      IF ( IRECID.NE.1 ) RETURN
C--
C  Store generator data in /PRJUNK/.
C--
C>>>
#ifdef TIME
      CALL CLOCK(T1,2,2)
#endif 
C>>>
      CALL SMRTBG(LEVEL,IDEBUG,IRT)
C>>>
#ifdef TIME
      CALL CLOCK(T2,2,2)
#endif 
#ifdef TIME
      TIMER(1) = TIMER(1) + T2 - T1
#endif 
C>>>
      IF ( IRT.LT.IRET ) THEN
         IRET = IRT
         RETURN
      ENDIF
C--
C  Create vertex detector space point bank.
C--
C>>>
#ifdef TIME
      CALL CLOCK(T1,2,2)
#endif 
C>>>
      CALL SMRVTX(LEVEL,IDEBUG,IRT)
C>>>
#ifdef TIME
      CALL CLOCK(T2,2,2)
#endif 
#ifdef TIME
      TIMER(2) = TIMER(2) + T2 - T1
#endif 
C>>>
      IF ( IRT.LT.IRET ) IRET = IRT
C--
C  Create charged track bank.
C--
C>>>
#ifdef TIME
      CALL CLOCK(T1,2,2)
#endif 
C>>>
      CALL SMRCHT(LEVEL,IDEBUG,IRT)
      IF ( IRT.LT.IRET ) IRET = IRT
C>>>
#ifdef TIME
      CALL CLOCK(T2,2,2)
#endif 
#ifdef TIME
      TIMER(3) = TIMER(3) + T2 - T1
#endif 
C>>>
C--
C  Create EM cluster bank.
C--
C>>>
#ifdef TIME
      CALL CLOCK(T1,2,2)
#endif 
C>>>
      CALL SMREMC(LEVEL,IDEBUG,IRT)
C>>>
#ifdef TIME
      CALL CLOCK(T2,2,2)
#endif 
#ifdef TIME
      TIMER(4) = TIMER(4) + T2 - T1
#endif 
C>>>
      IF ( IRT.LT.IRET ) IRET = IRT
C--
C  Create HD cluster bank.
C--
C>>>
#ifdef TIME
      CALL CLOCK(T1,2,2)
#endif 
C>>>
      CALL SMRHDC(LEVEL,IDEBUG,IRT)
C>>>
#ifdef TIME
      CALL CLOCK(T2,2,2)
#endif 
#ifdef TIME
      TIMER(5) = TIMER(5) + T2 - T1
#endif 
C>>>
      IF ( IRT.LT.IRET ) IRET = IRT
C--
C  Create EM cluster bank.
C--
C>>>
#ifdef TIME
      CALL CLOCK(T1,2,2)
#endif 
C>>>
      CALL SMRCBK(  1, IRT )
C>>>
#ifdef TIME
      CALL CLOCK(T2,2,2)
#endif 
#ifdef TIME
      TIMER(6) = TIMER(6) + T2 - T1
#endif 
C>>>
      IF ( IRT.LT.IRET ) IRET = IRT
C--
C  Create HD cluster bank.
C--
C>>>
#ifdef TIME
      CALL CLOCK(T1,2,2)
#endif 
C>>>
      CALL SMRCBK(  2, IRT )
C>>>
#ifdef TIME
      CALL CLOCK(T2,2,2)
#endif 
#ifdef TIME
      TIMER(7) = TIMER(7) + T2 - T1
#endif 
C>>>
#ifdef C
   IF ( IRT.LT.IRET ) IRET = IRT
#endif 
C--
C  Create CDC_VTX combined track bank.
C--
C>>>
#ifdef TIME
      CALL CLOCK(T1,2,2)
#endif 
C>>>
      CALL SMRCVT(LEVEL,IDEBUG,IRT)
C>>>
#ifdef TIME
      CALL CLOCK(T2,2,2)
#endif 
#ifdef TIME
      TIMER(8) = TIMER(8) + T2 - T1
#endif 
C>>>
      IF ( IRT.LT.IRET ) IRET = IRT
C--
C  That's it.
C--
      RETURN
      END
