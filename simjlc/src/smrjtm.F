      SUBROUTINE SMRJTM
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
C  Print out CPU times.
C--
C>>>
#ifdef TIME
      PRINT *, ' ***** CPU Time Summary ****** '
#endif 
#ifdef TIME
      PRINT *, '   SMRTBG     = ', TIMER(1)
#endif 
#ifdef TIME
      PRINT *, '   SMRVTX     = ', TIMER(2)
#endif 
#ifdef TIME
      PRINT *, '   SMRCHT     = ', TIMER(3)
#endif 
#ifdef TIME
      PRINT *, '   SMREMC     = ', TIMER(4)
#endif 
#ifdef TIME
      PRINT *, '   SMRHDC     = ', TIMER(5)
#endif 
#ifdef TIME
      PRINT *, '   SMRCBK(EM) = ', TIMER(6)
#endif 
#ifdef TIME
      PRINT *, '   SMRCBK(HD) = ', TIMER(7)
#endif 
#ifdef TIME
      PRINT *, '   SMRCVT     = ', TIMER(8)
#endif 
#ifdef TIME
      PRINT *, '   SMREMC-1   = ', TIMER(11)
#endif 
#ifdef TIME
      PRINT *, '   TBPUT(EM)  = ', TIMER(12)
#endif 
#ifdef TIME
      PRINT *, '   SMRHDC-1   = ', TIMER(16)
#endif 
#ifdef TIME
      PRINT *, '   TBPUT(HD)  = ', TIMER(17)
#endif 
C>>>
C--
C  That's it.
C--
      RETURN
      END
