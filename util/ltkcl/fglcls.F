CC**********************************************************************
C*
C*===========================
C*  Subrouitne FGLCLS(IRET)
C*===========================
C*
C* (Purpose)
C*     Find 2-dimimensional clusters.
C* (Inputs)
C* (Outputs)
C*     IRET   ; return flag.
C* (Relation)
C*     Invoked by
C*     Calls
C* (Update record)
C*     7/09/92  K. Fujii               Original version.
C*
CC**********************************************************************
 
      SUBROUTINE FGLCLS(IRET)
 
#include "prjunk.inc"
      PARAMETER ( MXxSSW = 100000 )
      COMMON /SSWORK/  IOUT(MXxSSW)
      INTEGER*4 IOUT
      INTEGER*4 IZB(2), IFB(2)
C
C==================< Entry Point >======================================
C
C--
C  Loop over bin data to find a starting bin.
C--
      DO 100 JBN = 1, NBNON
         NCL = NCLIN(JBN)
         IF ( NCL.GT.0 )                         GO TO 150
100   CONTINUE
C--
C  Cluster exhausted. Set return flag and return.
C--
      IRET = 2
      RETURN
C--
C  Starting bin found. Now find a cluster.
C--
150   IZF = IBNPNT(1,JBN)
      IFF = IBNPNT(2,JBN)
C>>>
#ifdef CP
      PRINT *, ' JBN, IZF, IFF = ', JBN, IZF,IFF
#endif 
#ifdef CP
      PRINT *, ' NCLIN(JBN) = ', NCLIN(JBN)
#endif 
#ifdef CP
      PRINT *, ' JBNPNT(IZF,IFF) = ', JBNPNT(IZF,IFF)
#endif 
#ifdef CP
      PRINT *, '  '
#endif 
#ifdef CP
      IZMN = MAX(0,IZF-10)
#endif 
#ifdef CP
      IZMX = MIN(MXxZET,IZF+20)
#endif 
#ifdef CP
      IXMN = IFF - 9
#endif 
#ifdef CP
      IXMX = IFF + 20
#endif 
#ifdef CP
      DO 666 K = IXMN, IXMX
#endif 
#ifdef CP
         IF ( K.LT.0 ) THEN
#endif 
#ifdef CP
            J = K + MXxPHI + 1
#endif 
#ifdef CP
         ELSE IF ( K.GT.MXxPHI ) THEN
#endif 
#ifdef CP
            J = K - MXxPHI - 1
#endif 
#ifdef CP
         ELSE
#endif 
#ifdef CP
            J = K
#endif 
#ifdef CP
         ENDIF
#endif 
#ifdef CP
         WRITE(6,'(13(2X,41I1))') (JBNPNT(I,J),I=IZMN,IZMX)
#endif 
#ifdef CP
666   CONTINUE
#endif 
#ifdef CP
      PRINT *, '  '
#endif 
C>>>
      CALL UFCLST(IZF,IFF,0,MXxZET,0,MXxPHI,JBNPNT,IZB,IFB,IOUT)
C>>>
#ifdef CP
      CALL CHKCLS(IZB,IFB,IOUT)
#endif 
C>>>
C--
C  Poison this cluster while filling IWORK.
C--
      IWORK(1) = IOUT(1)
      IPNT     = 2*IOUT(1) + 1
      DO 200 IFI = 2, 2*IOUT(1), 2
         JPHI = IOUT(IFI  )
         IPT  = IOUT(IFI+1)
         NCL  = IOUT(IPT)
C--
         IF ( JPHI.LT.0 ) THEN
            IPHI = JPHI + MXxPHI + 1
         ELSE IF ( JPHI.GT.MXxPHI ) THEN
            IPHI = JPHI - MXxPHI - 1
         ELSE
            IPHI = JPHI
         ENDIF
C--
         IWORK(IFI  ) = JPHI
         IPNT         = IPNT + 1
         IWORK(IFI+1) = IPNT
         KPNT         = IPNT
         IWORK(KPNT ) = 0
         DO 20 ICL = 1, NCL
            DO 2 IZT = IOUT(IPT+1), IOUT(IPT+1) + IOUT(IPT+2) - 1
               JBN = JBNPNT(IZT,IPHI)
               IF ( JBN.EQ.0 )                   GO TO 2
               JCL = NCLIN(JBN)
               IWORK(KPNT  ) = IWORK(KPNT) + 1
               IWORK(IPNT+1) = ICLPNT(1,JCL)
               IWORK(IPNT+2) = ICLPNT(2,JCL)
               IWORK(IPNT+3) = ICLPNT(5,JCL)
C>>>>
#ifdef CP
         PRINT *,' JCL = ', JCL, ' IPNT = ', IPNT
#endif 
#ifdef CP
         PRINT *,' IWORK(IPNT+1) = ',IWORK(IPNT+1)
#endif 
#ifdef CP
         PRINT *,'      (IPNT+2) = ',IWORK(IPNT+2)
#endif 
#ifdef CP
         PRINT *,'      (IPNT+3) = ',IWORK(IPNT+3)
#endif 
C>>>>
               JBNPNT(IZT,IPHI) = 0
               NCLIN(JBN)       = 0
               IPNT             = IPNT + 3
2           CONTINUE
            IPT  = IPT  + 2
20       CONTINUE
200   CONTINUE
C--
C  Set return flag and return.
C--
      IRET = 0
C--
      RETURN
      END
