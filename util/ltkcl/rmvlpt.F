CC**********************************************************************
C*
C*===========================
C*  Subrouitne RMVLPT(IRET)
C*===========================
C*
C* (Purpose)
C*     Poison lepton tracks.
C* (Inputs)
C* (Outputs)
C*     IRET   ; return flag.
C* (Relation)
C*     Invoked by
C*     Calls
C* (Update record)
C*    93/05/10  K. Fujii               Temporary version using
C*                                     generator information.
C*                                     This implies that leptons
C*                                     can be identified by external
C*                                     devices such as MDC and TRD.
C*
CC**********************************************************************
 
      SUBROUTINE RMVLPT(IRET)
 
#include "prjunk.inc"
C--
      COMMON /SSWORK/ RDATA(1000)
      CHARACTER*64 BKNAME / 'Generator:Particle_List' /
      DATA  ECUTE     / 1.0 /
      DATA  ECUTM     / 3.5 /
C
C==================< Entry Point >======================================
C
C--
C  Reset return flag.
C--
      IRET = 1
C--
C  Loop over CDC tracks.
C--
      DO 1000 ITK = 1, NCDCTK
         IEGEN = ITKBNK(57,ITK)
         CALL TBGET(1,BKNAME,IEGEN,NW,RDATA,IRT)
         ID    = RDATA(2)
         IDA   = IABS(ID)
         E     = RTKBNK(4,ITK)
         IF ( IDA.EQ.11 ) THEN
            IF ( E.GT.ECUTE ) KCNTRK(ITK) = 11
         ELSE IF ( IDA.EQ.13 ) THEN
            IF ( E.GT.ECUTM ) KCNTRK(ITK) = 13
         ENDIF
1000  CONTINUE
C--
C  That's it.
C--
      IRET = 0
9999  RETURN
      END
