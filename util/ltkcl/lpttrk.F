CC**********************************************************************
C*
C*=========================----==
C*  Subrouitne LPTTRK(NTRK,IRET)
C*=========================----==
C*
C* (Purpose)
C*     Output lepton track candidates.
C* (Inputs)
C*     NTRK   ; track number.
C* (Outputs)
C*     NTRK   ; track number.
C*     IRET   ; return flag.
C* (Relation)
C*     Invoked by
C*     Calls
C* (Update record)
C*    93/05/10  K. Fujii               Original version.
C*
CC**********************************************************************
 
      SUBROUTINE LPTTRK(NTRK,IRET)
 
#include "prjunk.inc"
#include "smrhgo.inc"
      INTEGER*4 NTRK, IRET
      PARAMETER ( LNxLTK = 12  )
      COMMON /SSOWRK/ RLTKBF(1000)
      CHARACTER*64 BKNAM1 / 'Production:Combined_Lepton_Track' /
      CHARACTER*64 BKNAM2 / 'Production:Combined_Hadron_Track' /
C
C==================< Entry Point >======================================
C
C--
C  Reset return flag.
C--
      IRET = 1
C--
C  TBCRTE track bank.
C--
      CALL TBCRTE(1,BKNAM1,0,0,IRT)
      IF ( NTRK.EQ.0 ) THEN
         CALL TBCRTE(1,BKNAM2,0,0,IRT)
      ENDIF
C--
C  Reset lepton track counter.
C--
      MTRK = 0
C--
C  First output apparent electrons.
C--
      DO 100 ITK = 1, NCDCTK
         IF ( KCNTRK(ITK).EQ.11 ) THEN
            ITYP = 11
            ISRC = 1
            MTRK = MTRK + 1
            CALL UVZERO(LNxLTK,RLTKBF)
            RLTKBF( 1) = RTKBNK(1,ITK)
            RLTKBF( 2) = RTKBNK(2,ITK)
            RLTKBF( 3) = RTKBNK(3,ITK)
            RLTKBF( 4) = RTKBNK(4,ITK)
            RLTKBF( 5) = RTKBNK(8,ITK)
            RLTKBF( 8) = ITKBNK(57,ITK)
            RLTKBF( 9) = SIGN(1.E0,RTKBNK(13,ITK))
            RLTKBF(10) = ITYP
            RLTKBF(11) = ISRC
            RLTKBF(12) = 1
            RLTKBF(13) = ITK
            CALL TBPUT(1,BKNAM1,MTRK,LNxLTK+1,RLTKBF,IRT)
         ENDIF
100   CONTINUE
C--
C  Then output apparent muons.
C--
      DO 200 ITK = 1, NCDCTK
         IF ( KCNTRK(ITK).EQ.13 ) THEN
            ITYP = 13
            ISRC = 1
            MTRK = MTRK + 1
            CALL UVZERO(LNxLTK,RLTKBF)
            RLTKBF( 1) = RTKBNK(1,ITK)
            RLTKBF( 2) = RTKBNK(2,ITK)
            RLTKBF( 3) = RTKBNK(3,ITK)
            RLTKBF( 4) = RTKBNK(4,ITK)
            RLTKBF( 8) = ITKBNK(57,ITK)
            RLTKBF( 9) = SIGN(1.E0,RTKBNK(13,ITK))
            RLTKBF(10) = ITYP
            RLTKBF(11) = ISRC
            RLTKBF(12) = 1
            RLTKBF(13) = ITK
            CALL TBPUT(1,BKNAM1,MTRK,LNxLTK+1,RLTKBF,IRT)
         ENDIF
200   CONTINUE
C--
C  Output unused tracks. These unused tracks are most likely
C  low momentum isolated muons or low momentum loopers which
C  hit none of calorimetric devices.
C--
      DO 300 ITK = 1, NCDCTK
         IF ( KCNTRK(ITK).EQ.0 ) THEN
            ITYP = 6
            ISRC = 1
            NTRK = NTRK + 1
            CALL UVZERO(LNxLTK,RLTKBF)
            RLTKBF( 1) = RTKBNK(1,ITK)
            RLTKBF( 2) = RTKBNK(2,ITK)
            RLTKBF( 3) = RTKBNK(3,ITK)
            RLTKBF( 4) = RTKBNK(4,ITK)
            RLTKBF( 8) = ITKBNK(57,ITK)
            RLTKBF( 9) = SIGN(1.E0,RTKBNK(13,ITK))
            RLTKBF(10) = ITYP
            RLTKBF(11) = ISRC
            RLTKBF(12) = 1
            RLTKBF(13) = ITK
            CALL TBPUT(1,BKNAM2,NTRK,LNxLTK+1,RLTKBF,IRT)
         ENDIF
300   CONTINUE
C--
C  That's it.
C--
      IRET = 0
      RETURN
      END
