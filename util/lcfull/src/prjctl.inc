CC********************************************************************CC
C*                                                                    *C
C*=================                                                   *C
C*   / PRJCTL /                                                       *C
C*=================                                                   *C
C*                                                                    *C
C* 1) Contents                                                        *C
C*     Job control parameter area.
C*                                                                    *C
C* 2) Update Record                                                   *C
C*    10/14/85  A. Miyamoto  Original Version.                        *C
C*     4/25/89  A.Miyamoto   Change default to process Generator data.*C
C*                                                                    *C
CC********************************************************************CC
 
      COMMON /PRJCTL/ RTIMLM, RCPUMX,
     .                NEVMAX, NDFREQ, IEVSEQ,
     .                NFSTEV, NLSTEV, NFSTDB, NLSTDB, NFSTOT, NLSTOT,
     .                KLLVLR, KLLVLP,
     .                NFSTEX, NLSTEX, NFSTRN, NLSTRN
C*
C*    RTIMLM  ; Remaining CPU Time limit, at which terminate job.
C*    RCPUMX  ; Running CPU time max.
C*    NEVMAX  ; Event sequence max.
C*    NDFREQ  ; Dump frequency.
C*    IEVSEQ  ; Event sequence number.
C*    NFSTEV  ; First event sequence # to analize.
C*    NLSTEV  ; Last event # sequence to analize.
C*    NFSTDB  ; First event # sequence to analize.
C*    NLSTDB  ; Last event # seqnece to analize..
C*    NFSTOT  ; First event # to start output.
C*    NLSTOT  ; Last event # to stop output.
C*    KLLVLR  ; Event Kill level in Reduction step.
C*    KLLVLP  ; Event Kill level in Production step.
C*    NFSTEX  ; First Exp# to analize.
C*    NLSTEX  ; Last Exp# to analize.
C*    NFSTRN  ; First Run# to analize.
C*    NLSTRN  ; Last Run# to analize.
C*
C=    DATA   RTIMLM/15.0/
C=    DATA   RCPUMX/99999.999/
C=    DATA   NEVMAX/999999999/
C=    DATA   NDFREQ/        1/
C=    DATA   IEVSEQ/        0/
C=    DATA   NFSTEV/        1/
C=    DATA   NLSTEV/999999999/
C=    DATA   NFSTDB/        1/
C=    DATA   NLSTDB/999999999/
C=    DATA   NFSTOT/        1/
C=    DATA   NLSTOT/999999999/
C=    DATA   KLLVLR/       -9/
C=    DATA   KLLVLP/       -9/
C=    DATA   NFSTEX/-99999999/
C=    DATA   NLSTEX/ 99999999/
C=    DATA   NFSTRN/-99999999/
C=    DATA   NLSTRN/ 99999999/
