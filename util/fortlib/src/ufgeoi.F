C**********************************************************************
C* 
C*  --------------------------------------------------------=========
C*  Subroutine UFGEOI( NTRK, LNxTRK, TRKINI, DRKINI, VTXINI, X, NRET)
C*  --------------------------------------------------------=========
C* 
C*(Function)
C*   Called from PGFTRK to store initial parameter for the geometrical
C*   constraint fit to the buffer.
C* 
C*(Input)
C*   NTRK    ; # of track.
C*   LNxTRK  ; Length of track parameter in I*4 unit.
C*   TRKINI  ; Initial track parameter with TPC;Track_Parameter format.
C*   DRKINI  ; Track parameter by Real*8 format.
C*   VTXINI  ; Initial position of the vertex.
C* 
C*(Output)
C*   X       ; Parameter array ( Inital values are set.)
C*   NRET    ; Return code.
C*          = 0 when normal return
C*          < 0 when failed to invert matrix.
C* 
C*(Author)
C*  A. Miyamoto  24-Jun-1987
C* 
C**********************************************************************
C* 
      SUBROUTINE UFGEOI( NTRK, LNxTRK, TRKINI, DRKINI, VTXINI, X, NRET)
C  
      REAL*4  TRKINI(LNxTRK, NTRK), VTXINI(3)
      REAL*8  DRKINI(LNxTRK/2, NTRK)
      REAL*8  X(*)
C  
#include "ufgeoc.inc"
#include "ufgeop.inc"
#include "prfeld.inc"
C  
C Local variables.
C  
      REAL*8          WRKMTX(10, 10)
      REAL*8          DRHO, DXC, DYC
      INTEGER*4       IMTRX(2, 15)
      DATA            IMTRX/ 1,1,  1,2,  1,3,  1,4,  1,5,
     >                             2,2,  2,3,  2,4,  2,5,
     >                                   3,3,  3,4,  3,5,
     >                                         4,4,  4,5,
     >                                               5,5/
      EXTERNAL PRBLK1
C  
C =====< Entry Point >=================================================
C  
#ifdef HIUXF77
      CALL PRBLK1
#endif
      DPTOR  = PTOR
C ---------------------------------------------------------------------
C (2) Loop by Trak to copy track parameter to /PGFBUF/
C ---------------------------------------------------------------------
C  
      NUMTRK = NTRK
      DO 200 ITRK = 1, NUMTRK
   
C (2.1) Copy Track parameters.
   
        DO 210 I = 1, 8
          TRKPAR(I,ITRK) = TRKINI(I,ITRK)
210     CONTINUE
   
C (2.2) Copy Error matrix to work array
   
        DO 220 IP = 5, 19
          I1          = IP- 4
          I           = IMTRX(1,I1)
          J           = IMTRX(2,I1)
          WRKMTX(I,J) = DRKINI(IP, ITRK)
          WRKMTX(J,I) = WRKMTX(I,J)
220     CONTINUE
   
C (2.3) Invert error matrix, and save to the work array.
   
        CALL MATINV( WRKMTX, 5, DET)
   
        DO 230 I = 1, 5
          DO 240 J = 1, 5
            TRKERR(I,J,ITRK) = WRKMTX(I,J)
240       CONTINUE
230     CONTINUE
C  
200   CONTINUE
C  
C ---------------------------------------------------------------------
C (3) Prepare Initial parameter array.
C ---------------------------------------------------------------------
C  
      X(1) = VTXINI(1)
      X(2) = VTXINI(2)
      X(3) = VTXINI(3)
C  
      DO 300 ITRK = 1, NTRK
        IP    = 3*(ITRK-1) + 3
        DRHO  = DPTOR/TRKPAR(3,ITRK)
        DXC   =  TRKPAR(6,ITRK) +
     >         ( DRHO + TRKPAR(1,ITRK) )*DCOS( TRKPAR(2,ITRK) )
        DYC   =  TRKPAR(7,ITRK) +
     >         ( DRHO + TRKPAR(1,ITRK) )*DSIN( TRKPAR(2,ITRK) )
        IF( DRHO .LT. 0.0D0 ) THEN
           X(IP+1) = DATAN2( -(DYC-X(2)), -(DXC-X(1)) )
        ELSE
           X(IP+1) = DATAN2(   DYC-X(2),    DXC-X(1)  )
        ENDIF
        X(IP+2)  = TRKPAR(3,ITRK)
        X(IP+3)  = TRKPAR(5,ITRK)
300   CONTINUE
C  
C ---------------------------------------------------------------------
C (5) Return to Caller.
C ---------------------------------------------------------------------
C  
      NRET = 0
C  
      RETURN
      END
