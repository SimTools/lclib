CC********************************************************************CC
C*                                                                    *C
C*=================                                                   *C
C*   / PRRSTA /                                                       *C
C*=================                                                   *C
C*                                                                    *C
C* 1) Contents                                                        *C
C*  This COMMON contains statistical information for each Record.     *C
C*    NRDREC      : # DSB's read in.                                  *C
C*    NWTREC      : # DSB's written.                                  *C
C*    NSTREC(1,j) : # read in                  --+                    *C
C*          (2,j) : # processed                  +-- for REC of       *C
C*          (3,j) : # processed w/o error        !   j-th kind.       *C
C*          (4,j) : # processed w/  warning      !                    *C
C*          (5,j) : # processed w/  fatal error  !                    *C
C*          (6,j) : # written                  --+                    *C
C*                                                                    *C
C*    Mxxxxx   is the statistics at the begining of run.              *C
C*                                                                    *C
C*                                                                    *C
C* 2) Update Record                                                   *C
C*    04/30/87  A. Miyamoto  Original Version.                        *C
C*                                                                    *C
CC********************************************************************CC
      COMMON /PRRSTA/ NRDREC,NWTREC,NSTREC(6,10),
     >                MRDREC,MWTREC,MSTREC(6,10)
C*
