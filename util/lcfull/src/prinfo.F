CC********************************************************************CC
C*                                                                    *C
C*-----------------------======                                       *C
C* Subroutine PRINFO(ID, IANS )                                       *C
C*-----------------------======                                       *C
C*                                                                    *C
C*(Purpose)                                                           *C
C*     Returns production job information.                            *C
C*                                                                    *C
C*(Input)                                                             *C
C*   ID      :  Identification number of data.                        *C
C*    = 1   for Current experiment number.                            *C
C*    = 2   for Current run number.                                   *C
C*    = 3   for Current run sequence number.                          *C
C*    = 4   for Current event number.                                 *C
C*    = 5   for Current event sequence number.                        *C
C*    = 6   for Event number to start at.                             *C
C*    = 7   for Event number to stop at.                              *C
C*    = 8   for Production version/level #                            *C
C*    = 9   Original data flag.                                       *C
C*            = 0 for real data, passes reduction job.                *C
C*            = 1 for MC data, passes reduction job.                  *C
C*            = 2 for MC data, without reduction job.                 *C
C*    =10   Kill level of reduction step                              *C
C*    =11   for date of this event.                                   *C
C*    =12   for time of this event.                                   *C
C*    =13   Return run type                                           *C
C*    =14   Return Beam energy (MEV)                                  *C
C*    =31   Run type code in data base.                               *C
C*    =32   Run start date from data base.                            *C
C*    =33   Run start time from data base.                            *C
C*    =34   Run end   date from data base.                            *C
C*    =35   Run end   time from data base.                            *C
C*                                                                    *C
C*(Output)                                                            *C
C*   IANS    :  Answer.                                               *C
C*            = -9999  for ID error.                                  *C
C*                                                                    *C
C*(Relation)                                                          *C
C*   Called by  user routines.                                        *C
C*                                                                    *C
C*(Author)                                                            *C
C*   10/21/85  A. Miyamoto  Original version.                         *C
C*   02/10/88  K. Fujii     ID = 11, 12 added.                        *C
C*                                                                    *C
CC********************************************************************CC
 
      SUBROUTINE PRINFO( ID, IANS )
 
#include "prhead.inc"
#include "prjctl.inc"
#include "prdtyp.inc"
#include "prunls.inc"
      CHARACTER   VERDAT*8
      INTEGER*4   IEBEAM(55)/  -1 ,   -1 , 25000, 26000, 27500,
     >                       27500, 28000, 28000, 28250, 28500,
     >                       30000, 30000, 29250,    -1, 29525,
     >                       30000, 30400, 29500, 30400, 30000,
     >                       30400, 30700, 27000,    -1, 32000,
     >                       31800,    -1,    -1, 29000, 29000,
     >                       29000, 29000, 29000, 29000, 29000,
     >                       29000, 29000, 29000, 29000, 29000,
     >                       29000, 29850, 29100, 29590, 29230,
     >                       29350, 29475, 29725, 28800, 29000,
     >                           0,     0,     0,     0,     0/
      PARAMETER  (NOxRNR = 9 )
      INTEGER*4   KEBM14(3,NOxRNR)
      DATA        KEBM14/ 1, 7, 28625,   8, 15, 28750,  16, 24, 28875,
     >                   25,34, 29000,  35, 45, 29375,  46, 60, 29500,
     >                   61,77, 29625,  78, 87, 29750,  88, 98, 29565/
C
C ----------------------------------------------------------------------
C (1) Chack ID range.
C ----------------------------------------------------------------------
C
      IF( ID .GT. 30 )        GO TO 300
      IF(ID.LT.1.OR.ID.GT.14) GO TO 9000
C
C ----------------------------------------------------------------------
C (2) Switch according to ID.
C ----------------------------------------------------------------------
C
      GO TO ( 10, 20, 30, 40, 50, 60, 70, 80, 90, 100,
     >       110,120,130,140) ID
10    IANS = xEXPNO
      GO TO 10000
20    IANS = xRUNNO
      GO TO 10000
CC30    IANS = NRNSEQ
30    IANS = -9999
      GO TO 10000
40    IANS = xEVTNO
      GO TO 10000
50    IANS = IEVSEQ
      GO TO 10000
60    IANS = NFSTEV
      GO TO 10000
70    IANS = NLSTEV
      GO TO 10000
80    CALL PRGVRS( IVRS, ILVL, VERDAT )
      IANS = 100*IVRS + ILVL
      GO TO 10000
90    IF(NDTYPE(1).EQ.1) THEN
        IANS = 0
      ELSE
        IF(NDTYPE(7).NE.0) THEN
          IANS = 1
        ELSE
          IANS = 2
        ENDIF
      ENDIF
      GO TO 10000
C
100   CONTINUE
      IANS = KLLVLR
C     PRINT *,' PRINFO..IANS, KLLVLR=', IANS, KLLVLR
      GO TO 10000
110   IANS = xDATE
      GO TO 10000
120   IANS = xTIME
      GO TO 10000
130   IANS = xRUNTP
      GO TO 10000
140   CONTINUE
      IF( xEXPNO .EQ. 14 ) THEN
        DO 141 IRX = 1, NOxRNR
          IF( xRUNNO .GE. KEBM14(1,IRX) .AND.
     >        xRUNNO .LE. KEBM14(2,IRX) ) THEN
            IANS = KEBM14(3, IRX)
            GO TO 142
          ENDIF
141     CONTINUE
        IANS =  0
142     CONTINUE
      ELSE
        IANS = IEBEAM(xEXPNO)
      ENDIF
      GO TO 10000
C
C ----------------------------------------------------------------------
C (3) When data base access is required.
C ----------------------------------------------------------------------
C
300   CONTINUE
      IF( ID .GT. 35 ) GO TO 9000
      CALL PRLRNL( xEXPNO, xRUNNO, IRET )
      IF( IRET  .LT. 0 ) GO TO 9000
      IF( NWRNL .LT. 0 ) GO TO 9000
      IDNEW = ID - 30
      GO TO (310, 320, 330, 340, 350 ) IDNEW
C
310   CONTINUE
      IANS  = LRUNLS( 7 )
      GO TO 10000
C
320   CONTINUE
      IANS  = LRUNLS( 11)
      GO TO 10000
C
330   CONTINUE
      IANS  = LRUNLS( 12)
      GO TO 10000
C
340   CONTINUE
      IANS  = LRUNLS( 13)
      GO TO 10000
C
350   CONTINUE
      IANS  = LRUNLS( 14)
      GO TO 10000
C
C ----------------------------------------------------------------------
C (9) ID Value error.
C ----------------------------------------------------------------------
C
9000  CONTINUE
      IANS = -9999
C
C ----------------------------------------------------------------------
C (10) Return to caller.
C ----------------------------------------------------------------------
C
10000 CONTINUE
      RETURN
      END
