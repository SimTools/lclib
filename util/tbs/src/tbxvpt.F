C
C----------
C TBxVPT
C----------
C
      SUBROUTINE TBxVPT ( IDREC, BANK, NELM, PNTR )
C
C ARGUMENTS
C
      INTEGER * 4      IDREC
      CHARACTER * (*)  BANK
      INTEGER * 4      NELM
      INTEGER * 4      PNTR(2)
C
C COMMON
C
#include "tbfmap.inc"
C
C VARIABLES
C
      INTEGER * 4      I, J
C
C MAIN
C
      DO 10 I = 1, NMAP
        IF ( IDREC .EQ. MAPID(I) .AND. BANK .EQ. MAPBNK(I) ) THEN
          PNTR(1) = I
C         PRINT *, BANK, ' is set to ', PNTR(1)
          IF ( MAPMEL(I) .EQ. 1 .AND. MAPELM(I,1) .EQ. -1 ) THEN
            PNTR(2) = 1
C           PRINT *, 'Element pointer is set to ', PNTR(2)
            RETURN
          ELSE
            DO 20 J = 1, MAPMEL(I)
              IF ( NELM .EQ. MAPELM (I,J) ) THEN
                PNTR(2) = J
C               PRINT *, 'Loop:Element pointer is set to ', PNTR(2)
C               PRINT *, '    : ', I, J, NELM, MAPELM(I,J)
                RETURN
              END IF
20          CONTINUE
            IF ( MAPELM(I,1) .EQ. -1 ) THEN
              PNTR(2) = 1
            ELSE
cccc              IPT = MOD ( NELM, MAPELM(I,MAPMEL(I)) )
              kkk=mapelm(i,mapmel(i))
	      ipt = mod( nelm ,kkk )
              IF ( IPT .EQ. 0 ) IPT = MAPELM(I,MAPMEL(I))
              DO 30 J = 1, MAPMEL(I)
                IF ( IPT .EQ. MAPELM (I,J) ) THEN
                  PNTR(2) = J
C                 PRINT *, 'Loop:Element pointer is set to ', PNTR(2)
C                 PRINT *, '    : ', I, J, NELM, MAPELM(I,J)
                  RETURN
                END IF
30            CONTINUE
              PNTR(1) = 0
              PRINT *, 'Element pointer Error ', NELM, ':', BANK
            END IF
            RETURN
          END IF
        END IF
10    CONTINUE
      PNTR(1) = 0
      RETURN
      END
