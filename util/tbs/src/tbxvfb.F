C
C----------
C TBxVFB
C----------
C
      LOGICAL FUNCTION TBxVFB ( IDREC, BANK )
C
C ARGUMENTS
C
      INTEGER * 4      IDREC
      CHARACTER * (*)  BANK
C
C COMMON
C
#include "tbfmap.inc"
C
C VARIABLES
C
      INTEGER * 4      I
C
C MAIN
C
      CALL UCUPER ( 64, BANK )
      DO 10 I = 1, NMAP
        IF ( IDREC .EQ. MAPID(I) .AND. BANK .EQ. MAPBNK(I) ) THEN
          TBxVFB = .TRUE.
          RETURN
        END IF
10    CONTINUE
      TBxVFB = .FALSE.
      RETURN
      END
