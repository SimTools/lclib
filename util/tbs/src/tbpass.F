C **********************************************************************
C*                                                                     *
C*======================================                               *
C* Subroutine TBPASS( RECNAM, LUN, IRET)                               *
C*======================================                               *
C*(Purpose)                                                            *
C*   To declare that the record RECNAM ought to be directly copied     *
C*   onto the logical unit LUN.                                        *
C*(Input)                                                              *
C*   RECNAM   : Record name                                            *
C*   LUN      : Logical unit number                                    *
C*(Output)                                                             *
C*   IRET     : Return_Flag                                            *
C*     IRET >=0 : Normal Return                                        *
C*          < 0 : Warning                                              *
C*          -1  : The record is already registered as either DSB or    *
C*                shared record.                                       *
C*                                                                     *
C*(Author)                                                             *
C*       S.Kawabata     Jan  9 '86                                     *
C*                                                                     *
C **********************************************************************
 
      SUBROUTINE TBPASS( RECNAM, LUN, IRET )
 
#include "tbsbnk.inc"
#include "tbspas.inc"
#include "tbsmes.inc"
      CHARACTER*(*) RECNAM
      CHARACTER*64  TNAME
 
C  --------------------------------------------------------------
C                Check the several Parameters
C  --------------------------------------------------------------
C   ------------ Test whether the RECNAM exist or not. ------
 
         TNAME = RECNAM
 
         CALL UCUPER( LEN(RECNAM), TNAME)
C
         IF( NOSUB .GT. 0 ) THEN
C                                ====  ====
             CALL TBxCRN( TNAME, IDSH, IRET)
 
             IF( IRET .GT. 0 ) THEN
                 WRITE(MESAGE(1),9001)
 9001            FORMAT(' The following record name exists already.')
                 WRITE(MESAGE(2),9002) TNAME
 9002            FORMAT('  Record name = ',A64)
                 CALL TBxERR( 200,'TBPASS', 2)
                 IRET  = -1
                 RETURN
             ENDIF
         ENDIF
C
C  ------------- Check the Number of the passed records -----
C      If the Number exceeds MAXPAS, then Warning and return.
C
         IF(    NOPAS .GE. MAXPAS) THEN
                 WRITE(MESAGE(1),9011)
 9011            FORMAT(' Too many passed record kinds')
                 WRITE(MESAGE(2),9022) NOPAS,MAXPAS
 9022            FORMAT('  Required number =',I3,', while Max =',I4)
                 CALL TBxERR( 200,'TBPASS', 2)
                 IRET  = -2
                 RETURN
         ENDIF
 
         NOPAS   = NOPAS + 1
         LUNPAS(NOPAS) = LUN
         PASNAM(NOPAS) = TNAME
 
         IRET  = 1
         RETURN
         END
