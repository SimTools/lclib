C **********************************************************************
C*                                                                     *
C*===============================================                      *
C* Subroutine TBSHAR( IDSHAR, DSBNAM, ID, IRET)                        *
C*===============================================                      *
C*(Purpose)                                                            *
C*   To define a record RECNAM( ID ) as a sharing record which uses    *
C*   the shared DSB (IDSHAR) as data buffer.                           *
C*(Input)                                                              *
C*   IDSHAR   : DSB ID of the shared DBS.                              *
C*   DSBNAM   : Name of record to be stored in the shared DBS          *
C*   ID       : ID_Number of the record                                *
C*(Output)                                                             *
C*   IRET     : Return_Flag                                            *
C*     IRET >=0 : Normal Return                                        *
C*          < 0 : Warning                                              *
C*          -1  : The shared DSB ID(IDSHAR) does not exist.            *
C*          -2  : The record ID exists already.                        *
C*          -3  : The record name exists already.                      *
C*          -4  : Too many DSBs are used as shared DSB.                *
C*          -5  : Too many records assigned to a DSB.                  *
C*(Used COMMON)                                                        *
#include "bnkexp.inc"
C*                                                                     *
C*(Author)                                                             *
C*       S.Kawabata     Nov 26 '85     V4.0                            *
C*                                                                     *
C **********************************************************************
 
      SUBROUTINE TBSHAR( IDSHAR, RECNAM, ID, IRET)
 
      IMPLICIT INTEGER*4 (H)
#include "tbsbnk.inc"
#include "tbspas.inc"
#include "tbsmes.inc"
      CHARACTER*(*) RECNAM
      CHARACTER*64  TNAME
 
C  --------------------------------------------------------------
C                Check the several Parameters
C  --------------------------------------------------------------
C
C    ---------- Test whether the DSB: IDSHAR exist --------------
C                             =====
         CALL TBxFSB( IDSHAR, IPSUB )
C
         IF( IPSUB .LT. 0 ) THEN
 
           WRITE(MESAGE(1),9001) ID
 9001      FORMAT(' Record ID(=',I5,' ) does not exist.')
           CALL TBxERR( 200,'TBSHAR', 1)
 
           IRET = -1
           RETURN
         ENDIF
 
C    ---------- Test whether the record: ID exists in record table
C                         ====
         CALL TBxCRD( ID, IRET )
 
         IF( IRET .GT. 0 ) THEN
               WRITE(MESAGE(1),9011) IDSHR
 9011          FORMAT(' Duplicate record ID(',I4,
     .                ' ) was tried to share.')
               CALL TBxERR( 200,'TBSHAR', 1 )
           IRET = -2
           RETURN
         ENDIF
 
C    ---------- Test whether the record name exists in record table
 
         TNAME  = RECNAM
         CALL UCUPER( LEN(RECNAM), TNAME )
C                            ====  ====
         CALL TBxCRN( TNAME, IDSH, IDSB )
 
         IF( IDSB .GT. 0 ) THEN
               WRITE(MESAGE(1),9021)
 9021          FORMAT(' Duplicate record name was tried to share.')
               WRITE(MESAGE(2),9022) TNAME
 9022          FORMAT(' Record name = ',A64)
               CALL TBxERR( 200,'TBSHAR', 2 )
           IRET = -3
           RETURN
         ENDIF
 
C    ---------- Test whether the record name exists in PAS table
 
         IF( NOPAS .GT. 0 ) THEN
             DO 100 I = 1, NOPAS
                IF( TNAME .EQ. PASNAM(I)) THEN
                   WRITE(MESAGE(1),9031)
 9031              FORMAT(' The following record name is already in',
     .                    ' passed record name table.')
                   WRITE(MESAGE(2),9022) TNAME
                   CALL TBxERR( 600,'TBSHAR', 2 )
                ENDIF
  100        CONTINUE
         ENDIF
 
C   ------------ Test whether number of shared DSB  ------
 
         NRECRD  = IDSUB(9,IPSUB)
         IF(     NRECRD .EQ.  0 ) THEN
             IF( NOSHR .EQ. MXSHR ) THEN
                   WRITE(MESAGE(1),9041)
 9041              FORMAT(' Too many DSBs are defined for time shared')
                   WRITE(MESAGE(2),9042) MXSHR
 9042              FORMAT(' Max number =',I5)
                   CALL TBxERR( 200,'TBSHAR', 2 )
                 IRET = -4
                 RETURN
             ENDIF
             NOSH        R   = NOSHR + 1
             IDSUB(10,IPSUB) = NOSHR
         ELSEIF( NRECRD .EQ. MAXSUB )THEN
                   WRITE(MESAGE(1),9051)
 9051              FORMAT(' Too many records are assigned to a DSB.')
                   WRITE(MESAGE(2),9042) MAXSUB
                   CALL TBxERR( 200,'TBSHAR', 2 )
             IRET = -5
             RETURN
         ENDIF
 
 
C   ------------ Set the record into the record table ----
 
         ISHRAD               = IDSUB(10,IPSUB)
         NRECRD               = NRECRD + 1
         IDSUB(9,IPSUB)       = NRECRD
         IDSHR(NRECRD,ISHRAD) = ID
         SHRNAM(NRECRD,ISHRAD)= TNAME
         IDSUB(11,IPSUB)      = -1
C
         IRET  = 1
         RETURN
         END
