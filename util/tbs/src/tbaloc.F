C **********************************************************************
C*                                                                     *
C*==========================================================           *
C* Subroutine TBALOC( DSBNAM, ID, NB, MXELMT, ISIZE, IRET )            *
C*==========================================================           *
C*(Purpose)                                                            *
C*   Allocate the Number of Banks, the number of Bank_Elements and     *
C*   ISIZE words Space for the DSB.                                    *
C*(Input)                                                              *
C*   DSBNAM   : Namme of the DSB.                                      *
C*   ID       : ID_Number of the DSB                                   *
C*   NB       : Maximum Number of Banks for the DSB                    *
C*   MXELMT   : Maximum Number of Bank_Elements for the DSB            *
C*   ISIZE    : Size of the DSB: ID                                    *
C*(Used COMMON)                                                        *
#include "bnkexp.inc"
C*  /TBSPAS/    Table of allocated unit numbers for passed_Records.    *
C*  /TBSPAN/    Name list tables for passed_Records.                   *
C*                                                                     *
C*(Author)                                                             *
C*       S.Kawabata     May 23 '84                                     *
C*       S.Kawabata     July 16 '85    Version up V3.0                 *
C*       S.Kawabata     Jan. 13 '85    Version up V4.0                 *
C*                                                                     *
C **********************************************************************
 
      SUBROUTINE TBALOC( DSBNAM, ID, NB, MXELMT, ISIZE, IRET )
 
      IMPLICIT INTEGER*4 (H)
#include "tbsbnk.inc"
#include "tbspas.inc"
#include "tbsmes.inc"
#include "tbssum.inc"
      CHARACTER*(*) DSBNAM
      CHARACTER*64  TNAME
 
      HSIZE    = 2*ISIZE
 
C  --------------------------------------------------------------
C                Check the several Parameters
C  --------------------------------------------------------------
C    ---------- Test whether the DSB: ID exist --------------
C
C                         =====
         CALL TBxCRD( ID, IPSUB )
C
         IF( IPSUB .GT. 0 ) THEN
           WRITE(MESAGE(1),9001) ID
 9001      FORMAT(' Duplicate record ID(=',I5,' ) has been allocated.')
           CALL TBxERR( 600,'TBALOC', 1 )
           RETURN
         ENDIF
C
C   ------------ Test whether the DSBNAM exist in Aloc or Shared table
 
         TNAME = DSBNAM
 
         CALL UCUPER( LEN(DSBNAM), TNAME)
 
         IF( NOSUB .GT. 0 ) THEN
C                                ====  ====
             CALL TBxCRN( TNAME, IDSH, IRET)
 
             IF( IRET .GT. 0 ) THEN
               WRITE(MESAGE(1),9011)
 9011          FORMAT(' Duplicate record name has been allocated.')
               WRITE(MESAGE(2),9012)
 9012          FORMAT(' At the allocation of the record name')
               WRITE(MESAGE(3),9013) TNAME
 9013          FORMAT(' ',A64)
               CALL TBxERR( 600,'TBALOC', 3 )
             ENDIF
         ENDIF
C
C   ------------ Test whether the DSBNAM exist in PAs table -
C
         IF( NOPAS .GT. 0 ) THEN
             DO 100 I=1,NOPAS
                IF( TNAME .EQ. PASNAM(I) ) THEN
                    WRITE(MESAGE(1),9021)
 9021               FORMAT(' The record name exists in the ',
     .                'passed record name table.')
                    WRITE(MESAGE(2),9012)
                    WRITE(MESAGE(3),9013) TNAME
                    CALL TBxERR( 600,'TBALOC', 3 )
                ENDIF
  100        CONTINUE
         ENDIF
C
C  ------------- Check the Number of the DSBs'  -------------
C      If the Number exceeds MAXSUB, then Warning and return.
C
         IF(    NOSUB .GE. MAXSUB) THEN
                    WRITE(MESAGE(1),9031)
 9031               FORMAT(' Too many records are allocated.')
                    WRITE(MESAGE(2),9032) NOSUB,MAXSUB
 9032               FORMAT(' Number of record kinds =',I5,
     .                     ', while max is',I3)
                    CALL TBxERR( 600,'TBALOC', 2 )
         ELSEIF( NOSUB .EQ. 0) THEN
           IPBOFT   = 0
           HADOFT   = 0
           IELOFT   = 0
         ELSE
           IPBOFT   = IDSUB(2,NOSUB) + IDSUB(3,NOSUB)
           HADOFT   = NWSUB(1,NOSUB) + NWSUB(2,NOSUB)
           IELOFT   = IDSUB(5,NOSUB) + IDSUB(6,NOSUB)
         ENDIF
C
C  ------------- Check the size of the Data_Buffer ------------
C
         NWFREE   = NWFREE - HSIZE
C
C        If the total size exceeds MAXSIZ, then Fatal Error Stop.
C
         IF( NWFREE .LT. 0  ) THEN
           INEED = -NWFREE/2
           WRITE(MESAGE(1),9041)
9041       FORMAT(' Data buffer size overflow.')
           WRITE(MESAGE(2),610) INEED
610        FORMAT(' ',I6,' words more space neccesary')
           CALL TBxERR( 600,'TBALOC', 2 )
         ENDIF
C
C  ------------- Check the total number of Banks  --------------
C
         IF( IPBOFT+NB .GT. MAXBNK ) THEN
           WRITE(MESAGE(1),9051)
9051       FORMAT(' Too many banks are allocated.')
           CALL TBxERR( 700, 'TBALOC', 1)
         ENDIF
C
C  ------------- Check the total number of Bank_Elements  -------
C
         IF( IELOFT+MXELMT .GT. MAXELM ) THEN
           WRITE(MESAGE(1),9061)
 9061      FORMAT(' Too many Banks_elements are allocated.')
           CALL TBxERR( 700,'TBALOC', 1)
         ENDIF
C
C  -------------------------------------------------------------------
C             Update the contents of COMMON /TBSBNK/
C  -------------------------------------------------------------------
C  -----------  Increment the number of DSBs' : NOSUB   ------------
C
         NOSUB    = NOSUB + 1
C
C  --- Save the DSB_Name DSBNAM into the DSB_Name_Table: SUBNAM----
C
         SUBNAM(NOSUB)  = TNAME
C
C  ---------- Initialize the DSB_Word_Count_Table : NWSUB----------
C
         NWSUB(1,NOSUB) = HADOFT
         NWSUB(2,NOSUB) = HSIZE
         NWSUB(3,NOSUB) = 0
C
C  -------- Initialize the DSB_Parameter_Table : IDSUB ---------
C
         IDSUB(1,NOSUB) = ID
         IDSUB(2,NOSUB) = IPBOFT
         IDSUB(3,NOSUB) = NB
         IDSUB(4,NOSUB) = 0
         IDSUB(5,NOSUB) = IELOFT
         IDSUB(6,NOSUB) = MXELMT
         IDSUB(7,NOSUB) = 0
         IDSUB(8,NOSUB) = 0
         IDSUB(9,NOSUB) = 0
         IDSUB(10,NOSUB)= 0
         IDSUB(11,NOSUB)= 0
         IDSUB(12,NOSUB)= 0
 
C      Reset the statistical parameters
 
         NOREAD(NOSUB)  = 0
         NOWRIT(NOSUB)  = 0
         IDSIZE(NOSUB)  = 0
 
         MXNBNK(NOSUB)  = 0
         MXNELM(NOSUB)  = 0
         MXIDSZ(NOSUB)  = 0
 
         NOGAB(NOSUB)   = 0
         NODEX(NOSUB)   = 0
         NOBEX(NOSUB)   = 0
         NOEEX(NOSUB)   = 0
 
         IRET  = 1
C
         RETURN
         END
