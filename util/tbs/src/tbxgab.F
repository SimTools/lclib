C **********************************************************************
C*                                                                     *
C*===========================                                          *
C* Subroutine TBxGAB( IPSUB )                                          *
C*===========================                                          *
C*(Purpose)                                                            *
C*   Make Garbage Collection.                                          *
C*(Input)                                                              *
C*   IPSUB    : Address of the DSB in the DSB_Parameter_Table : IDSUB  *
C*(Used COMMON)                                                        *
C*    /TBSBUF/ and /TBSBNK/   For detail information, see TBREAD.      *
C*(Author)                                                             *
C*       S.Kawabata     May 23 '84                                     *
C*       S.Kawabata     July 20 '85     Version up   V 3.0             *
C*       S.Kawabata     Dec  12 '86     Error on No.of words fixed.    *
C*                                                                     *
C***********************************************************************
 
      SUBROUTINE TBxGAB( IPSUB )
 
      IMPLICIT INTEGER*4 (H)
#include "tbsbnk.inc"
#include "tbsbuf.inc"
#include "tbsmes.inc"
#include "tbssum.inc"
C
C                                                            //// DEBUG
C                                                   WRITE(6,9000) IPSUB
C9000                         FORMAT(1X,'TBxGAB is called. IPSUB =',I6)
C
C  ------------------------------------------------------------------
C     Get the offset of IPSUB-th DSB in the Address_Table: IADTBL
C  ------------------------------------------------------------------
C
         IOFSTE   = IDSUB(5,IPSUB)
         MXELMT   = IDSUB(6,IPSUB)
         NOELMT   = IDSUB(7,IPSUB)
         HMINAD   = NWSUB(1,IPSUB)
         HMAXAD   = HMINAD + NWSUB(3,IPSUB)
         HADOLD   = HMINAD
         HWDOLD   = 0
         HWORD    = HWDOLD
C
C  -----------------------------------------------------------------
C          Make Garbage_Collection; Loop for NOELMT times
C  -----------------------------------------------------------------
C
         DO 500 NTIME = 1, NOELMT
C
C  --- Set the Maximum Address MXADD by MAXADD and  ----------------
C  ---     the Minimum Address MNADD by MINADD.     ----------------
C
             HMXAD    = HMAXAD
             HMNAD    = HMINAD
C
C  --- Look for the smallest address between HMXAD and HMNAD, ------
C  --- and set the address as HMXAD.                          ------
C
             DO 100 J= 1,MXELMT
                 IADD0  = IOFSTE + J
               IF(LINK(1,IADD0) .GE. 0 ) THEN
                   HPDATA = IADTBL(1,IADD0) + NWSUB(1,IPSUB)
                 IF( HPDATA .GT. HMNAD .AND. HPDATA .LE. HMXAD) THEN
                     HMXAD = HPDATA
                     IADDX = IADD0
                 ENDIF
               ENDIF
  100        CONTINUE
           IF(HMXAD.EQ.HMAXAD) GO TO 500
C
C  --- Set the new address of the next Bank_element by HADNEW. -----
C
             HADNEW  = HADOLD + HWDOLD
             HWDOLD  = NWELM(1,IADDX)
             HMXAD   = HMXAD - 1
C///// DEBUG
C      PRINT *,' TBxGAB..NTIME,HADNEW,HWDOLD,HADOLD=',NTIME,HADNEW,
C    >                   HWDOLD, HADOLD
C      PRINT *,' TBxGAB..HMXAD,IADDX=',HMXAD,IADDX
C      PRINT *,' TBxGAB..LINK=',LINK(1,IADDX),LINK(2,IADDX)
C      PRINT *,' TBxGAB..IADT=',IADTBL(1,IADDX),NWELM(1,IADDX),
C    >                          NWELM(2,IADDX)
C
C
C     1. If HADNEW is larger than HMXAD, then Fatal Error.
C
           IF( HADNEW .GT. HMXAD ) THEN
               WRITE(MESAGE(1),9000)
 9000          FORMAT(' Bank_Element Size Error')
               WRITE(MESAGE(2),9002) HADNEW,HMXAD
 9002          FORMAT(' New_Add(',I5,' ) greater than Old_Add(',I5,' )')
               CALL TBxERR( 800,'TBxGAB', 2 )
 
C     2. If HADNEW is less than HMXAD, then shift the contents of the
C       next Bank_Element to the address HADNEW.
 
           ELSEIF( HADNEW .LT. HMXAD ) THEN
 
               HADD1    = HADNEW + 1
               HADD2    = HMXAD  + 1
 
               CALL UVCPYH( HWDOLD, HBUFF(HADD2), HBUFF(HADD1))
 
C The following statements are for the versions before V402.
C              DO 200 I       = 1, HWDOLD
C                 HADD1        = I + HADNEW
C                 HADD2        = I + HMXAD
C                 HBUFF(HADD1) = HBUFF(HADD2)
C 200          CONTINUE
           ENDIF
C
C  --- Update the Addresses  ---------------------------------------
C
           HADOLD           = HADNEW
           HMINAD           = HADOLD + 1
           IADTBL(1,IADDX)  = HMINAD - NWSUB(1,IPSUB)
           HWORD            = HWORD + HWDOLD
C          HWD1             = NWELM(2,IADDX)
           IF( LINK(3,IADDX) .GE. 0 ) THEN
               IF( LINK(3,IADDX) .EQ. 0 ) THEN
                   NPOINT   =  HMINAD
C                  NWORD    =  HWD1
               ELSE
                   NPOINT   =  (HMINAD+1)/2
C                  NWORD    =  (HWD1  +1)/2
               ENDIF
               CALL UVSET( IADTBL(2,IADDX), NPOINT )
C              CALL UVSET( IADTBL(3,IADDX), NWORD  )
           ENDIF
C
C        PRINT *,' TBxGAB===HADOLD,HMINAD,HWORD=',HADOLD,HMINAD,HWORD
C
C
 500     CONTINUE
C
C  --- Update the DSB_Word_Count_Table: NWSUB and       -------------
C  --  DSB_Paramter_Table :IDSUB.                       -------------
C
         NWSUB(3,IPSUB) = HWORD
         IDSUB(8,IPSUB) = 0
 
C ---- Update the counter of garbage collection
 
         NOGAB(IPSUB)  = NOGAB(IPSUB) + 1
 
 
       WRITE(MESAGE(1),9501)
 9501  FORMAT('  Garbage Collection for the record')
       WRITE(MESAGE(2),9502) SUBNAM(IPSUB)
 9502  FORMAT('  ',A64)
       CALL TBxERR( 100, 'TBxGAB', 2 )
C
         RETURN
         END
