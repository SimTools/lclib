C**********************************************************************C
C*                                                                     *
C*=================================                                    *
C* Subroutine TBLSPC(ID, BNAME )                                       *
C*=================================                                    *
C*(Purpose)                                                            *
C*   List the Space used by TBS.                                       *
C*                                                                     *
C*(Input)                                                              *
C*   ID    : DSB ID number, List all DSB information when ID is        *
C*           negative.                                                 *
C*   BNAME : Bank name to list space information, when BNAME is blank, *
C*           list all bank information.                                *
C*                                                                     *
C*(Used COMMON)                                                        *
C*   /TBSBNK/                                                          *
C*(Author)                                                             *
C*      A.Miyamoto   July 26 '85                                       *
C*                                                                     *
C *********************************************************************C
 
      SUBROUTINE TBLSPC(ID, BNAME )
 
#include "tbsbnk.inc"
#include "tbsbuf.inc"
#include "tbsprm.inc"
#include "tbsmes.inc"
C
      CHARACTER*(*) BNAME
      CHARACTER*64 TNAME
C
C (1) if NOSUB le 0 then return
C
      IF( NOSUB .LE. 0 ) THEN
 
           WRITE(MESAGE(1),9001)
 9001      FORMAT(' No Data Sub-buffer is defined.')
           CALL TBxERR( 200,'TBLSPC', 1)
 
        RETURN
      END IF
      IF( ID .LT. 0 ) THEN
        IDMIN = 1
        IDMAX = NOSUB
      ELSE
        IDMIN = ID
        IDMAX = ID
      ENDIF
      LBNAM = LEN(BNAME)
      TNAME = BNAME
      CALL UCUPER( LBNAM, TNAME)
      WRITE(LUNIT,100) ID, TNAME
100   FORMAT(//,' **** List of TBS Space for DSB ID =',I5,
     >          ' Bank=',A64)
C
C
C
      DO 200 IDSB = IDMIN, IDMAX
         NOBNK    = IDSUB(4,IDSB)
         NALOC    = NWSUB(2,IDSB)/2
         NUSED    = NWSUB(3,IDSB)/2
         WRITE(LUNIT,210) SUBNAM(IDSB), NALOC, NUSED, NOBNK
210      FORMAT(/,' *** DSB =',A64,/,
     >            '     Allocated size ',I7,' I*4 words',
     >            '  :  Used size ',I7,' I*4 words',
     > '  :  ',I5,' of Banks.')
         IF(TNAME(1:1).EQ.' ') THEN
           IBMIN = IDSUB(2,IDSB) + 1
           IBMAX = IDSUB(2,IDSB) + NOBNK
         ELSE
           CALL TBxFRD( IDSB, IPSUB)
           IF(IPSUB .LT. 0 ) GO TO 200
           CALL TBxFBK( IPSUB, TNAME, ISRT, IPBNK, IRET)
           IF(IRET .LT. 0 ) GO TO 200
           IBMIN = IPBNK
           IBMAX = IPBNK
         ENDIF
C
C Loop over banks in a DSB
C
         WRITE(LUNIT,310)
310      FORMAT(' ***  Bank Space List *** size is in I*2 unit')
         WRITE(LUNIT,320)
320      FORMAT(6X,' #Elm   #Alloc.     #Used    Bank name')
         DO 300 IBNK = IBMIN, IBMAX
            NOELM = KEYBNK(2,IBNK)
            NALOC = 0
            NUSED = 0
            JP    = KEYBNK(1,IBNK)
            DO 400 I = 1, NOELM
              IF(LINK(1,JP).LT.0) GO TO 400
               NALOC = NWELM(1,JP) + NALOC
               NUSED = NWELM(2,JP) + NUSED
               JP    = LINK(2,JP)
400         CONTINUE
         WRITE(LUNIT,330) IBNK, NOELM, NALOC, NUSED,BNKNAM(IBNK)
330      FORMAT(I6,I5,I10,I10,5X,A64)
300      CONTINUE
200    CONTINUE
 
       RETURN
       END
