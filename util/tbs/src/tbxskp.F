      LOGICAL FUNCTION TBxSKP( LUN )
C**********************************************************************C
C* *                                                                   *
C*================================                                     *
C* Logical Function TBxSKP( LUN )                                      *
C*================================                                     *
C*(Purpose)                                                            *
C*   Read Data from the Logical_unit LUN into the Data_Sub_Buffer      *
C*(Input)                                                              *
C*   LUN      : Logical_Unit_Number for the Input Device               *
C*(Output)                                                             *
C*   TBxSKP   : .TRUE. = Normal return                                 *
C*              .FALSE.= EOF                                           *
C*(Author)                                                             *
C*       S.Kawabata     June 2 '84                                     *
C*                                                                     *
C***********************************************************************
C
#include "tbsmes.inc"
#include "tbsfmt.inc"
#include "sswork.inc"
#ifdef UNIX
      Integer * 4      TB_read_buffer
      common /tbsds_dummy/ idata(1000000)
#endif
C
C  --------------------------------------------------------------------
C       Read Data form the logical unit: LUN into the Buffer IDATA
C  --------------------------------------------------------------------
C
#ifdef  UNIX
      IF( NRWFMT(1).EQ.0 ) THEN
C         READ( LUN, END =8000, IOSTAT=IST ) NW
        Is = TB_Read_Buffer ( Lun, Nw, Iwork )
        If ( Is .EQ. 0 ) Goto 8000
C
C   To skip VBS format data.
      ELSEIF( NRWFMT(1).EQ.1 ) THEN
        IST = 0
        CALL TBVRED(1, NW, IDUMY)
        IF( NW.EQ.0 ) GO TO 8000
        IF( NW.LT.0 ) IST = -20
        
C
C   To skip SONY DIR-1000L tapes.
      ELSEIF( NRWFMT(1).EQ.3 ) THEN
        IST = 0
        CALL TBTRED(1, NW, IDUMY)
        IF( NW.EQ.0 ) GO TO 8000
        IF( NW.LT.0 ) IST = -20
C
C   Read Fortran-I/O'ed files
      Else If ( NRWFMT(1) .EQ. 4 ) then
        READ( LUN, END =8000, IOSTAT=IST ) NW
C
       
C
C   Error
      ELSE
        WRITE(MESAGE(1),310) NRWFMT(1)
310     FORMAT(' Unsupported input data format#',I3)
        CALL TBxERR(700, 'TBxSKP',1 )
      ENDIF
#else
      READ( LUN, END =8000, IOSTAT=IST ) NW
#endif
C
C  --- I/O Status Handling. -------------------------------------------
C
         IF( IST .EQ. 0 .OR. IST .EQ. 213 ) THEN
           TBxSKP  = .TRUE.
C
         ELSE
           IF( IST .EQ. 219 ) THEN
             WRITE(MESAGE(1),100) LUN
100          FORMAT(' DD data missing, LUN=',I3)
C
             CALL TBxERR( 600, 'TBxSKP', 1)
C
           ELSE
             WRITE(MESAGE(1),110) IST
110          FORMAT(' Read Error, IOSTAT =',I12)
C
             CALL TBxERR( 700, 'TBxSKP', 1 )
C
           ENDIF
         ENDIF
         RETURN
C
 8000    CONTINUE
         TBxSKP  = .FALSE.
C
             WRITE(MESAGE(1),120) LUN
120          FORMAT(' End of File for LUN =',I4)
C
             CALL TBxERR( 100, 'TBxSKP', 1 )
         RETURN
         END
