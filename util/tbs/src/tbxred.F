      LOGICAL FUNCTION TBxRED( LUN, NW, IDATA)
C**********************************************************************C
C* *                                                                   *
C*===========================================                          *
C* Logical Function TBxRED( LUN, NW, IDATA )                           *
C*===========================================                          *
C*(Purpose)                                                            *
C*   Read Data from the Logical_unit LUN into the Data_Sub_Buffer      *
C*(Input)                                                              *
C*   LUN      : Logical_Unit_Number for the Input Device               *
C*(Output)                                                             *
C*   NW       : Word_Count                                             *
C*   IDATA    : Data_Buffer                                            *
C*   TBxRED   : .TRUE. = Normal return                                 *
C*              .FALSE.= EOF                                           *
C*(Author)                                                             *
C*       S.Kawabata     June 2 '84                                     *
C*                                                                     *
C***********************************************************************
C
         DIMENSION IDATA(*)
#include "tbsmes.inc"
#include "tbsfmt.inc"

#ifdef UNIX
      Integer * 4      TB_read_buffer
#endif
C
C  --------------------------------------------------------------------
C       Read Data form the logical unit: LUN into the Buffer IDATA
C  --------------------------------------------------------------------
C
#ifdef  UNIX
      IF( NRWFMT(1).EQ.0 ) THEN
	Is = TB_read_buffer ( LUN, Nw, Idata )
	If ( Is .EQ. 0 ) Goto 8000 
C
C   Read VBS format data.
      ELSEIF( NRWFMT(1) .EQ. 1 ) THEN
        CALL TBVRED(LUN, NW, IDATA(1))
        IF( NW.EQ.0 ) THEN
          GO TO 8000
        ELSEIF( NW.LT.0 ) THEN
          IST = -10
        ELSE
          IST = 0
        ENDIF
#if !defined(JLC) && !defined(HP)
C
C   Read FACOM data through RMTL
      ELSEIF( NRWFMT(1) .EQ. 2 ) THEN
        LBUF = LUN*4
        CALL MTLRED(10, NW, LBUF, IST)
        IF( IST.EQ.-1.AND.NW.EQ.0 ) GO TO 8000
#endif
C
C   Read SONY-DIR100L Tapes.
      ELSEIF( NRWFMT(1) .EQ. 3 ) THEN
        CALL TBTRED(LUN, NW, IDATA(1))
        IF( NW.EQ.0 ) THEN
          GO TO 8000
        ELSEIF( NW.LT.0 ) THEN
          IST = -10
        ELSE
          IST = 0
        ENDIF
C
C   Read Fortran-I/O'ed files
      ElseIf ( NRWFMT(1) .EQ. 4 .OR. NRWFMT(1).EQ.5 ) then
        READ( LUN, END =8000, IOSTAT=IST ) NW,(IDATA(K),K=1,NW)
C
C   Read Through tbsds
C
      Elseif( NRWFMT(1).EQ.20 .OR. NRWFMT(1).EQ.21 .OR.
     >	       NRWFMT(1).EQ.23 .OR. NRWFMT(1).EQ.24 ) THEN
        call tbsds_receive(100000, nw, idata)
        if( nw .gt. 1 ) then
          ist = 0
        elseif( nw.eq.1 .and. idata(1).eq.0 ) then
          go to 8000
        else
          ist = -10
        endif
        
C   Error
      ELSE
        WRITE(MESAGE(1),310) NRWFMT(1)
310     FORMAT(' Unsupported input data format#',I3)
        CALL TBxERR(700, 'TBxRED',1 )
      ENDIF
#else
         READ( LUN, END =8000, IOSTAT=IST ) NW,(IDATA(K),K=1,NW)
#endif
C
C  --- I/O Status Handling. -------------------------------------------
C
         IF( IST .EQ. 0 .OR. IST .EQ. 213 ) THEN
           TBxRED  = .TRUE.
C
         ELSE
           IF( IST .EQ. 219 ) THEN
             WRITE(MESAGE(1),100) LUN
100          FORMAT(' DD data missing, LUN=',I3)
C
             CALL TBxERR( 600, 'TBxRED', 1)
C
           ELSE
             WRITE(MESAGE(1),110) IST
110          FORMAT('  Read Error, IOSTAT =',I12)
C
             CALL TBxERR( 700, 'TBxRED', 1)
C
           ENDIF
         ENDIF
         RETURN
C
 8000    CONTINUE
         TBxRED  = .FALSE.
C
         RETURN
         END
