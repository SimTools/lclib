C**********************************************************************
C*
C*  For the analysis of WW    event.
C*
C*  A. Miyamoto  20-May-1990  Original version.
C*
C**********************************************************************
C*
C* -----------------
C* Subroutine PRUSER( IDREC, LEVEL, IDEBUG, NRET )
C* -----------------
C*
C*(Function)
C*   Event analysis.
C*
C*(Input)
C*   IDREC ; Record ID.  ( = 1 for event record.)
C*   LEVEL ; Level flag. ( no meaning )
C*   IDEBUG; Debug flag. ( no meaning.)
C*
C*(Output)
C*   NRET  ; Return code. = 0 to output this record.
C*                        =-1 to skip this events.
C*
C*(Author)
C*  A. Miyamoto.  20-May-1990   Original version.
C*
C**********************************************************************
C*
      SUBROUTINE PRUSER( IDREC, LEVEL, IDEBUG, NRET )
C
C
C------------< Entry Point >-----------------------------------------
C
C -------------------------------------------------------------------
C (1) Some initialization.
C -------------------------------------------------------------------
C
      NRET = -1
      IF( IDREC .NE. 1 ) RETURN
C
      CALL WWANL( -1, NRET )

      RETURN
      END
C**********************************************************************
C*
C* -----------------
C* Subroutine WWANL( NPED, NRET )
C* -----------------
C*
C*(Function)
C*   Histogram Generator data, etc.
C*
C*(Input)
C*   NPED > 0 for HDEFs
C*        < 0 for HCUM
C*
C*(Output)
C*   NRET  ;
C*
C*
C*(Author)
C*  A. Miyamoto.  26-Jun-1990   Original version.
C*
C**********************************************************************
C*
      SUBROUTINE WWANL( NPD, NRET )
C
      PARAMETER (LNxPRT=100)
      PARAMETER (LNxLST=20 )
      COMMON /SSWORK/ EGENV(4), NOELM(500),
     >                CDCTRK(LNxPRT,100), SPTRK(20,20)
      DATA  IPD/0/
C
C
C------------< Entry Point >-----------------------------------------
C
C
C -------------------------------------------------------------------
C (1) HDEFs
C -------------------------------------------------------------------
C
      IF( NPD .LT. 0 ) GO TO 200
      IPD = NPD
      call hdef1(ipd+31,'i*4 ',100,0.,1.,'No. of charged track.')
      call hdef2(ipd+32,'i*2 ',50,50,0.,0.,10.,10.,'pmax vs pmin')	
      call hdef2(ipd+33,'i*2 ',50,50,0.,-1.0,10.,0.01,'pmax vs acop')	
      call hdef2(ipd+34,'i*2 ',50,50,0.,-1.0,10.,0.01,'pmin vs acop')	
      NRET = 300
      RETURN
C
C -------------------------------------------------------------------
C (2) Load Detector signal into local common.
C -------------------------------------------------------------------
C
200   CONTINUE
      NRET = -1
 
      EGENV(1) = 0
      EGENV(2) = 0
      EGENV(3) = 0
      EGENV(4) = 0

      DO 201 I = 1, 6
        CALL TBGET(1,'Spring:Parton_List',I,NW,SPTRK(1,I),IRET)
        IF( I .LE.2 ) GO TO 201
        ID = SPTRK(2,I)
        CALL HCUM1(IPD+16, SPTRK(2,I), 1.)
        IF( IABS(ID) .GT. 10 ) THEN
          CALL HCUM1(IPD+17, SPTRK(4,I),1.)
        ENDIF
        EGENV(1) = EGENV(1) + SPTRK(5,I)
        EGENV(2) = EGENV(2) + SPTRK(6,I)
        EGENV(3) = EGENV(3) + SPTRK(7,I)
        EGENV(4) = EGENV(4) + SPTRK(8,I)
 
201   CONTINUE

C
C  Select Two track event
C

      CALL TBNOEL(1,'Production:CDC;Track_Parameter',NTRK,NOELM)
      CALL HCUM1(IPD+31, FLOAT(NTRK), 1.)

      IF( NTRK.NE.2 ) RETURN


      DO 300 I = 1, 2
	IT = NOELM(I)
        CALL TBGET(1,'Production:CDC;Track_Parameter',IT,
     >           NW, CDCTRK(1,I), IRET)
300   CONTINUE

      
      p1   = sqrt(udot3(cdctrk(1,1),cdctrk(1,1)))
      p2   = sqrt(udot3(cdctrk(1,2),cdctrk(1,2)))
      acop = udot3(cdctrk(1,1), cdctrk(1,2))/p1/p2 

      pmax = max(p1,p2)
      pmin = min(p1,p2)

      CALL HCUM2(IPD+32,pmax,pmin,1.)

      if( pmin.lt.10. ) return
	

      call hcum2(ipd+33, pmax, acop, 1.)
      call hcum2(ipd+34, pmin, acop, 1.)


      nret = 0

      return
      end

C**********************************************************************
C*
C* -----------------
C* Subroutine PRURIN( LEVEL, IDEBUG )
C* -----------------
C*
C*(Function)
C*   Run initialization
C*
C*(Author)
C*  A. Miyamoto.  20-May-1990   Original version.
C*
C**********************************************************************
C*
      SUBROUTINE PRURIN( LEVEL, IDEBUG )
C
C
C------------< Entry Point >-----------------------------------------
C
C
      RETURN
      END
C**********************************************************************
C*
C* -----------------
C* Subroutine PRUJIN
C* -----------------
C*
C*(Function)
C*   Job initialization.  Clear counter.
C*
C*(Author)
C*  A. Miyamoto.  20-May-1990   Original version.
C*
C**********************************************************************
C*
      SUBROUTINE PRUJIN
C
C
      PARAMETER  (MHxSIZ=400 000 )
chpak      COMMON /HCOM/ M(MHxSIZ)
      common / PAWC/ M(MHxSIZ)
      CHARACTER*44  DISFIL
      COMMON /IOFILE/ DISFIL
C
C------------< Entry Point >-----------------------------------------
C
C
C--
C  Initialize plot buffer.
C--
      CALL HINIT(MHxSIZ)
      ILDIS = LNBLNK(DISFIL)
      IF(DISFIL(1:3).NE.'DIS'.AND.DISFIL(1:3).NE.'dis') THEN
        OPEN(UNIT=20,FILE=DISFIL(:ILDIS),ACCESS='DIRECT',RECL=4096)
        CALL HRFILE(20,'WWANL','N')
        PRINT *,'Paw Histogram file ',DISFIL(:ILDIS),' opened.'
      ENDIF

C--
C  Choose appropriate options.
C--
      CALL DOPT('NCALLS',.TRUE.)
      CALL DOPT('L2ZAUT',.TRUE.)
      CALL DOPT('L2COL',2)
      CALL DOPT('YZERO',.TRUE.)
      CALL HOPTN('STAT',.TRUE.)
CMSP       CALL PRDIS( -1 )
C--
C  Initialize hists and plots.
C--
C
      IPD = 0
      CALL WWANL( IPD, NUMHST )
C
      RETURN
      END
C**********************************************************************
C*
C* -----------------
C* Subroutine PRUJTM
C* -----------------
C*
C*(Function)
C*   Job termination; Output histogram
C*
C*(Author)
C*  A. Miyamoto.  20-May-1990   Original version.
C*
C**********************************************************************
C*
      SUBROUTINE PRUJTM
C
      CHARACTER*44  DISFIL
      CHARACTER*44  TEMPNM
      COMMON /IOFILE/ DISFIL
 
C
C------------< Entry Point >-----------------------------------------
C
C ... Output Histogram; file is allocated dynamically.
C
      DO 100 I = 1, 80
        IF(DISFIL(I:I).NE.' ') THEN
          TEMPNM = DISFIL(I:)
          GO TO 150
        ENDIF
100   CONTINUE
      PRINT *,'Dis file not specified... Writeen to temp file.'
      TEMPNM = 'TAYM.@.DIS.TEMP'
150   CONTINUE
C
           ILDIS = LNBLNK(TEMPNM)
C
      IF( TEMPNM(1:3).EQ.'DIS'.OR.TEMPNM(1:3).EQ.'dis') THEN
           call hrput(0, TEMPNM(:ILDIS), 'n')
           PRINT *,'Complete HWRITE to a file ',TEMPNM(:ILDIS)
C
      ELSE
C--
        CALL HROUT(0, ICYCLE, ' ')
        CALL HREND('WWANL')
        CLOSE(20)   

      ENDIF


C
      RETURN
      END
CC********************************************************************CC
C*                                                                    *C
C*====================                                                *C
C* Subroutine PRUCOM                                                  *C
C*====================                                                *C
C*                                                                    *C
C*(Function)                                                          *C
C*   Reads HWRITE file name.                                          *C
C*(Author)                                                            *C
 
C*                                                                    *C
CC********************************************************************CC
C
C ==========================
      SUBROUTINE PRUCOM
C ==========================
C
      CHARACTER*72 INPUT
      CHARACTER*44  DISFIL
      COMMON /IOFILE/ DISFIL
C
      DISFIL = '?'
      INPUT = ' '
      CALL TITRD('DIS file :',INPUT  )
      DISFIL = INPUT
      INPUT = ' '
      CALL TITRD('EXIT ?:', INPUT )
C
      RETURN
      END