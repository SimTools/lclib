C**********************************************************************
C*
C*  --------------------------
C*   Subrtoutine PRDIS( MODE )
C*  --------------------------
C*
C*(Function)
C*   Define special histogram to display Date/Time etc by DIS command
C*
C*(Input)
C*   MODE : = -1 to do HDEF
C*               (User should call HINIT prior to this call.)
C*          =  0 to save job information, Date and Time
C*          =  1 to save run information ( should be called in RUN-In.)
C*
C*(Author)
C*   Original by R.Itoh.
C*   Converted by A.Miyamoto  28-Nov-1988
C*
C*
C**********************************************************************
C*
      SUBROUTINE PRDIS( MODE )
C*
C
C COMMON
C
        COMMON / HCOM / HMEMOR ( 1000 )
        INTEGER * 4     HMEMOR, HPNTR
        COMMON /SSWORK/ IDTYPE(20), IUHEAD(20)
C
C
C =====< Entry Point >=================================================
C
      IF( MODE ) 100, 200, 300
C
C ---------------------------------------------------------------------
C (1) Initialize
C ---------------------------------------------------------------------
C
100   CONTINUE
      CALL HDEF1 ( 'TYPE', 'I*4 ', 9, 1.0, 1.0, 'DATA_TYPE@' )
      CALL HDEF1 ( 'URHD', 'I*4 ', 16, 1.0, 1.0,
     #                'UNIVERSAL_RECORD_HEADER@' )
      CALL HDEF1 ( 'DATE', 'I*4 ', 2, 1.0, 1.0, 'ANALYSIS_DATE@' )
      CALL HDEF1 ( 'TIME', 'I*4 ', 2, 1.0, 1.0, 'ANALYSIS_TIME@' )
C     RETURN
C
C ---------------------------------------------------------------------
C (2) Save Date and time of the Job.
C ---------------------------------------------------------------------
C
200   CONTINUE
C
C
C  PACK ANALYSIS DATE AND TIME
C
      IPNTR = HPNTR ( 'DATE', 'HIST' )
      IF( IPNTR .NE. 0 ) CALL  DATE( HMEMOR(IPNTR+1) )
      IPNTR = HPNTR ( 'TIME', 'HIST' )
      IF( IPNTR .NE. 0 ) CALL UTIME( HMEMOR(IPNTR+1) )
C
      RETURN
C
C ---------------------------------------------------------------------
C (3) Save Run type information.
C ---------------------------------------------------------------------
C
300   CONTINUE
      CALL TBGET ( 2, 'Data_Type', 1, NWRD, IDTYPE, IRET )
      IF ( IRET .EQ. 1 ) THEN
         IF ( IDTYPE ( 1 ) .EQ. 2 ) THEN
            CALL TBGET ( 2, 'Generator', 1, NWRD, IUHEAD, IRET )
            IF ( IRET .EQ. 1 ) THEN
               IDTYPE ( 5 ) = IUHEAD ( 3 )
               IDTYPE ( 6 ) = IUHEAD ( 4 )
               IDTYPE ( 7 ) = IUHEAD ( 5 )
               IDTYPE ( 8 ) = IUHEAD ( 6 )
            END IF
         END IF
         IPNTR = HPNTR ( 'TYPE', 'HIST' )
         DO 320 I = 1, 9
           HMEMOR ( IPNTR + I ) = IDTYPE ( I )
320      CONTINUE
      END IF
 
      CALL TBGET ( 2, 'Begin_Run:Universal_Record_Header', 1, NWRD,
     #                 IUHEAD, IRET )
      IF ( IRET .EQ. 1 ) THEN
         IPNTR = HPNTR ( 'URHD', 'HIST' )
         DO 330 I = 1, 16
            HMEMOR ( IPNTR + I ) = IUHEAD ( I )
330      CONTINUE
      END IF
C
      RETURN
      END
