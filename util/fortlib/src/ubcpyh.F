CC********************************************************************CC
C*                                                                    *C
C*=============================================                       *C
C*  Subroutine UBCPYH( NW, IAD0, IAD1, HDATA )                        *C
C*=============================================                       *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*   To move NW half-words data between addresses IAD0 and IAD0+NW-1  *C
C*   to the same array of addresses between IAD1 and IAD1+NW-1.       *C
C*   Example                                                          *C
C*   Before calling this routine                                      *C
C*           IAD0                     IAD1                            *C
C*   ........************................................             *C
C*                                                                    *C
C*   After calling this routine                                       *C
C*           IAD0                     IAD1                            *C
C*   ........************.............************.......             *C
C*                                                                    *C
C* (Input)                                                            *C
C*     NW       : No. of I*2 words to be copied.                      *C
C*     IAD0     : Start address of the original data.                 *C
C*     IAD1     : Start address of the copied data.                   *C
C*     HDATA    : Array in which the data are copied.                 *C
C*                                                                    *C
C* (Author)                                                           *C
C*     S.Kawabata    17 Sept. '86                                     *C
C*     K.Fujii       25 Jan.  '90       A bug fixed.                  *C
C*                                                                    *C
CC********************************************************************CC
 
      SUBROUTINE UBCPYH( NW, IAD0, IAD1, HDATA )
 
      INTEGER*4 NW, IAD0, IAD1
      INTEGER*2 HDATA(*)
C
C========< Entry Point >================================================
C
      IF( IAD0 .GT. IAD1 .OR. IAD1 .GE. IAD0+NW ) THEN
          CALL UVCPYH( NW, HDATA(IAD0), HDATA(IAD1))
      ELSE
          NSLICE = IAD1 - IAD0
          IF ( ( NW     .LE. 50 ) .OR.
     .         ( NSLICE .LE. 20 ) .OR.
     .         ( NSLICE .LE. 30 .AND. NW .LE. 150 ) .OR.
     .         ( NSLICE .LE. 40 .AND. NW .LE. 100 ) ) THEN
             DO 100 I = NW-1, 0, -1
                HDATA(IAD1+I) = HDATA(IAD0+I)
  100        CONTINUE
          ELSE
             NST0   = IAD0 + NW
             DO 200 I = 1, NW
                NST1  = NST0
                NST0  = NST0 - NSLICE
                IF ( NST0 .LT. IAD0 ) THEN
                   NST0   = IAD0
                   NSLICE = NST1 - IAD0
                   NST1   = IAD1
                ENDIF
                CALL UVCPYH( NSLICE, HDATA(NST0), HDATA(NST1))
                IF( NST0 .EQ. IAD0 )             GO TO 1000
200          CONTINUE
          ENDIF
      ENDIF
 
 1000 RETURN
      END
