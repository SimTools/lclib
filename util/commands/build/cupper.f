C***********************************************************************
C*                                                                     *
C*============================
C* Subroutine CUPPER( STRING )                                         *
C*============================
C*(Purpose)                                                            *
C*   Convert the lower case character to the upper case character.     *
C*(Input/Output)                                                       *
C*   STRING   : Character string to be converted.                      *
C*(Author)                                                             *
C*   A. Miyamoto    26-June-1985                                       *
C*                                                                     *
C***********************************************************************
C  
      SUBROUTINE CUPPER( STRING )
      CHARACTER*(*) STRING
C  
      LSTR = LEN(STRING)
C  
C  ----------------------------------------------------------------
C               Copy from CHIN to CHOUT.
C  ----------------------------------------------------------------
C  
         DO 300   I  = 1, LSTR
           ICODE     = ICHAR( STRING(I:I) )
C  
C  --- Convert the lowercase character into theOCppercase one.  ---
C  
           IF((ICODE .GE. 129 .AND. ICODE .LE. 137) .OR.
     .        (ICODE .GE. 145 .AND. ICODE .LE. 153) .OR.
     .        (ICODE .GE. 162 .AND. ICODE .LE. 169) )   THEN
C  
               ICODE = ICODE + 64
               STRING(I:I) = CHAR( ICODE )
C  
           ENDIF
C  
C  --- Save character into the output character buffer.
C  
C  
  300    CONTINUE
C  
         RETURN
         END
