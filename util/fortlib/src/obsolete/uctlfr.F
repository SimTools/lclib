C***********************************************************************
C* 
C* ------------------------
C* Subroutine UCTLFR(NUNIT)
C* ------------------------
C* 
C*(Function)
C*   Dynammically free  CTL files.
C* 
C*(Input)
C*   NUNIT   : Fortran Unit #.
C* 
C*(Output)
C*   (none)
C* 
C*(Author)
C*   A. Miyamoto  24-Sep-1987  Test version.
C* 
C***********************************************************************
C  
      SUBROUTINE UCTLFR( NUNIT )
C  
#include "uctl00.inc"
C  
C  
C======< Entry Point >==================================================
C  
C ----------------------------------------------------------------------
C (1) Free
C ----------------------------------------------------------------------
C  
      IF( MOUNIT .LE. 0 .OR. MOUNIT .NE. NUNIT ) THEN
        PRINT *,'%Warning..UCTLFR  File is not allocated yet.',
     .          ' or wrong unit # specified.'
        PRINT *,'  Requested to free unit #',NUNIT
        PRINT *,'  Current Unit # stack    ',MOUNIT
        CALL ERRTRA
        RETURN
      ENDIF
C  
      CLOSE (NUNIT)
      MOUNIT = 0
      RETURN
      END
