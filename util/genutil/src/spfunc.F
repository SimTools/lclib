C***********************************************************************
C*
C* ----------------------------------
C*  Subroutine SPFUNC( /FUNC/, MXTRY )
C* ----------------------------------
C*
C*(Function)
C*   Set Function name and MXTRY for SPRING.
C*   To be called in SPINIT of user program.
C*(Input)
C*   FUNC  :  Function name
C*   MXTRY ;  MXTRY
C*   Same as those used for SPMAIN.
C*
C*(Author)
C*   A.Miyamoto  9-Feb-1990  Original version.
C*
C***********************************************************************
C*
c      SUBROUTINE SPFUNC( /MFUNC/, MMXTRY )
      SUBROUTINE SPFUNC( MFUNC, MMXTRY )
C
      COMMON /SPPARM/ NADSPF, MXTRY
C
#if defined(AIX)
      NADSPF = LOC(MFUNC)
#elif defined(__GFORTRAN__)
      call addfunc(mfunc)
#else
      NADSPF = %LOC(MFUNC)
#endif
      MXTRY  = MMXTRY
C
      RETURN
      END
