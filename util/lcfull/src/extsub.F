COPTIONS @OPTIONS  NODEBUG
CC*********************************************************************C
C*                                                                     *
C*=====================================                                *
C* Subroutine EXTSUB( IADDR, /MODULE/)                                 *
C*=====================================                                *
C*  (Purpose)                                                          *
C*       Return entry point address of the subroutine.                 *
C*  (Input)                                                            *
C*       MODULE    Subroutine entry name.                              *
C*                 Must be declared as EXTERNAL in calling routine.    *
C*  (Output)                                                           *
C*       IADDR     Adress of the subroutien.                           *
C*  (Author)                                                           *
C*       A. Miyamoto   25-Apr-1987                                     *
C*                                                                     *
CC**********************************************************************
 
#ifdef  MSP
      SUBROUTINE EXTSUB( IADDR, /MODULE/ )
#endif
#ifdef  UNIX
       SUBROUTINE EXTSUB( IADDR, MODULE )
#endif
 
C
 
C===================<< Entry point >>=================================
 
#ifdef  MSP
      IADDR = MODULE
#endif
#if defined(SUN) || defined(AIX)
       IADDR = LOC(MODULE)
#endif
#if defined(HP) || defined(HIUXF77) || defined(MACHTEN_PPC)
        IADDR = %LOC(MODULE)
#endif
 
      RETURN
      END
