CC*********************************************************************C
C*                                                                     *
C*====================                                                 *
C* Subroutine PRINIT                                                   *
C*====================                                                 *
C*  (Purpose)                                                          *
C*       Initialization of JCL-FULL analysis system.                   *
C*  (Relation)                                                         *
C*       Called by                                                     *
C*           PRMAIN : Main Program of TOPAZ Production Job.            *
C*  (Author)                                                           *
C*       A. Miyamoto    02-Apr-1987  Original version.                 *
C*       A. Miyamoto    03-Jun-1987  Fixed a bug to count output size. *
C*       A. Miyamoto    08-Sep-1989  To FULLnize simulator.            *
C*                                   call PRTBSI after reading param.  *
C*                                                                     *
CC**********************************************************************
C
      SUBROUTINE PRINIT
C
#include "prjenv.inc"
#include "clibuf.inc"
 
C
C==================<<  Entry Point  >>================================
C
C
C ------------------- Write Job heading
C
      CALL PRJTIT
C
C---------------------- Initialize error handling routine.
C
      CALL UERINI(3)
C
C---------------------- Initialize counter for output data size.
C
      CALL UCTLSZ(-1, TAPESZ, DISKSZ)
C
C---------------------- Initialize CLI command storage buffer.
C
#ifdef  MSP
      JUNK = CLIMEM( MXxLIN )
#endif
C
C---------------------- Initialize data base routine.
C
CX    CALL PRDBIN(IRET)
CX    IF(IRET.LT.0)               GO TO 9000
C
C
C---------------------- Read in job parameters.
C
CX    CALL PRRINP(IRET)
CX    IF(IRET.LT.0)               GO TO 9000
C
C---------------------- TBS Initialize.
C
      CALL PRTBS1(IRET)
      IF(IRET.LT.0)               GO TO 9000
C
C---------------------- Get User ID.
C
#ifdef HIUXF77 
      call getenv("USER", 4, USERID, 8)
9000  CONTINUE
      RETURN
      END
#else

#ifdef MACHTEN_PPC
#else
      CALL GETUID( USERID )
#endif
C
 
C
9000  CONTINUE
      RETURN
      END
#endif
