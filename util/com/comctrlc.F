C   12/11/86 611121958  MEMBER NAME  COMCTRLC (FORT)     M  FORTRAN
C+++++ THIS IS COMCTRLC.FOR            B. GABIOUD JUNE 1982
C
       SUBROUTINE SETATN
c      include 'pep4_com_lib(comode)'
       include 'comode.inc'
       EXTERNAL GOTATN
       INTEGER*4 LCHAN, IFF
       integer * 4      iproc
       external signal
       integer signal
C
C..... DEFINE CONTROL-C (ATTENTION) HANDLER
C
       CTRLC = .FALSE.
C       CALL ATNXIT ( 1, GOTATN, IRET )
CMSP        call get_processor_id ( iproc )
#ifdef HIUXF77
       call jmlatn( gotatn )
#elif MACHTEN_PPC
#else
       CALL GETPID(iproc)
       if ( iproc .lt. 0 ) then
         iii = signal(2,gotatn,-1)
       end if
#endif
C
       END
C-----
       SUBROUTINE GOTATN
c      include 'pep4_com_lib(comode)'
       include 'comode.inc'
       CHARACTER * 1   ANS   / ' ' /
C
C Func
c
       Integer * 4      signal
c       External         Gotatn
C
C..... PROCESS CONTROL-C
C
       CTRLC = .TRUE.
C       WRITE ( 6, * ) '%COM-I-CTRLC, CTRLC detected'
C       CALL PROMPT ( 2, 'Do you want to continue <Y/N> ? ', IRET )
C       READ ( 5, '(A)' ) ANS
C       IF ( ANS(1:1) .EQ. 'N' ) STOP
       ICOMER = 2
c       iii=signal(2,gotatn,-1)
       END
