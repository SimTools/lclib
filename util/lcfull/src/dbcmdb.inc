C***********************************************************************
C*
C*  Common /DBCMDB/, /DBCMDC/
C*
C*(Contents)
C*  Command parser data buffer.
C*
C*  MXxCMD  ; Maximum Number of command.
C*  MXxOPR  ; Maximum number of operand.
C*  NUMCMD  ; # of defined command.
C*
C*  DBCMDN(i)   = Character*16 array for command and operand name.
C*  NDBCTL(1,i) = Dummy.
C*        (2,i) = Pointer to the next operand, or first operand in case
C*               of command, =  0 for last operand.
C*        (3,i) = Type of operand.
C*           > 0  for C*n arguments.
C*           = 0  for command.
C*           = -1 for I*4 arguments.
C*           = -2 for R*4 arguments.
C*           = -3 for Yes/No or On/Off flag (1/0)
C*           = -4 to  check existance of the operand.
C*           < -10 for S*n arguments (lower case arguments are allowed.
C*                n is buffer length ( > 10 )
C*        (4,i) = Subroutine address which are called when command is
C*               entered or Argument address where value is set. =  0
C*               when subroutine is not need to be called.
C*
C*  IxOPER(j)  = Address of j-th operand.
C*
C***********************************************************************
C*
      PARAMETER (NCMDxB=300)
      COMMON /DBCMDB/ MXxCMD, MXxOPR, NUMCMD, NUMOPR, NDBCTL(4,NCMDxB)
      CHARACTER*16    DBCMDN(NCMDxB)
      COMMON /DBCMDC/ DBCMDN
C
