C***********************************************************************
C*
C*  COMMON /PRJPR1/
C*
C*  Job parameters.
C*
C***********************************************************************
C
      COMMON /PRJPR1/ NDELBK, NALLWT
C
C NDELBK = 1 to delete bank defined as output bank in describe.
C        = 0 not to delete.
C NALLWT = 1 to write all Begin_Run, End_Run records.
C        = 0 not to write Begin_Run, End_Run when No output event
C            records exists in the run.
C
C
C=    DATA  NDELBK/ 1 /
C=    DATA  NALLWT/ 1 /
C
