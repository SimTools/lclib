C=    PARAMETER (MAXSUB = 20)
      COMMON /TBSSUM/ NTREAD,NREDER,NOREAD(MAXSUB),NOPASS,NOSKIP,
     .          NTWRIT,NOWRIT(MAXSUB),IDSIZE(MAXSUB),MXIDSZ(MAXSUB),
     .          NODEX(MAXSUB), MXNBNK(MAXSUB),NOBEX(MAXSUB),
     .          MXNELM(MAXSUB),NOEEX(MAXSUB), NOGAB(MAXSUB)
C-----------------------------------------------------------------------
C   For I/O statistics
C           NTREAD : Total number of reads
C           NOREAD(i) : Number of reads for the i-th DSB
C           NOPASS    : Number of records passed
C           NOSKIP    : Number of records skipped
C           NTWRIT : Total number of write
C           NOWRIT(i) : Number of write of the i-th DSB
C
C   For DSB statistics
C           IDSIZE(i) : Current size of real data of the i-th DSB
C           MXIDSZ(i) : Maximum size of real data of the i-th DSB
C           NODEX(i)  : Number of DSB size extensions
C           MXNBNK(i) : Max. number of existing banks
C           NOBEX(i)  : Number of bank_table extension
C           MXNELM(i) : Max. number of existing elements
C           NOEEX(i)  : Number of element_table extension
C           NOGAB(i)  : Number of gabage collection for the i-th DSB
C-----------------------------------------------------------------------
C
