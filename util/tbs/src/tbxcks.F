C **********************************************************************
C*                                                                     *
C*===============================================================      *
C* Logical Function TBxCKS( IPSUB, MAXSUB, NWSUB, HWRODS, HPDATA)      *
C*===============================================================      *
C*(Purpose)                                                            *
C*   Check the size of the IPSUB-th Data_Sub_Buffer.                   *
C*(Input)                                                              *
C*   IPSUB    : Address of the DSB in the DSB_Word_Count_Table:NWSUB   *
C*   MAXSUB   : Maximum number of the DSBs'                            *
C*   NWSUB    : DSB_Word_Count_Table                                   *
C*   HWORDS   : Word_Count of the new Bank_Element                     *
C*(Output)                                                             *
C*   HPDATA   : Address of the Bank_Element in the Data_Buffer.        *
C*   TBxCKS = .TRUE. : There is enough space for the new Bank_element. *
C*   TBxCKS = .FALSE.: There is not enough space.                      *
C*(Author)                                                             *
C*       S.Kawabata     May 23 '84                                     *
C*       S.Kawabata     July 20 '85    Version up   V 3.0              *
C*                                                                     *
C **********************************************************************
 
      LOGICAL FUNCTION TBxCKS( IPSUB, MAXSUB, NWSUB, HWORDS, HPDATA)
 
         IMPLICIT INTEGER*4 (H)
         INTEGER NWSUB(3,MAXSUB)
C
C  -- Get the Address of free space in the Data_Buffer. ---------------
C
         MXADD   = NWSUB(1,IPSUB)
         HPDATA  = MXADD + NWSUB(3,IPSUB) + 1
         MXADD   = MXADD + NWSUB(2,IPSUB)
C//// DEBUG
C        WRITE(6,9000) IPSUB,(NWSUB(I,IPSUB),I=1,3)
C9000    FORMAT(1X,'FROM TBxCKS IPSUB,NWSUB(1)~(3)  =',4I6)
C        WRITE(6,9100) IPDATA, MXADD,NWORDS
C9100    FORMAT(1X,'FROM TBxCKS IPDATA,MXADD,NWORDS =',3I6)
C        CALL ERRTRA
C
C  ------  Check whether enough space or not. ------------------------
C*
         IF( HPDATA+HWORDS-1 .GT. MXADD ) THEN
           TBxCKS = .FALSE.
C
C                WRITE(6,9200)
C9200            FORMAT(10X,'***** TBxCKS = .FALSE. ****')
         ELSE
           TBxCKS = .TRUE.
C
C                WRITE(6,9300)
C9300            FORMAT(10X,'***** TBxCKS = .TRUE.******')
         ENDIF
C
         RETURN
         END
