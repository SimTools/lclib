      SUBROUTINE CHKCLS(IRNG,JRNG,IOUT)
 
      INTEGER*4 ITST(0:42,0:19),IOUT(*)
      INTEGER*4 IRNG(2),JRNG(2)
 
      WRITE(6,'(''  IZRNG = '',2I5)') IRNG(1), IRNG(2)
      WRITE(6,'(''  IXRNG = '',2I5)') JRNG(1), JRNG(2)
      CALL UVZERO(860,ITST)
      NPD = IOUT(1)
      DO 100 IPD = 1, NPD
         IX   = IOUT(2*IPD) - JRNG(1)
         IP   = IOUT(2*IPD+1)
         NCL  = IOUT(IP)
         DO 10 IZCL = 1, NCL
            IP   = IP + 1
            IFST = IOUT(IP)
            IP   = IP + 1
            ILST = IFST + IOUT(IP) - 1
            DO 1 IZ = IFST-IRNG(1), ILST-IRNG(1)
               ITST(IZ,IX) = 1
1           CONTINUE
10       CONTINUE
100   CONTINUE
      WRITE(6,'('' ITST ''/)')
      WRITE(6,'(15(/2X,30I1)/)') ((ITST(IZ,IX),IZ=0,29),IX=0,14)
 
      RETURN
      END
