CC**********************************************************************
C*
C*=======================
C*  COMMON /PRJUNK/
C*=======================
C*
C* (Purpose)
C*    Carries global cluster information for EMC or HDC.
C* (Contents)
C*    NBNON          = # bins filled.
C*    JBNPNT(IZ,IX)  = JBN for bin (IZ,IX).
C*    IBNPNT(1,JBN)  = IZT for JBN-th bin data.
C*          (2,JBN)  = IFI
C*    NCLIN (  JBN)  = JCL for JBN.
C*    NCELLS         = # hit cells(=NBNON).
C*    ICLPNT(1,JCL)  = energy in MeV.
C*          (2,JCL)  = cell ID.
C*          (3,JCL)  = IZT.
C*          (4,JCL)  = IFI.
C*          (5,JCL)  = pointer to Hit_Cell bank.
C*
C*    NCDCTK         = # CDC tracks.
C*    RTKBNK( *,JTK) = track info of JTK-th track.
C*          (60,JTK) :(I*4): # possible hit cells.
C*          (61,JTK) :(R*4): x of expected hit point.
C*          (62,JTK) :(R*4): y of expected hit point.
C*          (63,JTK) :(R*4): z of expected hit point.
C*          (64,JTK) :(I*4): ID of 1-st possible hit cell.
C*          (65,JTK) :(R*4): delta_z   in units of cell width.
C*          (66,JTK) :(R*4): delta_phi in units of cell width.
C*          (67,JTK) :(I*4): ID of 2-nd possible hit cell.
C*          (68,JTK) :(R*4): delta_z   in units of cell width.
C*          (69,JTK) :(R*4): delta_phi in units of cell width.
C*            ..................................
C*    KBNPNT(IZ,IX)  = KBN for bin (IZ,IX).
C*    NTKIN (  KBN)  = # tracks in this bin.
C*    ITKPNT(I,KBN)  = JTK of I-th track in this bin.
C*    KCNTRK(  JTK)  = poison flag.
C*
C*    IWORK(*)      = cluster data formatted as follows;
C*             +==================================================+
C*      D   1  ! # rows for this cluster.                         !
C*      i      +==================================================+
C*      r   2  ! row # ( J ).                                     !
C*      e   3  ! pointer to row # J ( k1 ).                       !
C*      c      +--------------------------------------------------+
C*      t   4  ! row # ( J + 1 ).                                 !
C*      o   5  ! pointer to row # J + 1 ( k2 ).                   !
C*      r      +--------------------------------------------------+
C*      y      !   ......                                         !
C*             +==================================================+
C*          k1 ! # cells    in row # J.                           !
C*      C      +--------------------------------------------------+
C*      l   +1 ! energy in MeV.                                   !
C*      u   +2 ! cell ID.                                         !
C*      s   +3 ! IELM for Production:HDC;Hit_Cell.                !
C*      t      +--------------------------------------------------+
C*      e   +4 ! energy in MeV.                                   !
C*      r   +5 ! cell ID.                                         !
C*          +6 ! IELM for Production:HDC;Hit_Cell.                !
C*      D      +--------------------------------------------------+
C*      a      !                                                  !
C*      t      !                                                  !
C*      a      !                                                  !
C*
C* (Update Record)
C*    9/16/86  K.Fujii  Original version.
C*   12/11/99  A.Miyamoto MXxPHI, MXxZET are increased
C*
CC**********************************************************************
 
CC      PARAMETER ( MXxZET=43, MXxPHI=77, MXxBD=(MXxZET+1)*(MXxPHI+1),
      PARAMETER ( MXxZET=90, MXxPHI=100,MXxBD=(MXxZET+1)*(MXxPHI+1),
CC     .            MXxCLS=5000 ,MXxWRK=1000, LNxTRK = 100, MXxTRK = 500 )
     .            MXxCLS=5000 ,MXxWRK=3000, LNxTRK = 100, MXxTRK = 500 )
      COMMON /PRJUNK/ NBNON, NCELLS, JBNPNT(0:MXxZET,0:MXxPHI),
     .                IBNPNT(2,MXxBD) , NCLIN(MXxBD), ICLPNT(5,MXxCLS),
     .                WORK(MXxWRK), NCDCTK, RTKBNK(LNxTRK,MXxTRK),
     .                KBNPNT(0:MXxZET,0:MXxPHI), NTKIN(MXxBD),
     .                ITKPNT(MXxTRK,MXxBD), KCNTRK(MXxTRK)
      INTEGER*4       NBNON, NCELLS, JBNPNT, IBNPNT, NCLIN, ICLPNT,
     .                NCDCTK, KBNPNT, NTKIN, ITKPNT, KCNTRK
      REAL   *4       WORK, RTKBNK
C--
      INTEGER*4       IWORK(MXxWRK), ITKBNK(LNxTRK,MXxTRK)
      EQUIVALENCE    (IWORK,WORK)
      EQUIVALENCE    (ITKBNK,RTKBNK)
      INTEGER*2       HTKBNK(2,LNxTRK,MXxTRK)
      EQUIVALENCE    (HTKBNK,RTKBNK)
