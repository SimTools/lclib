C***********************************************************************
C* 
C* -----------------------================
C* SUBROUTINE UCTLSZ( NW,  TAPESZ, DISKSZ)
C* -----------------------================
C* 
C*(Function)
C*     Accumulate output data size on disk and CTL.   As a default,
C*  Following parameters are assumed.
C* 
C*     Disk file    RECFM = VBS, BLKSIZE=23476
C*                  2 blocks per track.
C*     CTL file     RECFM = VBS, BLKSIZE=32760
C*                  Density = 32000 bit per inch
C*                  IRG     = 0.126
C* 
C*(Input)
C*   NW    >= 0 ; Accumulate output data size, NW is # of write
C*          =-1 ; Clear counter. ( initialization.)
C*          =-2 ; Returns only statistics.
C* 
C*          =-10 ; Change BLock size and # of block per track for disk
C*                file.  Use TAPESZ for BLKSIZE and DISKSZ as # of
C*                blocks per track.
C*          =-11 ; Change BLKSIZE of CTL file.  Use TAPESZ as BLKSIZE
C*                If DISKSZ > 0., Assumes CTL file, DISKSZ < 0.,
C*                Assumes ordinary tape of Density 6400 bpi and
C*                IRG = 0.3035 inch.
C*          =-12 ; CHange CTL  density and IRG, use TAPESZ as denisity
C*                DISKSZ as IRG.
C* 
C*(Output)
C*   TAPESZ : Tape length.
C*   DISKSZ : Disk size in unit of tracks.
C* 
C*   A. Miyamoto   9-Oct-1986  Original version.
C*                 7-Apr-1987  CTL version installed.
C* 
C***********************************************************************
C  
      SUBROUTINE UCTLSZ( NW, TAPESZ, DISKSZ)
C  
C  
      SAVE          IMTOUT, IMTBLK, NMTBLK, TAPDEN,
     >              TAPIRG, IDKOUT,         NDKBLK,
     >              DSKUNI, TAPUNI
C  
C     IMTOUT  ; Current block size.
C     IMTBLK  ; # of blocks being written.
C     NMTBLK  ; Tape block size.
C     TAPDEN  ; Tape record density.( 6400 bpi)
C     TAPIRG  ; Tape IRG length.
C  
C     IDKOUT  ; Current block size for DISK file.
C     IDKBLK  ; # of blocks being written on DISK.
C     NDKBLK  ; Tape block size.
C  
      DATA NMTBLK/32760/
      DATA TAPDEN/32000./, TAPIRG/0.126/
      DATA NDKBLK/23476/ , DSKUNI/0.5/
C  
C======< Entry Point >==================================================
C  
C ----------------------------------------------------------------------
C (2) When called with NW = -1, reset counter.
C ----------------------------------------------------------------------
C  
      IF ( NW ) 100, 300, 300
C  
100   CONTINUE
      IF(NW.EQ.-1) THEN
        IMTOUT = 0
        IMTBLK = 1
        IDKOUT = 0
        IDKBLK = 1
        TAPUNI = (FLOAT(NMTBLK)/TAPDEN+TAPIRG)/12.
        TAPESZ = 0.0
        DISKSZ = 0.0
      ELSEIF(NW.EQ.-2) THEN
        GO TO 500
      ELSEIF(NW.EQ.-10) THEN
        NMTBLK = TAPESZ
        DSKUNI = 1./DISKSZ
      ELSEIF(NW.EQ.-11) THEN
        NMTBLK = TAPESZ
        IF(DISKSZ.GT.0.1) THEN
          TAPDEN = 32000.
          TAPIRG = 0.126
        ELSEIF(DISKSZ.LT.-0.1) THEN
          TAPDEN = 6400.
          TAPIRG = 0.364
        ENDIF
      ELSEIF(NW.EQ.-12) THEN
        TAPDEN = TAPESZ
        TAPIRG = DISKSZ
      ENDIF
      RETURN
C  
C ----------------------------------------------------------------------
C  (3) Accumulate # of blocks and other statistics.
C ----------------------------------------------------------------------
C  
300   CONTINUE
      IMTOUT = IMTOUT + 4*NW + 4
      IDKOUT = IDKOUT + 4*NW + 4
C  
C  For Disk file case.
C  
310   CONTINUE
      IF(IDKOUT.GT.NDKBLK-4) THEN
         IDKBLK = IDKBLK + 1
         IDKOUT = IDKOUT - (NDKBLK - 4) + 4
         GO TO 310
      ELSEIF(IDKOUT.EQ.NDKBLK-4) THEN
         IDKBLK = IDKBLK + 1
         IDKOUT = 0
      ENDIF
C  
C  For Tape file case.
C  
320   CONTINUE
      IF(IMTOUT.GT.NMTBLK-4) THEN
         IMTBLK = IMTBLK + 1
         IMTOUT = IMTOUT - (NMTBLK - 4) + 4
         GO TO 320
      ELSEIF(IMTOUT.EQ.NMTBLK-4) THEN
         IMTBLK = IMTBLK + 1
         IMTOUT = 0
      ENDIF
C  
C ----------------------------------------------------------------------
C (5) Output disk or tape size.
C ----------------------------------------------------------------------
C  
500   CONTINUE
      TAPESZ   = FLOAT(IMTBLK)*TAPUNI
      DISKSZ   = FLOAT(IDKBLK)*DSKUNI
C  
      RETURN
      END
