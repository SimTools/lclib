C*
      COMMON/TBSFMT/NRWFMT(4)
C*
C*  NRWFMT(i) = 0  ; No data format conversion at Read/Write.
C*
C*  NRWFMT(1) : Used to indicate input data format.
C*           = 0 : to read UNIX standard format data.
C*           = 1 : to read from FACOM using RMTL routines.
C*                 standard MSP->UNIX data conversion is applied.
C*           = 2 : to read VBS format data copied from FACOM MSP,
C*                 standard MSP->UNIX data conversion is applied.
C*           = 3 : Direct read of SONY-DIR1000L tape.
C*                 standard MSP->UNIX data conversion is applied.
C*           = 4 : Read using standrd fortran I/O. (no data conversion)
C*           =20 : Read through TCP/IP-TBSDS, with NRWFMT(1)=0 disk format.
C*           =21 : Read through TCP/IP-TBSDS, with NRWFMT(1)=1 disk format.
C*           =23 : Read through TCP/IP-TBSDS, with NRWFMT(1)=3 format.
C*           =24 : Read through TCP/IP-TBSDS, with NRWFMT(1)=4 format.
C*    when 20 <= NRWFMT(1) <= 24, file must be opened properly prior
C*    to use, using tbsds_client routines.
C*
C*        (2) = 1  ; Convert unix standard format to MSP VBS format.
C*        (3) = for TBCRED
C*        (4) = for TBCWRT
C*
