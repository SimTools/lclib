/*****************************************************************
**
** =====================================
**  Data format of SONY Tape (DIR-1000)
** =====================================
**
**(1) Data structure on the tape.
**   Each label record is 512 bytes long.
**
**   V T H             T H            T E T 
**   O M D  .. Data .. M D .. Data .. M O M 
**   L   R               R              V 
**   L   L               L              L 
**
**     VOLL : Volume label.
**     HDRL : Header label. 
**     TM   : Tape mark
**     EOVL : End of volume Label
**
**   VOLL  512 byte length
**       Word# 0-  3 = A character string, "VOLL" in ASC code.
**             4- 67 = Volume name.
**            68- 71 = Two characters indicating the data storage mode.
**                     DM = SONY DMS Data format.
**            72-103 = Tape Owner (toptape@kektrws1 for a moment )
**           104-111 = Tape initialized date(yy-mm-dd).
**           112-119 = Tape initialized time(hh:mm:ss).
**       Meaningfull region are filled with zero(\0).
**
**   HDRL 512 byte length
**       Word#  0- 3 = A character string, "HDRL"
**              4-67 = file name(<64 bytes)
**             68-71 = Record format(see below)(4 characters)
**             72-79 = Record length(8 characters)
**             80-87 = File sequence number(8 characters).
**           104-111 = File creation date(yy-mm-dd).
**           112-119 = File creation time(hh:mm:ss).
**      (note) 
**         (1) All information is saved by ASC-II.
**             meaningless area is filled with \0, except 
**             68-87, where space is padded.
**         (2) Record format:
**             F   ; fixed length, which is stored in 73-80.
**             V   ; variable length( First word of a record is a length)
**             VBS ; FACOM's VBS format, block size is stored in 73-80.
**             FTBS; fixed length created by TBS system. 
**
**   EOVL 512 byte length
**       Word#  0- 3 = A character string, "EOVL"
**             80-87 = File sequence number.
**           104-111 = File creation date(yy-mm-dd).
**           112-119 = File creation time(hh:mm:ss).
**
**
**
** =============================================================
** Calling sequence of subroutines.
** =============================================================
** 
**  Subroutine TBTOPN(UNIT, VOLUME, FILENAM, FILEPOS, ACTION, NRET )    
**   
**    UNIT   : Tape Unit # (Not used for a moment)
**    VOLUME : Tape Volume name.
**             In the system, we memorize the VOLUME in use.
**           If VOLUME is not same as those stored in the memory,
**           return TBTOPN with NRET=-1.
**           If VOLUME is same, we compare the file position in memory
**           and FILEPOS, and move the tape position to the proper 
**           position.  Then read HDR1/HDR2 and check the file name.
**           If file name is not same, return with NRET=-2.
**           If OK, return after open data file.
**    FILENAME : file name
**    FILEPOS  : file sequence numer.
**    ACTION   : "READ" or "WRITE"
**
**  Subroutine TBTFMT(UNIT, RECFM, LRECL )
**    UNIT     : 
**    FORMAT   : Record format :  "V", "F" or  "VBS"
**    LRECL    : Record length 
**   This routine should be called before calling TBTOPN with ACTION="WRITE".
**
**  Subroutine TBTWRT(UNIT, NW, NBUF, NRET )
**
**  Subroutine TBTRED(UNIT, MAXBUF, NW, NBUF )
**
**  Subroutine TBTCLS(UNIT, NRET)
**   Close tape file. Check consistency bye reading EOF1/EOF2
**
**  Subroutine TBTUNM(UNIT, VOLUME, NRET )
**   Unmount tape.
**
**********************************************************************/

#define HEADSIZE 512

  union hblock {
    char dummy[HEADSIZE];
    struct header {
      char name[100];
      char mode[8];
      char uid[8];
      char gid[8];
      char size[13];
      char mtime[11];
      char chksum[8];
      char linkflag[1];

      char linkname[10];
      char labelid[6];
      char recfm[6];
      char lrec[12];
      char fseq[8];
      char unused[58];
/*                         */
      char magic[6];
      char version[2];
      char uname[32];
      char gname[32];
      char devmajor[8];
      char devminor[8];
      char prefix[155];
    } dbuf;
  };

/*
** Prefix = VOLL for volume header.
**          HDRL for tape file header.
**          EOFL for volume EOF.
** labelid = TBS for our standard format.
**           if this field is all \0, it must be created by tar command.
** recfm   = F for fixed length.
**         = V for variable length ( First word of a record is record length)
** lrecl   = record length by a decimal-ASC-II number.
** fseq    = file sequence number, decimal ASC-II  ( 0 for Volume header.)
** mode    is alwasy 644.
*/


