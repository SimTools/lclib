C***********************************************************************
C*
C* Common /TBCCTL/ and /TBCCTS/
C*
C* A. Miyamoto  8-Jul-1987  Put protection to avoid wrong compile.
C*
C***********************************************************************
C*
      COMMON /TBCCTL/ MCxDBS, MCxOPN, MCxKEY, MCxKYL, MCxVSM,
     >                MCxOPS, MCxOPD, MCxOPT
C*
C*    MCxDBS  : Logical unit # for the Data base I/O
C*    MCxOPN  : Data base access mode,
C*             = 0 : Read access.
C*             = 1 : Read/Write Access.
C*    MCxKEY  : Total Key length.
C*    MCxKYL  : Valid Key length.
C*    MCxVSM  : Maximum version # + 1
C*    MCxOPS  : Data set open status
C*             = 0 : Data set is closed.
C*             = 1 : Data set is opened.
C*             = 2 : Data set is opened in TBCxVF
C*    MCxOPD  : Data base open date.
C*    MCxOPT  : Data base open time
C*
C=    DATA  MCxDBS/-1/
CKEK C=    DATA  MCxDBS/78/
CNGY C=    DATA  MCxDBS/78/
CNWU C=    DATA  MCxDBS/78/
C=    DATA  MCxOPN/ 0/
C=    DATA  MCxKEY/999999/
CNGY C=    DATA  MCxKEY/50/
CKEK C=    DATA  MCxKEY/50/
CNWU C=    DATA  MCxKEY/50/
C=    DATA  MCxKYL/44/
C=    DATA  MCxVSM/100000/
C=    DATA  MCxOPS/0/
C=    DATA  MCxOPD/0/
C=    DATA  MCxOPT/0/
C*
#ifdef MSP
      CHARACTER       CMxDBS*44, CMxUID*8, CMxJBN*16, CMxVRS*8
#endif
#ifdef UNIX
      CHARACTER       CMxDBS*128, CMxUID*8, CMxJBN*16, CMxVRS*8
#endif
      CHARACTER       CMxBLK*64, CMxDLM*64
      COMMON /TBCCTC/ CMxDBS, CMxUID, CMxJBN, CMxVRS,
     >                CMxBLK, CMxDLM
C*    CMxDBS  : Data base file name.
C*    CMxUID  : User ID of Data base user.
C*    CMxJBN  : Job name
C*    CMxVRS  : Format to convert character data into version string.
C*    CMxBLK  : Blank array used as Header Key.
C*    CMxDLM  : EOF Key.
C*
C=    DATA  CMxDBS/'XXXXXXXXXXXXXXXXXXX'/
CKEK C=    DATA  CMxDBS/'T#DB.TOPAZ.CONSTANT'/
CNGY C=    DATA  CMxDBS/'TRISTAN.T#DB.TOPAZ.CONSTANT'/
CNWU C=    DATA  CMxDBS/'SP63041.T#DB.TOPAZ.CONSTANT'/
C=    DATA  CMxJBN/'XXXXXXXXUSERJOB'/
C=    DATA  CMxVRS/'(I5.5)'/
C=    DATA  CMxBLK/'  '/
C=    DATA  CMxDLM
C=   >  /'9999999999999999999999999999999999999999999999999999999999'/
