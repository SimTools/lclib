C***********************************************************************
C*
C* Common /MACNTL/ and /MACTLC/
C*
C***********************************************************************
C*
      COMMON /MACNTL/ MCxDBS, MCxOPN, MCxKEY, MCxKYL, MCxVSM,
     >                MCxOPS, MCxOPD, MCxOPT,
     >                MCxMPT, MCxMGT, MCxPRT, MCxMSL
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
C=    DATA  MCxDBS/80/
C=    DATA  MCxOPN/ 0/
C=    DATA  MCxKEY/20/
C=    DATA  MCxKYL/16/
C=    DATA  MCxVSM/10000/
C=    DATA  MCxOPS/0/
C=    DATA  MCxOPD/0/
C=    DATA  MCxOPT/0/
C=    DATA  MCxMPT/6/
C=    DATA  MCxMGT/5/
C=    DATA  MCxPRT/77/
C=    DATA  MCxMSL/100/
C*
      CHARACTER       CMxDBS*44, CMxUID*8, CMxJBN*16, CMxVRS*8
      CHARACTER       CMxBLK*64, CMxDLM*64
      COMMON /MACTLC/ CMxDBS, CMxUID, CMxJBN, CMxVRS,
     >                CMxBLK, CMxDLM
C*    CMxDBS  : Data base file name.
C*    CMxUID  : User ID of Data base user.
C*    CMxJBN  : Job name
C*    CMxVRS  : Format to convert character data into version string.
C*    CMxBLK  : Blank array used as Header Key.
C*    CMxDLM  : EOF Key.
C*
CKEK C=    DATA  CMxDBS/'T#ON.TOPAZ.RUNLIST'/
CNGY C=    DATA  CMxDBS/'TRISTAN.T#ON.TOPAZ.RUNLIST'/
CNWU C=    DATA  CMxDBS/'SP63041.T#ON.TOPAZ.RUNLIST'/
C=    DATA  CMxJBN/'XXXXXXXXUSERJOB'/
C=    DATA  CMxVRS/'(I4.4)'/
C=    DATA  CMxBLK/'  '/
C=    DATA  CMxDLM
C=   >  /'9999999999999999999999999999999999999999999999999999999999'/
