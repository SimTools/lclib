C*********************************************************************
C*======================                                             *
C*  COMMON /PRDTYP/                                                  *
C*======================                                             *
C*                                                                   *
C*(1) Purpose                                                        *
C*                                                                   *
C*  Contains the inportant part of Begin_run DSB.  Actually, it      *
C*  INCLUDES CONTENTS OF THE BANK DATA_TYPE .                        *
C*                                                                   *
C*(2) Discription of data.                                           *
C*                                                                   *
C*    NDTYPE(i)  : Copy of bank Data_Type.                           *
C*          (1)  : = 2, means this is MC data.                       *
C*          (2)  : = 1/2 , means field on/off                        *
C*          (3)  : = 0/1 , when simulated by Debug/MC generator.     *
C*          (4)  : = 1   , means this data passes simulator job.     *
C*          (5)  : = 0                                               *
C*           !                                                       *
C*          (9)  : = 0                                               *
C*    NSVERS     : Simulator Version # (-1 for Raw_Data)             *
C*                                                                   *
C*********************************************************************
C
C --------------------------------------------------------------------
      PARAMETER  (KPxPRD=8)
      COMMON /PRDTYP/ NDTYPE(9),NSVERS
C --------------------------------------------------------------------
