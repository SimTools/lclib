C***********************************************************************
C*
C*  Parameter MALPNT
C*
C*  Pointers to the runlist record.
C*
C***********************************************************************
C*
C*
      PARAMETER (MLxLVD = 1, MLxLVT = 2, MLxLEX = 3, MLxLRN = 4,
     >           MLxLFM = 5,
     >           MLxFVD = 7, MLxFVT = 8, MLxFEX = 9, MLxFRN =10,
     >           MPxFFM =11)
C*    IBUF(MLxLVD) ; Date when last run information is saved.
C*         MLxLVT  ; Time
C*         MLxLEX  ; Last run's Experiment #.
C*         MLxLRN  ; Last run's Run #.
C*         MLxLFM  ; Las run's format tape vol=ser.
C*         MLxFVD  ; Date when 1st run information is saved.
C*         MLxFVT  ; Time
C*         MLxFEX  ; First run's experiment #.
C*         MLxFRN  ; First run's run #.
C*         MLxFFM  ; First run's formatted tape vol=ser
