C***********************************************************************
C*
C*  Parameter MARPNT
C*
C*  Pointers to the data in run record.
C*
C***********************************************************************
C*
C*
      PARAMETER (MPxVDT = 1, MPxVTM = 2, MPxEXP = 3, MPxRUN = 4,
     >           MPxREC = 5, MPxEVT = 6, MPxTYP = 7, MPxCLU = 8,
     >           MPxERR = 9, MPxQAL =10, MPxSDT =11, MPxSTM =12,
     >           MPxEDT =13, MPxETM =14, MPxRCV =15, MPxRCF =17,
     >           MPxFMV =18, MPxFMF =20, MPxFMS =21, MPxFMD =22)
C*    IBUF(MPxVDT) ; Date when this record is created.
C*         MPxVTM  ; Time
C*         MPxEXP  ; Experiment #.
C*         MPxRUN  ; Run #.
C*         MPxREC  ; # of record in the run.
C*         MPxEVT  ; Last event #.
C*         MPxTYP  ; Run type.
C*         MPxCLU  ; Shift clue.
C*         MPxERR  ; Error flag.
C*         MPxQAL  ; Run quality.
C*         MPxSDT  ; Run start date.
C*         MCxSTM  ; Run start time.
C*         MPxEDT  ; End start date.
C*         MPxETM  ; End start time.
C*         MPxRCV:MPxRCV+1  ; Recycle tape vol=ser.
C*         MPxRCF  ; File # of recycle tape.
C*                 > 0 when data exist on recycle tape.
C*                 < 0 when data is scratched.
C*         MPxFMV:MPxFMV+1  ; Format tape vol=ser.
C*         MPxFMF  ; File # of format tape.
C*                 > 0 when data exist on tape.
C*                 = 0 when format data is not exist.
C*                 < 0 when data is scratched.
C*         MPxFMS  ; Formatted data size ( in unit of tracks.)
C*         MPxFMD  ; Exist on disk/tape flag.
C*                   0 =  exist on tape.
C*                   1 =  exist on disk and tape.
