CC********************************************************************CC
C*                                                                    *C
C*=================                                                   *C
C*   / PRHEAD /                                                       *C
C*=================                                                   *C
C*                                                                    *C
C* 1) Contents                                                        *C
C*     Contains the URH of current record.                            *C
C*     URH means the Copy of Bank Raw_Data:Header.                    *C
C*                                                                    *C
C* 2) Update Record                                                   *C
C*    25-Apr-1987  A. Miyamoto  Original version.                     *C
C*                                                                    *C
CC********************************************************************CC
C
      INTEGER*4       xTWORD, xERROR, xRECTP, xEXPNO, xRUNNO,
     .                xEVTNO, xDATE,  xTIME,  xTDACU, xRUNTP,
     .                xCREW , xFMERR, xEVTID, xDUMMY(3),
     .                xERRS1, xERRS2
      COMMON /PRHEAD/ xTWORD, xERROR, xRECTP, xEXPNO, xRUNNO,
     .                xEVTNO, xDATE,  xTIME,  xTDACU, xRUNTP,
     .                xCREW , xFMERR, xEVTID, xDUMMY,
     .                xERRS1, xERRS2
C
C*    xTWORD  ; Total # of word (I*4)
C*    xERROR  ; Error flag.
C*    xRECTP  ; Record type.
C*    xEXPNO  ; Experiment no.
C*    xRUNNO  ; Run no.
C*    xEVTNO  ; Event no.
C*    xDATE   ; Date
C*    xTIME   ; Time
C*    xTDACU  ; Tape Dacu flag.
C*    xRUNTP  ; Run type.
C*    xCREW   ; Shift Crew.
C*    xFMERR  ; Format job error flag.
C*    xEVTID  ; Event ID.
C*    xDUMMY  ; Dummy data.
C*    xERRS1  ; Error Status word-1
C*    xERRS2  ; Error Status word-2
