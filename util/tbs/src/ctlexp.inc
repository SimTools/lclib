C* /TBSCTL/                                                            *
C*    PARAMETER       (MAXCTL = 15724)                                 *
C*    PARAMETER       (MXHCTL = 2*MAXCTL)                              *
C*    COMMON /TBSCTL/ NWCNTL,ICNTL(MAXCTL)                             *
C*    INTEGER*2       HCNTL(MXHCTL)                                    *
C*    EQUIVALENCE     (HCNTL(1), ICNTL(1))                             *
C*   Data_Format of ICNTL (HCNTL)                                      *
C*    a. Size of ICNTL should be greater than                          *
C*        18.5*MAXBNK + 3*MAXELM + 24                                  *
C*        For the case of MAXBNK = 200 and MAXELM = 4000, the size of  *
C*        ICNTL should be greater than 15724 I*4 Words.                *
C*    b. General Information and Pointers                              *
C*        NWCNTL                 : Size of the Control_Block           *
C*                   =< 18.5*NB + 3*MAXELM + 24                        *
C*        ICNTL(  1)             : Word_Count of the Data_Sub_Buffer   *
C*        ICNTL(  2) = 402       : TBS Version Number                  *
C*        ICNTL(  3)             : Size of the Data_Sub_Buffer         *
C*        ICNTL(  4)------+                                            *
C*              :         |      : Data_Sub_Buffer_Name                *
C*        ICNTL( 19)------+                                            *
C*        HCNTL( 39) = NB        : Number of Banks used                *
C*        HCNTL( 40) = MXBNK     : Allocated Number of Banks           *
C*        HCNTL( 41) = NE        : Number of Bank_Elements used        *
C*        HCNTL( 42) = MXELM     : Allocated Number of Bank_Elements   *
C*        HCNTL( 43) = 48                                              *
C*                   = IOFSRT    : Offset  to Sort_Table in I*2        *
C*        HCNTL( 44) = NB + IOFSRT                                     *
C*                   = IOFNAM    : Offset  to Bank_Name_Table in I*2   *
C*        HCNTL( 45) = IOFNWB    : Pointer to Words count table in I*4 *
C*        HCNTL( 46) = IOFKEY    : Pointer to Bank_Key_Table in I*2    *
C*        HCNTL( 47) = IOFKEY+ 2*NB                                    *
C*                   = IOFLNK    : Pointer to the Link_Table in I*2    *
C*        HCNTL( 48) = (IOFKEY+ 2*NB+ 2*NE)/2                          *
C*                   = IOFELM    : Pointer to the word count of ELM I*4*
C*        HCNTL( 49) = IOFADT    : Pointer to the Address_Table in I*4 *
C*    c. Information for this record                                   *
C*        HCNTL(IOFSRT+ 1)----+                                        *
C*              :             !  : Sort_Table for the DSB  (0.5*MAXBNK)*
C*              :             !                                        *
C*        HCNTL(IOFNAM*2 )----+                                        *
C*        ICNTL(IOFNAM+ 1)----+  : Bank_Names for the DSB  (<16*MAXBNK)*
C*              :             !  Packed Format                         *
C*              :             ! ..., Ni,ABCDE..GH, Nj,IJKL..M, ...     *
C*        ICNTL(*********)----+        !__Ni____!     !__Nj_!          *
C*        ICNTL(IOFNWB+ 1)----+                                        *
C*              :             !  : Words count for the bank            *
C*              :             !                            (2*MAXBNK)  *
C*        ICNTL(IOFKEY/2 )----+                                        *
C*        HCNTL(IOFKEY+ 1)----+                                        *
C*              :             !  : Bank_Key_Table for the DSB          *
C*              :             !                            (MAXBNK)    *
C*        HCNTL(IOFLNK   )----+                                        *
C*        HCNTL(IOFLNK+ 1)----+                                        *
C*              :             !  : Link_Table for the DSB              *
C*              :             !                           (MAXELM)     *
C*        HCNTL(IOFLNK+2*NE)--+                                        *
C*        ICNTL(IOFELM   )----+                                        *
C*              :             !  : Words count for the element         *
C*              :             !                           (2*MAXELM)   *
C*        ICNTL(IOFELM+2*NE)--+                                        *
C*        ICNTL(IOFADT+ 1)----+                                        *
C*              :             |  : Address_Table for the DSB           *
C*              :             |                           (MAXELM)     *
C*        ICNTL(IOFADT+ NE)---+                                        *
