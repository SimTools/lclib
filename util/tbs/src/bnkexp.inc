C*/TBSBNK/                                                             *
C*   PARAMETER   (MAXSUB= 20, MXSHR= 10, MAXBNK= 200, MAXELM= 4000)    *
C*   INTEGER*2   IDSUB, ISORT, KEYBNK, LINK                            *
C*   COMMON /TBSBNK/ MAXSIZ, NWFREE, NOSUB, NOSHR                      *
C*  .,      NWSUB(3,MAXSUB), IDSUB(12,MAXSUB),IDSHR(MAXSUB,MXSHR)      *
C*  .,      ISORT(MAXBNK),   NWBNK(2,MAXBNK), KEYBNK(2,MAXBNK)         *
C*  .,      LINK(3,MAXELM),  NWELM(2,MAXELM), IADTBL(3,MAXELM)         *
C*                                                                     *
C*  MAXSIZ           : Size of the Data_Buffer (I*4)                   *
C*  NWFREE           : Word_Count of the free space (I*4)              *
C*  NOSUB            : Number of the Data_Sub_Buffers (I*4)            *
C*  NOSHR            : Number of the Shared DBS's     (I*4)            *
C*                                                                     *
C*(1) DSB Paramters                                                    *
C*  NWSUB(3,MAXSUB)  : DSB_Word_Count_Table                            *
C*    NWSUB(1,j)      = Offset of the j-th Data_Sub_Buffer (DSB)       *
C*    NWSUB(2,j)      = Size of the j-th DSB                           *
C*    NWSUB(3,j)      = Number of wrods in the j-th DSB                *
C*  IDSUB(12,MAXSUB) : DSB_Parameter_Table                             *
C*    IDSUB(1,j)      = ID of the j-th DSB                             *
C*    IDSUB(2,j)      = Offset in Bank_Table (ISORT,BNKNAM,KEYBNK) of  *
C*                      the j-th DSB.                                  *
C*    IDSUB(3,j)      = Maximum number of Banks in the j-th DSB        *
C*    IDSUB(4,j)      = Number of Banks in the j-th DSB                *
C*    IDSUB(5,j)      = Offset in Element_Table (LINK,NWELM,IADTBL) of *
C*                      the j-th Data_Sub_Buffer                       *
C*    IDSUB(6,j)      = Maximum number of Bank_elements in the j-th DSB*
C*    IDSUB(7,j)      = Number of Bank_elements in the j-th DSB        *
C*    IDSUB(8,j)      = Flag for Garbage_Collection:(0=Not Needed)     *
C*    IDSUB(9,j)      = When the j-the DSB is of shared, the number of *
C*                      sharing record names is set. This must be less *
C*                      than or equal to MAXSUB.                       *
C*    IDSUB(10,j)     = When the j-th DSB is of shared, the seuential  *
C*                      number m (=  1 -> MXSHR ) is set.              *
C*    IDSUB(11,j)     = DSB status                                     *
C*          < 0       - Nothing on the DSB                             *
C*          = 0       - The record defined by TBALOC is on DSB.        *
C*          = l > 0   - When the j-th DSB is of shared and one of shar-*
C*                      ing records is on the DSB, the pointer l to    *
C*                      the sharing record name table and ID table is  *
C*                      set.                                           *
C*                      Namely, the current sharing record name and ID *
C*                      are given by                                   *
C*                          SHRNAM(l,m) and IDSHR(l,m).                *
C*    IDSUB(12,j)     = Unit of extension size                         *
C*                      If this number is zero, size of extension      *
C*                      is determined by the just one required.        *
C*(2) Bank Parameters                                                  *
C*    The ofset value n of the sort_table is given by n = IDSUB(2,j).  *
C*  ISORT(MAXBNK)    : Sorted_Pointer_Table for the sake of binary     *
C*    i = ISORT(?+n)    search of a bank name. The number ? runs from  *
C*                      1 to IDSUB(4,j).                               *
C*  NWBNK(2,MAXBNK)  : Number of word counts for the Bank              *
C*    NWBNK(1,i)      = Allocated # of words for the j-th Bank         *
C*    NWBNK(2,i)      = # of words for extension                       *
C*  KEYBNK(2,MAXBNK) : Bank_Key_Table                                  *
C*    KEYBNK(1,i)= ip = Address of the first Element of the Bank       *
C*                      in the Element_Table ( LINK, NWELM, IADTBL).   *
C*    KEYBNK(2,i)     = Number of elements in the Bank BNKNAM(j)       *
C*                                                                     *
C*(3) Bank Element Parameters                                          *
C*    The ofset value of the Element_table is given by IDSUB(5,j).     *
C*  LINK(2,MAXELM)  : Link_Table                                       *
C*    LINK(1,ip)      = (-1 / anyother ) = ( Unused/ Element # )       *
C*    LINK(2,ip) = ipn= Give the address of the next Element.          *
C*                      If this element is the last, ipn is -1.        *
C*    LINK(3,ip)      = Attribute of the element defined by TBLINK or  *
C*                      TBLNKA.                                        *
C*  NWELM(2,MAXELM) : Words count for the Bank element                 *
C*    NWELM(1,ip)     = Allocated # of words in the Bank_Element       *
C*    NWELM(2,ip)     = Number of words in the Bank_Element            *
C*  IADTBL(3,MAXELM): Address_Table                                    *
C*    IADTBL(1,ip)    = address of the Bank_Element in the Data_Buffer.*
C*    IADTBL(2,ip)    = Address of the pointer variable                *
C*    IADTBL(3,ip)    = Address of the word variable                   *
C*                      If users define  pointer and word variables    *
C*                      by calling TBLINK(...) or TBLNKA(...), the     *
C*                      addresses of the pointer and word variables    *
C*                      are stored in IADTBL(2/3,ip).                  *
C*                      These variables are always updated after these *
C*                      values are changed by garbage collection etc.  *
C*/TBSNAM/                                                             *
C*   CHARACTER*64    SUBNAM, BNKNAM, SHRNAM                            *
C*   COMMON /TBSNAM/ SUBNAM(MAXSUB),BNKNAM(MAXBNK),SHRNAM(MAXSUB,MXSHR)*
C*                                                                     *
C*  SUBNAM(MAXSUB)       : DSB_Name_Table in Character*64 format       *
C*  BNKNAM(MAXBNK)       : Bank_Name_Table in Character*64 format      *
C*  SHRNAM(MAXSUB,MXSHR) : Record names in the shared DBS              *
C*                                                                     *
C*     Required Buffer size is determined as follows:                  *
C*        760*MAXSUB + 74*MAXBNK + 26*MAXELM + 16                      *
C*      For     (20)       (200)      (4000)        = 134016Bytes      *
C*                                                                     *
