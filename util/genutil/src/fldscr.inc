C   13/11/86 702131949  MEMBER NAME  @FLDSCR  (FORT)     M  FORTRAN     
C+                                                                      
C File : @FLDSCR                                                        
C Description : FULL analysis system description file                   
C                                                                       
C Author : Ryosuke Itoh, Univ. of Tokyo ( at KEK )                      
C Date : 13 - NOV - 1986                                                
C-                                                                      
C                                                                       
C Constants                                                             
C                                                                       
      INTEGER * 4      MAXANL, MAXHST                                   
      PARAMETER        ( MAXANL = 40, MAXHST = 32 )                     
C                                                                       
C Common                                                                
C                                                                       
      COMMON /FLDSCR/  LISTSP (0:MAXANL),                               
     >                 ANLQUI (MAXANL),                                 
     >                 OUTQUI (MAXANL),                                 
     >                 NAMCOM (MAXANL),                                 
     >                 ENTNAM (MAXANL),                                 
     >                 ENTEVT (MAXANL),                                 
     >                 ENTINI (MAXANL),                                 
     >                 ENTTER (MAXANL),                                 
     >                 ENTDIA (MAXANL),                                 
     >                 ENTSTA (MAXANL),                                 
     >                 ENTBRU (MAXANL),                                 
     >                 ENTERU (MAXANL),                                 
     >                 INPBLK (MAXANL),                                 
     >                 INBNAM (MAXANL,32),                              
     >                 OUTBLK (MAXANL), OUBSIZ (MAXANL,32),             
     >                 OUBNAM (MAXANL,32),                              
     >                 ENTQUI (MAXANL),                                 
     >                 NPROG, NENTR, NHIST,                             
     >                 ENTHIS (MAXHST),                                 
     >                 HISNAM (MAXHST)                                  
C                                                                       
      INTEGER * 4      LISTSP                                           
      LOGICAL * 1      ANLQUI, OUTQUI                                   
      CHARACTER * 12   NAMCOM, ENTNAM                                   
      INTEGER * 4      ENTEVT, ENTINI, ENTTER, ENTDIA, ENTSTA           
      INTEGER * 4      ENTBRU, ENTERU                                   
      INTEGER * 4      INPBLK, OUTBLK                                   
      CHARACTER * 64   INBNAM, OUBNAM                                   
      INTEGER * 4      OUBSIZ                                           
      INTEGER * 4      ENTQUI                                           
      INTEGER * 4      NPROG, NENTR, NHIST                              
C                                                                       
      INTEGER * 4      ENTHIS                                           
      CHARACTER * 12   HISNAM                                           
C                                                                       
C E N D                                                                 
C                                                                       
