/*****************************************************************************
**  f2unix.c
** ==< Function >===
**   Convert FORTRAN program written with the TOPAZ standard format
**   to the fortran code processable on unix machine.
**
** ==< Command format >==
**   f2unix -inc infile.f > outfile.f
**        -inc  : convert =EXPAND statement to INCLUDE statement.
** (or)  
**   f2unix -inc -o outfile.f infile.f
**
** ==< Conversion rule >==
**   (1) A character $ are converted to x.
**   (2) FACOM's include statement is modified for WS fortran style, 
**    i.e., INCLUDE (xxxxx)  -> INCLUDE 'xxxx.inc'
**       file type .inc is appended, @ prefix is removed from the member name.
**   (3) Convert =EXPAND statement to FORTRAN standard inlucde statement,
**       when -inc is specified as command argument.  rule (2) for member name
**       conversion is also applied.
**   (4) If first column is "*", converted to "C"
**   (5) $ in IMPLICIT statements is replaced with a balank.
**       A first comma in front of $, if exists, is replaced with a blank
**   
** ==< Author >==
**   A. Miyamoto  18-Feb-1994  Original version.
**
****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int gINCOPT;  /* =1 to convert =EXPAND to INCLUDE statement */
void PrintHelp();
/* void DoConvert(); */
void DoConvert(char *infname, char *ofname);



main(argc, argv)
int argc;
char *argv[];
{
  int i;
  char *ofname;
  int oflag,iflag;

  printf("f2unix started .. \n");

  gINCOPT = 0;
  oflag=0;
  iflag=1;
  i = 0;
  if ( argc == 1 ) PrintHelp();
  else {
    while(i<argc){
      if ( strncmp(argv[i],"?",1)==0 ){
	PrintHelp();
	exit;}
      else if (strncmp(argv[i],"-inc",4)==0)
        gINCOPT=1;
      else if (strncmp(argv[i],"-o",2)==0 ){
	oflag=i+1;
	i=i+1;}
      else 
	iflag=i;
      i++;
      }
      if( oflag == 0 ) 
	 DoConvert(argv[argc-1], NULL);
      else
	 DoConvert(argv[argc-1], argv[oflag]);
  }

  exit;
}	

/*
**  PrintHelp()
*/
void PrintHelp()
{
  system("more /net/topsun1/home/topsun1/miyamoto/TOPAZLIB/f2unix/f2unix.doc");
  return;
  }
/*
** DoConvert(char infname, char ofname)
*/
void DoConvert(char *infname, char *ofname)
{
 FILE *input;
 FILE *output;
 char inlinestr[164];
 char *getsc;
 int   lstr,i,icmp;
 char *tmp;

 int   fimplicit,fi;

 char *bra;
 char  *ket;
 char  newline[164];

 printf("input file name is %s \n",infname);
 printf("output file name is %s \n",ofname);

  if( (input=fopen(infname,"r")) == NULL ){
    printf(" error to open file %s\n",infname);
    exit(1);
    }
	printf("input file is allocated.\n"); 
  if( ofname == NULL ){
    printf("output file name is null results is written in stdout\n");
    output=stdout;
  }
  else {
    if( (output=fopen(ofname,"w")) == NULL ){
    printf(" error to open out putfile %s\n",ofname);
    exit(1);
   }
  }  

  printf(" file opened successfull \n");

  while( (getsc=fgets(inlinestr, 164, input)) != NULL ){
     lstr=strlen(inlinestr);
/*
**   get  valid string length
*/
     if(lstr>72) lstr=72 ;
     for(i=lstr-1;i>0;i--){
       if ( strncmp(inlinestr+i," ",1) != 0 ) break;
     }
     lstr=i+1;
/*     printf("lstr =%d, i=%d\n",lstr,i);*/
     strncpy(&inlinestr[i+1],"\n\0",2);
/*     printf("%s",inlinestr); */

/*
** (2.1) if first character is '*' change it to "C"
*/
     if( strncmp(inlinestr,"*",1) == 0 ) {
       strncpy(inlinestr,"C",1);
     }
/*
** (2.2) change $ to x
*/

     
     fimplicit=-1;
     if(strncmp(inlinestr,"C",1)!=0) fimplicit=0; 
     for(i=1;i<lstr;i++){
       if(strncmp(inlinestr+i," IMPLICIT ",10)==0) fimplicit++;
       if(strncmp(inlinestr+i,"$",1)==0 &&
	 (strncmp(inlinestr+i-1,"!$pragma",8)!=0 &&  
	  strncmp(inlinestr+i-1,"!$PRAGMA",8)!=0) ) { 
/*	 printf("found $ when i=%d \n",i); */
	 if( fimplicit!=1 ) {
  	   strncpy(inlinestr+i,"x",1);}
	 else {
	   strncpy(inlinestr+i," ",1);
	   for(fi=i;fi>0;fi--){
	     if(strncmp(inlinestr+fi,",",1)==0){
	        strncpy(inlinestr+fi," ",1);
	        break;}
	   }
	 }
       }
     }
/*
** (2.3) include (xxxx) --> include xxx.inc
*/
     if( strncmp(inlinestr,"C",1) != 0 ) {
       if( strstr(inlinestr," INCLUDE ")!=NULL){
	 bra=strchr(inlinestr,'(');
	 strncpy(bra," ",1);
	 ket=strchr(inlinestr,')');
	 if( strncmp(bra+1,"@",1)==0) bra=bra+1;
	 strncpy(bra,"'",1);
	 strncpy(ket,".inc'",5);
	 printf("inlinestr=%s\n",inlinestr);
       }
     }
/*
** =EXPAND statement
*/
     if( strncmp(inlinestr,"=EXPAND ",8)==0  &&  
        ( bra=strchr(inlinestr,'(') ) != 0 ){
       ket=strchr(inlinestr,')');
       if(gINCOPT==1){
	 strcpy(newline,"      INCLUDE '");
       }
       else {
	 strncpy(newline,inlinestr,bra-inlinestr);
	 strcpy(newline+(bra-inlinestr),"/\0"); 
          }
       if( strncmp(bra+1,"@",1)==0){ 
	 bra=bra+1;
         strncat(newline,bra+1,ket-bra-1);
         strcat(newline,".inc'\n");
         strcpy(inlinestr,newline);
       }
       else{
	 strncat(newline,bra+1,ket-bra-1);
	 strcat(newline,".f'\n");
	 strcpy(inlinestr,newline);
       }
       printf("%s",inlinestr);
	 
	 
     }

/*
** ==EXPAND statement, proceed with care.
*/
     if( strncmp(inlinestr,"==EXPAND ",9)==0  &&
        ( bra=strchr(inlinestr,'(')) != 0 ){
       ket=strchr(inlinestr,')');
       if(gINCOPT==1){
	 printf("We can not convert this line to INCLUDE statement:\n");
	 printf("Input=%S\n",inlinestr);
       }
       else {
	 strncpy(newline,inlinestr,bra-inlinestr);
	 strcpy(newline+(bra-inlinestr),"/\0"); 
          }
       if( strncmp(bra+1,"@",1)==0) bra=bra+1;
       strncat(newline,bra+1,ket-bra-1);
       strcat(newline,".inc'\n");
       strcpy(inlinestr,newline);
       printf("%s",inlinestr);
	 
     }

/*
** Output result
*/
     fprintf(output,"%s",inlinestr);

   }
  fclose(output);
  fclose(input);

/*
** Read file
*/


 return;
}

           
