#+
# File : convert.awk
# Description : Convert a FACOM file to a UNIX file using awk
# 
# Author : Ryosuke Itoh, Physics Division, KEK
# Date : 5 - Dec - 1994
#
# Note:
#        Process =EXPAND, ==EXPAND
#        Process CMSP, CVAX, CUNIX
#        Process CSUN, CHP, CALPHA, CAV
#        Process CKEK, CNWU, CNGY
#	 Process CJLC
#-
#
# Main Loop
#
{
if ( $0 ~ /^=EXPAND/ ) { 
  nfn = newfname( $2 )
  print "#include \"" nfn "\""
}

#
# ==EXPAND statement
#
else if ( $0 ~ /^==EXPAND/ ) {
  gsub( "'", "", $2 )
  nof = split($2, farray, "/")
  if ( nof == 0 ) { tfn = $2 }
# scan ~/.buildrc file to construct true file name
  else {
#    print "Look for " farray[1]
    tfn = ""
    bldfil = ENVIRON["HOME"]"/.buildrc"  
    while ( getline x < bldfil > 0 ) {
      iex = index(x, farray[1])
      if ( iex == 1 ) { 
	sub( farray[1], "", x )
	tfn = substr(x, 3)
	sub( "//", "/", tfn  )
      }
    }
    tfn = tfn farray[2]
    close(bldfil)

  } 
  if ( tfn == "" ) { 
    print "Uable to process a line " $0 > "/dev/tty"
    exit }

  print tfn " is expanded " > "/dev/tty"

  nefn =  lowercase( farray[2] )
  print "#include \"" nefn "\""
  while ( getline line < tfn > 0 ) {
   if( line ~ /^C=/ ) {
      nline = substr( line, 3)
      print "  " nline
    }
  }
  close( tfn )
}



else if ( $0 ~ /^C[A-Za-z0-9]+ =EXPAND/ ) { 
  nfn = newfname( $3 )
  id  = substr( $1, 2 )
  print "#ifdef "  id
  if ( id != "FACOM" && id != "MSP" ) { print "#include \"" nfn "\"" }
  else { print $0 }
  print "#endif "
}
else if ( $0 ~ /^C[A-Za-z0-9]+ / ) { 
  id  = substr( $1, 2 )
  st  = substr( $0, 2, 71 )
  last  = sub( id, "", st )
  st  = substr( st, 2)
  print "#ifdef "  id
  print st
  print "#endif "
}
else {
  print
}
}

#----------
# convert_line
#    convert a line in variablie "input" to UNIX format
#----------

function newfname (INITNAME) {
  lname = lowercase(INITNAME)
  gsub( "'", "", lname)
  nslash = split(lname, name, "/" )
  if( nslash == 0 ) {
    oname = lname
  }
  else {
    oname = name[nslash]
  }
  return oname
}
#----------
# lowercase
#    convert string to lowercase characters
#----------

function lowercase (ISTR) {
  LSTR = ISTR
  gsub ("A","a",LSTR)
  gsub ("B","b",LSTR)
  gsub ("C","c",LSTR)
  gsub ("D","d",LSTR)
  gsub ("E","e",LSTR)
  gsub ("F","f",LSTR)
  gsub ("G","g",LSTR)
  gsub ("H","h",LSTR)
  gsub ("I","i",LSTR)
  gsub ("J","j",LSTR)
  gsub ("K","k",LSTR)
  gsub ("L","l",LSTR)
  gsub ("M","m",LSTR)
  gsub ("N","n",LSTR)
  gsub ("O","o",LSTR)
  gsub ("P","p",LSTR)
  gsub ("Q","q",LSTR)
  gsub ("R","r",LSTR)
  gsub ("S","s",LSTR)
  gsub ("T","t",LSTR)
  gsub ("U","u",LSTR)
  gsub ("V","v",LSTR)
  gsub ("W","w",LSTR)
  gsub ("X","x",LSTR)
  gsub ("Y","y",LSTR)
  gsub ("Z","z",LSTR)
  return LSTR
}

