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
#-
#
# Main Loop
#
{
if ( $1 == "=EXPAND" ) {
  gsub ( "'", "", $2 )
  gsub ( "T#SM.TBS.FORT/", "", $2 )
  lstr = $2
  lowercase()
  $2 = lstr
  print "#include \""$2"\""
}
else if ( $1 == "==EXPAND" ) {
  gsub ( "'", "", $2 )
  gsub ( "T#SM.TBS.FORT/", "", $2 )
  lstr = $2
  lowercase()
  $2 = lstr
  print "#include \""$2"\""
  while ( getline x <$2 > 0 ) {
    input = x
    convert()
    if ( input ~ /C=/ ) {
      sub ( "C=","  ",input)
      if ( machine != "ALL" ) print "#ifdef ", machine  
      print input
      if ( machine != "ALL" ) print "#endif"
    }
  }
}
else {
  input = $0
  convert()
  if ( machine != "ALL" ) print "#ifdef ", machine  
  print input
  if ( machine != "ALL" ) print "#endif"
}
}

#----------
# convert_line
#    convert 1 line to UNIX
#----------

function convert () {

sub (/\$/, "x", input )
if ( input ~ /^CMSP/ ) {
  sub ( "CMSP ", "", input )
  machine = "MSP"
}
else if ( input ~ /^CVAX/ ) {
  sub ( "CVAX ", "", input )
  machine = "VAX"
}
else if ( input ~ /^CUNIX/ ) {
  sub ( "CUNIX ", "", input )
  machine = "UNIX"
}
else if ( input ~ /^CSUN/ ) {
  sub ( "CSUN ", "", input )
  machine = "SUN"
}
else if ( input ~ /^CHP/ ) {
  sub ( "CHP ", "", input )
  machine = "HP"
}
else if ( input ~ /^CALPHA/ ) {
  sub ( "CALPHA ", "", input )
  machine = "ALPHA"
}
else if ( input ~ /^CAV/ ) {
  sub ( "CAV ", "", input )
  machine = "AV"
}
else if ( input ~ /^CKEK/ ) {
  sub ( "CKEK ", "", input )
  machine = "KEK"
}
else if ( input ~ /^CNGY/ ) {
  sub ( "CNGY ", "", input )
  machine = "NGY"
}
else if ( input ~ /^CNWU/ ) {
  sub ( "CNWU ", "", input )
#  ip = match ( input, /^[A-Za-z]/ )
#  body = substr ( input, ip, length(input) )
#  input = sprintf ( "      ", body )
  machine = "NWU"
}
else {
  machine = "ALL"
}
}
#----------
# lowercase
#    convert to lowercase characters in global variable : lstr
#----------

function lowercase () {
  gsub ("A","a",lstr)
  gsub ("B","b",lstr)
  gsub ("C","c",lstr)
  gsub ("D","d",lstr)
  gsub ("E","e",lstr)
  gsub ("F","f",lstr)
  gsub ("G","g",lstr)
  gsub ("H","h",lstr)
  gsub ("I","i",lstr)
  gsub ("J","j",lstr)
  gsub ("K","k",lstr)
  gsub ("L","l",lstr)
  gsub ("M","m",lstr)
  gsub ("N","n",lstr)
  gsub ("O","o",lstr)
  gsub ("P","p",lstr)
  gsub ("Q","q",lstr)
  gsub ("R","r",lstr)
  gsub ("S","s",lstr)
  gsub ("T","t",lstr)
  gsub ("U","u",lstr)
  gsub ("V","v",lstr)
  gsub ("W","w",lstr)
  gsub ("X","x",lstr)
  gsub ("Y","y",lstr)
  gsub ("Z","z",lstr)
}
