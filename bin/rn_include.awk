#+
# File : rn_include.awk
# Description : Rename the file name in the INCLUDE statement
# 
# Author : Akiya Miyamoto,  17-Mar-1995
#
#-
#
# Main Loop
#
{
tstr = lowercase($1)
if ( tstr ~ /^include/ ) { 
  tfn = $2
  gsub( "'", "", tfn )
  unds = substr(tfn, 1, 1)
  if ( unds == "_" ) {
    $2 = "'" substr(tfn, 2) "'" }
  else { 
    $2 = "'" tfn "'" }


  nstr = lowercase($2)
  $2   = nstr
  print "      " $0
}
else {
  print
}
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

