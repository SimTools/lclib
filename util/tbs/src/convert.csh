#! /bin/csh
foreach x (org/*.f)
  set file=$x:r
  set file=$file:t
  set file=$file.F
  nawk -f convert.awk < $x > $file
end
exit
