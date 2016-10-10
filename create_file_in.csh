#!/bin/csh

set NUM = 48

set ncount = 0

rm -f file.in

LOOP:
 @ ncount = $ncount + 1
 if ( $ncount > $NUM) goto EXIT
 echo $ncount
cat <<EOF>>file.in
fake_accum.dat
1
EOF
 goto LOOP

EXIT:
 
