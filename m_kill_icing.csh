#!/bin/csh

set LOGIN = ` whoami `

#set HOME = /home1/$LOGIN/vtmp20150721

#set NODE = ( 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 )
#set NODE = ( 1 2 3 4 5 6 7 8 )

#set NODE = ( 1 2 3 4 5 6 7 8 9 10 11 12 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 37 38 39 40 41 42 43 44 )
#set NODE = ( 9  )

set GMID = $1

set NODE = $2

if ( $1 == "" || $2 == "" ) then
 echo "MISSING INPUT: exit"
 exit
endif

set HOME = /home1/$LOGIN/data/GMODJOBS/$GMID/housekeeping/icing

# =================
foreach nn ( $NODE )

#ssh node$nn -v  << EOF
$HOME/killjobs_icing.csh $HOME node$nn
$HOME/killjobs_icing.csh $HOME node$nn
$HOME/killjobs_icing.csh $HOME node$nn
#logout
#EOF

end

