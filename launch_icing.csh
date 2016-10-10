#!/bin/csh

set NODE = $1

if ( $1 == "" ) then
 echo "node input missing: EXIT"
 exit
endif

# =====
set LOGIN = ` whoami `


# ====
set PATH_OUT = /home1/$LOGIN/data/GMODJOBS/GE7CHUAN/housekeeping/icing

ssh node$NODE -v << EOF
cd $PATH_OUT
nohup icing_process.pl > /dev/null &
exit
EOF

