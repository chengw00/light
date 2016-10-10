#!/bin/csh

# ======================
set LOGIN = ` whoami `

set HOME = $1

set NODE = $2

if ( $1 == "" || $2 == "" ) then
 echo "MISSING INPUT: exit "
 exit
endif

if ( ! -e $HOME ) then
 echo "$HOME does not exist: EXIT"
 exit
endif

rm -f $HOME/pid_${NODE}.txt

ps -u $LOGIN > $HOME/pid_${NODE}.txt

# ==== edit file
ex $HOME/pid_${NODE}.txt <<EOF
1, 1d
:g/?/s//x/g
wq
EOF

# ========
foreach linec ( " ` cat $HOME/pid_${NODE}.txt ` " )

 set argv = ( $linec )
 echo $argv[1]
 set PID = $argv[1]
 set JOB = $argv[4]

 set job5 = ` echo $JOB | cut -c1-5 `

 if ( ( $job5 == "icing" ) ) then
  echo $argv[1] $job5
  kill -9 $argv[1]
 endif

end


