#!/bin/csh

# 1: extract accum
#
set FILE1 = rain1.nc
set FILE2 = rain2.nc

rm -f rain_2d.gdat

cat <<EOF>command
ncl 'file_in1="$FILE1"' 'file_in2="$FILE2"' extract_rain.ncl
EOF

chmod u+x command
command

# 2: read accum and modify
# 
#
cat <<EOF>command
ncl 'file_in2="$FILE2"' create_rain1h.ncl
EOF

chmod u+x command
command

rm -f rain_2d.gdat
