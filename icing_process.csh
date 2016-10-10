#!/bin/csh

#set echo

set LOGIN = ` whoami `

set FILE_STN_LATLON = line_latlon_all_3000m.txt

set FILE_STN_NAME = stn_list_all_3000m.txt

set NBACK = ( -48 -47 -46 -45 -44 -43 -42 -41 -40 -39 -38 -37 -36 -35 -34 -33 -32 -31 -30 -29 -28 -27 -26 -25 -24 -23 -22 -21 -20 -19 -18 -17 -16 -15 -14 -13 -12 -11 -10 -9 -8 -7 -6 -5 -4 -3 -2 -1 )

# ==============
if ( ! -e /home1/$LOGIN/bin/advance_cymdh.pl ) then
 echo "ERROR advance_cymdh.pl MISSING: exit"
 exit
endif

# ======
set DOMAIN = ( d03 d04 d05 d06 d07 d08 )

# ===== figure out cycle to process =====
set GMID = $1

set MEMBER = $2

set CYCINT = $3

set NUMFCST = $4

set NUMFCST_INNER = $5

set CYCLE = $6

# =======
if ( $1 == "" || $2 == "" || $3 == "" || $4 == "" || $5 == "" ) then
 echo "MISSING GMID or MEMBER or CYCINT or NUMFCST or NUMFCST_INNER : EXIT"
 exit
endif

# ========================
if ( $6 == "" ) then
  set yr = ` date -u +%Y `
  set mn = ` date -u +%m `
  set dy = ` date -u +%d `
  set hr = ` date -u +%H `
else
  set yr = ` echo $CYCLE | cut -c1-4 `
  set mn = ` echo $CYCLE | cut -c5-6 `
  set dy = ` echo $CYCLE | cut -c7-8 `
  set hr = ` echo $CYCLE | cut -c9-10 `
endif

# ========
set hr_cycle_1 = `echo $hr | cut -c1-1`
if ( $hr_cycle_1 == 0 ) then
 set hr_use = `echo $hr | cut -c2-2`
else
 set hr_use = `echo $hr | cut -c1-2`
endif

if ( $CYCINT == 6 ) then

 if ( $hr_use >= 0 && $hr_use < 6 ) then
  set hr = 00
 else if ( $hr_use >= 6 && $hr_use < 12 ) then
  set hr = 06
 else if ( $hr_use >= 12 && $hr_use < 18 ) then
  set hr = 12
 else
  set hr = 18
 endif

else if ( $CYCINT == 3 ) then

 # ==========
 if ( $hr_use >= 0 && $hr_use < 3 ) then
  # need to find cycle 24-h ago
  set cycle_day_ago = `perl /home1/${LOGIN}/bin/advance_cymdh.pl $yr$mn$dy$hr -24 `
  set yr = ` echo $cycle_day_ago | cut -c1-4 `
  set mn = ` echo $cycle_day_ago | cut -c5-6 `
  set dy = ` echo $cycle_day_ago | cut -c7-8 `
  set hr = 23

 else if ( $hr_use >= 3 && $hr_use < 6 ) then
  set hr = 02

 else if ( $hr_use >= 6 && $hr_use < 9 ) then
  set hr = 05

 else if ( $hr_use >= 9 && $hr_use < 12 ) then
  set hr = 08

 else if ( $hr_use >= 12 && $hr_use < 15 ) then
  set hr = 11

 else if ( $hr_use >= 15 && $hr_use < 18 ) then
  set hr = 14

 else if ( $hr_use >= 18 && $hr_use < 21 ) then
  set hr = 17
 #else if ( $hr_use >= 21 ) then
 else
  set hr = 20
 endif

else
 echo "CYCINT $CYCINT not supported: EXIT"
 exit
endif

# ========
set CYCLE = $yr$mn$dy$hr

echo $CYCLE

# =======
set CYCDIR = /home1/$LOGIN/data/cycles/$GMID/$MEMBER/$CYCLE

set CODEDIR = /home1/$LOGIN/data/GMODJOBS/$GMID/housekeeping/icing

set WORKDIR = /home1/$LOGIN/data/cycles/$GMID/prod/$MEMBER/icing/working/$CYCLE

set ARC_ASCII = /home1/$LOGIN/data/cycles/$GMID/prod/$MEMBER/icing/archive/ascii/$yr/$yr$mn/$CYCLE

set ARC_ASCII_2D = /home1/$LOGIN/data/cycles/$GMID/prod/$MEMBER/icing/archive/ascii_2d/$yr/$yr$mn/$CYCLE

set ARC_NCF = /home1/$LOGIN/data/cycles/$GMID/prod/$MEMBER/icing/archive/netcdf/$yr/$yr$mn/$CYCLE

set ARC_IMAGES = /home1/$LOGIN/data/cycles/$GMID/prod/$MEMBER/icing/archive/images/$yr/$yr$mn/$CYCLE

set RUNNING_ASCII = /home1/$LOGIN/data/cycles/$GMID/prod/$MEMBER/icing/running/ascii

set RUNNING_ASCII_2D = /home1/$LOGIN/data/cycles/$GMID/prod/$MEMBER/icing/running/ascii_2d

set RUNNING_NCF = /home1/$LOGIN/data/cycles/$GMID/prod/$MEMBER/icing/running/netcdf

set RUNNING_IMAGES = /home1/$LOGIN/data/cycles/$GMID/prod/$MEMBER/icing/running/images

# =============
rm -r -f $WORKDIR
mkdir -p $WORKDIR/WRF_F
mkdir -p $WORKDIR/WRF_P

mkdir -p $ARC_ASCII/WRF_F
mkdir -p $ARC_ASCII/WRF_P

mkdir -p $ARC_ASCII_2D/WRF_F
mkdir -p $ARC_ASCII_2D/WRF_P

mkdir -p $ARC_NCF/WRF_F
mkdir -p $ARC_NCF/WRF_P

mkdir -p $ARC_IMAGES/WRF_F
mkdir -p $ARC_IMAGES/WRF_P

mkdir -p $RUNNING_ASCII

mkdir -p $RUNNING_ASCII_2D

mkdir -p $RUNNING_NCF

mkdir -p $RUNNING_IMAGES
# =========
# F-stage
set TYPE = WRF_F
cd $WORKDIR/$TYPE
cp -p $CODEDIR/$FILE_STN_LATLON .
cp -p $CODEDIR/$FILE_STN_NAME .
cp -p $CODEDIR/*.exe .
cp -p $CODEDIR/*.csh .
cp -p $CODEDIR/*.ncl .

rm -f line_latlon_all_use.txt
rm -f stn_list_all_use.txt

ln -sf $FILE_STN_LATLON line_latlon_all_use.txt 
ln -sf $FILE_STN_NAME   stn_list_all_use.txt

@ ncount = -$CYCINT - 1

LOOP1:
 @ ncount = $ncount + 1
 if ( $ncount > 0 ) goto EXIT1
 set time_now = `perl /home1/${LOGIN}/bin/advance_cymdh.pl $CYCLE $ncount `
 set yrw = ` echo $time_now |  cut -c1-4 `
 set mnw = ` echo $time_now |  cut -c5-6 `
 set dyw = ` echo $time_now |  cut -c7-8 `
 set hrw = ` echo $time_now |  cut -c9-10 `

 echo "F-stage" $ncount $CYCLE $time_now

 # ===================
 foreach dd ( $DOMAIN )

  if ( $dd == d01 ) then
   set wrffilesize = 85000000
  else if ( $dd == d02 ) then
   set wrffilesize = 100000000
  else if ( $dd == d03 ) then
   set wrffilesize = 200000000
  else if ( $dd == d04 ) then
   set wrffilesize = 60000000
  else if ( $dd == d05 ) then
   set wrffilesize = 90000000
  else if ( $dd == d06 ) then
   set wrffilesize = 67000000
  else if ( $dd == d07 ) then
   set wrffilesize = 45000000
  else if ( $dd == d08 ) then
   set wrffilesize = 70000000
  endif

  set nwait = 0
WAIT1: 
  set FILE1 = wrfout_${dd}_${yrw}-${mnw}-${dyw}_${hrw}:00:00
  set FILE2 = wrfout_${dd}_${yrw}-${mnw}-${dyw}_${hrw}:00:00.${MEMBER}_F
  if ( -e $CYCDIR/$TYPE/$FILE1 ) then
   set filesize = ` wc -c $CYCDIR/$TYPE/$FILE1 `
   if ( $filesize[1] >= $wrffilesize ) then
    ncks -O -d bottom_top,0,5 -d bottom_top_stag,0,6 $CYCDIR/$TYPE/$FILE1 -o ${FILE1}.nc
    # WRF_F
    set time_old = ` perl /home1/${LOGIN}/bin/advance_cymdh.pl $time_now -1 `
    set yro = ` echo $time_old |  cut -c1-4 `
    set mno = ` echo $time_old |  cut -c5-6 `
    set dyo = ` echo $time_old |  cut -c7-8 `
    set hro = ` echo $time_old |  cut -c9-10 `

    set CYCLE_OLD = ` perl /home1/${LOGIN}/bin/advance_cymdh.pl $CYCLE -$CYCINT `
    if ( $ncount == -$CYCINT ) then
     set wrf_old = /home1/$LOGIN/data/cycles/$GMID/prod/$MEMBER/icing/working/${CYCLE_OLD}/WRF_F/wrfout_${dd}_${yro}-${mno}-${dyo}_${hro}:00:00.nc
    else
     set wrf_old = wrfout_${dd}_${yro}-${mno}-${dyo}_${hro}:00:00.nc
    endif

    set wrf_new = ${FILE1}.nc
    rm -f rain1.nc
    rm -f rain2.nc

    ln -sf $wrf_new rain2.nc
    if ( -e $wrf_old ) then
     ln -sf $wrf_old rain1.nc
    else
     ln -sf $wrf_new rain1.nc
    endif

    create_1h_rainrate.csh
    rm -f rain1.nc
    rm -f rain2.nc

    rm -f wrfout_2d.gdat
    rm -f wrfout_3d.gdat
    rm -f wrf_grid_dim.dat
cat <<EOF>command
ncl 'file_in="${FILE1}.nc"' extract_wrf.ncl
EOF

    chmod u+x command
    command

    # ===== line ======= 
    rm -f fort.99
    rm -f file.in
    
    foreach nb ( $NBACK )
     set time_past = `perl /home1/${LOGIN}/bin/advance_cymdh.pl $time_now $nb `
     set yrb = ` echo $time_past |  cut -c1-4 `
     set mnb = ` echo $time_past |  cut -c5-6 `
     set dyb = ` echo $time_past |  cut -c7-8 `
     set hrb = ` echo $time_past |  cut -c9-10 `
     set FILE_TIME_SER = ${RUNNING_ASCII}/icing_combined_${yrb}-${mnb}-${dyb}_${hrb}:00:00.dat
     if ( -e $FILE_TIME_SER ) then
      set filesize = ` wc -c $FILE_TIME_SER `
      if ( $filesize[1] >= 100000 ) then
       set FLAG_TIME_SER = 1
      else
       set FLAG_TIME_SER = 0
      endif
     else
      set FLAG_TIME_SER = 0
     endif
cat <<EOF>>file.in
'${FILE_TIME_SER}'
$FLAG_TIME_SER
EOF
    end
    power_line_icing.exe < file.in
    mv fort.99 icing_${dd}_${yrw}-${mnw}-${dyw}_${hrw}:00:00.dat
    cp -p icing_${dd}_${yrw}-${mnw}-${dyw}_${hrw}:00:00.dat $ARC_ASCII/$TYPE/icing_${dd}_${yrw}-${mnw}-${dyw}_${hrw}:00:00.dat 

    # ===== 2D ======= 
    rm -f fort.99
    rm -f file.in
    
    foreach nb ( $NBACK )
     set time_past = `perl /home1/${LOGIN}/bin/advance_cymdh.pl $time_now $nb `
     set yrb = ` echo $time_past |  cut -c1-4 `
     set mnb = ` echo $time_past |  cut -c5-6 `
     set dyb = ` echo $time_past |  cut -c7-8 `
     set hrb = ` echo $time_past |  cut -c9-10 `
     set FILE_TIME_SER_2D = ${RUNNING_ASCII_2D}/${yrb}${mnb}${dyb}${hrb}.${dd}.2d.icingDATA.ascii
     if ( -e $FILE_TIME_SER_2D ) then
      set filesize = ` wc -c $FILE_TIME_SER_2D `
      if ( $filesize[1] >= 100000 ) then
       set FLAG_TIME_SER_2D = 1
      else
       set FLAG_TIME_SER_2D = 0
      endif
     else
      set FLAG_TIME_SER_2D = 0
     endif
cat <<EOF>>file.in
'${FILE_TIME_SER_2D}'
$FLAG_TIME_SER_2D
EOF
    end
    power_line_icing_2d.exe < file.in
    mv fort.99 ${yrw}${mnw}${dyw}${hrw}.${dd}.2d.icingDATA.ascii
    cp -p ${yrw}${mnw}${dyw}${hrw}.${dd}.2d.icingDATA.ascii $ARC_ASCII_2D/$TYPE/${yrw}${mnw}${dyw}${hrw}.${dd}.2d.icingDATA.ascii
    cp -p ${yrw}${mnw}${dyw}${hrw}.${dd}.2d.icingDATA.ascii ${RUNNING_ASCII_2D}

   else
    @ nwait = $nwait + 1
    sleep 120
    if ( $nwait > 16 ) then
     goto NOWAIT1
    else
     goto WAIT1
    endif
   endif 

  else if ( -e $CYCDIR/$FILE2 ) then
   set filesize = ` wc -c $CYCDIR/$FILE2 `
   if ( $filesize[1] >= $wrffilesize ) then
    ncks -O -d bottom_top,0,5 -d bottom_top_stag,0,6 $CYCDIR/$FILE2 -o ${FILE1}.nc

    # WRF_F
    set time_old = ` perl /home1/${LOGIN}/bin/advance_cymdh.pl $time_now -1 `
    set yro = ` echo $time_old |  cut -c1-4 `
    set mno = ` echo $time_old |  cut -c5-6 `
    set dyo = ` echo $time_old |  cut -c7-8 `
    set hro = ` echo $time_old |  cut -c9-10 `

    set CYCLE_OLD = ` perl /home1/${LOGIN}/bin/advance_cymdh.pl $CYCLE -$CYCINT `
    if ( $ncount == -$CYCINT ) then
     set wrf_old = /home1/$LOGIN/data/cycles/$GMID/prod/$MEMBER/icing/working/${CYCLE_OLD}/WRF_F/wrfout_${dd}_${yro}-${mno}-${dyo}_${hro}:00:00.nc
    else
     set wrf_old = wrfout_${dd}_${yro}-${mno}-${dyo}_${hro}:00:00.nc
    endif

    set wrf_new = ${FILE1}.nc
    rm -f rain1.nc
    rm -f rain2.nc

    ln -sf $wrf_new rain2.nc
    if ( -e $wrf_old ) then
     ln -sf $wrf_old rain1.nc
    else
     ln -sf $wrf_new rain1.nc
    endif

    create_1h_rainrate.csh
    rm -f rain1.nc
    rm -f rain2.nc

    rm -f wrfout_2d.gdat
    rm -f wrfout_3d.gdat
    rm -f wrf_grid_dim.dat
cat <<EOF>command
ncl 'file_in="${FILE1}.nc"' extract_wrf.ncl
EOF

    chmod u+x command
    command

    # ===== line ========= 
    rm -f fort.99
    rm -f file.in
    
    foreach nb ( $NBACK )
     set time_past = `perl /home1/${LOGIN}/bin/advance_cymdh.pl $time_now $nb `
     set yrb = ` echo $time_past |  cut -c1-4 `
     set mnb = ` echo $time_past |  cut -c5-6 `
     set dyb = ` echo $time_past |  cut -c7-8 `
     set hrb = ` echo $time_past |  cut -c9-10 `
     set FILE_TIME_SER = ${RUNNING_ASCII}/icing_combined_${yrb}-${mnb}-${dyb}_${hrb}:00:00.dat
     if ( -e $FILE_TIME_SER ) then
      set filesize = ` wc -c $FILE_TIME_SER `
      if ( $filesize[1] >= 100000 ) then
       set FLAG_TIME_SER = 1
      else
       set FLAG_TIME_SER = 0
      endif
     else
      set FLAG_TIME_SER = 0
     endif
cat <<EOF>>file.in
'${FILE_TIME_SER}'
$FLAG_TIME_SER
EOF
    end
    power_line_icing.exe < file.in
    mv fort.99 icing_${dd}_${yrw}-${mnw}-${dyw}_${hrw}:00:00.dat
    cp -p icing_${dd}_${yrw}-${mnw}-${dyw}_${hrw}:00:00.dat $ARC_ASCII/$TYPE/icing_${dd}_${yrw}-${mnw}-${dyw}_${hrw}:00:00.dat 

    # ===== 2D ======= 
    rm -f fort.99
    rm -f file.in
    
    foreach nb ( $NBACK )
     set time_past = `perl /home1/${LOGIN}/bin/advance_cymdh.pl $time_now $nb `
     set yrb = ` echo $time_past |  cut -c1-4 `
     set mnb = ` echo $time_past |  cut -c5-6 `
     set dyb = ` echo $time_past |  cut -c7-8 `
     set hrb = ` echo $time_past |  cut -c9-10 `
     set FILE_TIME_SER_2D = ${RUNNING_ASCII_2D}/${yrb}${mnb}${dyb}${hrb}.${dd}.2d.icingDATA.ascii
     if ( -e $FILE_TIME_SER_2D ) then
      set filesize = ` wc -c $FILE_TIME_SER_2D `
      if ( $filesize[1] >= 100000 ) then
       set FLAG_TIME_SER_2D = 1
      else
       set FLAG_TIME_SER_2D = 0
      endif
     else
      set FLAG_TIME_SER_2D = 0
     endif
cat <<EOF>>file.in
'${FILE_TIME_SER_2D}'
$FLAG_TIME_SER_2D
EOF
    end
    power_line_icing_2d.exe < file.in
    mv fort.99 ${yrw}${mnw}${dyw}${hrw}.${dd}.2d.icingDATA.ascii
    cp -p ${yrw}${mnw}${dyw}${hrw}.${dd}.2d.icingDATA.ascii $ARC_ASCII_2D/$TYPE/${yrw}${mnw}${dyw}${hrw}.${dd}.2d.icingDATA.ascii
    cp -p ${yrw}${mnw}${dyw}${hrw}.${dd}.2d.icingDATA.ascii ${RUNNING_ASCII_2D}

   else
    @ nwait = $nwait + 1
    sleep 120
    if ( $nwait > 16 ) then
     goto NOWAIT1
    else
     goto WAIT1
    endif

   endif
  else
   @ nwait = $nwait + 1
   sleep 120
   if ( $nwait > 16 ) then
    goto NOWAIT1
   else 
    goto WAIT1
   endif
  endif

NOWAIT1:
 end

 # ncks for each domain: by cycle/domain/type
 # process line for each domain
 #  - copy to archive
 # merge all domain
 # create netcdf file
 #
 # === merge files ======
 set FILE_ICE1 = icing_$DOMAIN[1]_${yrw}-${mnw}-${dyw}_${hrw}:00:00.dat
 if ( -e $FILE_ICE1 ) then
  set filesize = ` wc -c $FILE_ICE1 `
  if ( $filesize[1] >= 100000 ) then
   rm -f file.in
   foreach dd ( $DOMAIN )
    set FILE_ICE2 = icing_${dd}_${yrw}-${mnw}-${dyw}_${hrw}:00:00.dat
    if ( -e $FILE_ICE2 ) then
     set filesize2 = ` wc -c $FILE_ICE2 `
     if ( $filesize2[1] >= 100000 ) then
      set FLAG_FILE_ICE2 = 1
     else
      set FLAG_FILE_ICE2 = 0
     endif
    else
     set FLAG_FILE_ICE2 = 0
    endif

cat <<EOF>>file.in
'${FILE_ICE2}'
$FLAG_FILE_ICE2
EOF
   end

   rm -f fort.99
   merge_icing_files.exe < file.in
   mv fort.99 icing_combined_${yrw}-${mnw}-${dyw}_${hrw}:00:00.dat
   cp -p icing_combined_${yrw}-${mnw}-${dyw}_${hrw}:00:00.dat $ARC_ASCII/$TYPE
   cp -p icing_combined_${yrw}-${mnw}-${dyw}_${hrw}:00:00.dat $RUNNING_ASCII

   make_icing_netcdf.csh $CYCLE $ncount
   cp -p icing_combined_${yrw}-${mnw}-${dyw}_${hrw}:00:00.nc $ARC_NCF/$TYPE
   cp -p icing_combined_${yrw}-${mnw}-${dyw}_${hrw}:00:00.nc $RUNNING_NCF

cat <<EOF>command
ncl 'file_in="wrfout_d03_${yrw}-${mnw}-${dyw}_${hrw}:00:00.nc"' plot_powerlines_icing_ALL.ncl > /dev/null
ncl 'file_in="wrfout_d03_${yrw}-${mnw}-${dyw}_${hrw}:00:00.nc"' plot_powerlines_trans_ALL.ncl > /dev/null
EOF
   chmod u+x command
   command

   foreach dd ( $DOMAIN )
    if ( $dd != $DOMAIN[1] ) then
cat <<EOF>command
ncl 'file_in="wrfout_${dd}_${yrw}-${mnw}-${dyw}_${hrw}:00:00.nc"' plot_powerlines_icing_ALL.ncl > /dev/null
ncl 'file_in="wrfout_${dd}_${yrw}-${mnw}-${dyw}_${hrw}:00:00.nc"' plot_powerlines_trans_ALL.ncl > /dev/null
EOF
    chmod u+x command
    command
    endif
   end

   cp -p icing_plots/${yrw}${mnw}${dyw}${hrw}/*.png $RUNNING_IMAGES
   cp -p icing_plots/${yrw}${mnw}${dyw}${hrw}/*.png $ARC_IMAGES/$TYPE
   
#add by yliu 20160103
   cd $RUNNING_IMAGES 
    /home1/ncar_fdda/data/GMODJOBS/GE7CHUAN/housekeeping/icing/d3.icing_maps_overlay.pl
   cd $ARC_IMAGES/$TYPE
    /home1/ncar_fdda/data/GMODJOBS/GE7CHUAN/housekeeping/icing/d3.icing_maps_overlay.pl
#

  endif
 endif
  
 goto LOOP1
EXIT1:


# ======
# P-stage
#
set TYPE = WRF_P
cd $WORKDIR/$TYPE
cp -p $CODEDIR/$FILE_STN_LATLON .
cp -p $CODEDIR/$FILE_STN_NAME .
cp -p $CODEDIR/*.exe .
cp -p $CODEDIR/*.csh .
cp -p $CODEDIR/*.ncl .

rm -f line_latlon_all_use.txt
rm -f stn_list_all_use.txt

ln -sf $FILE_STN_LATLON line_latlon_all_use.txt
ln -sf $FILE_STN_NAME   stn_list_all_use.txt

set ncount = -1

LOOP2:
 @ ncount = $ncount + 1
 if ( $ncount > $NUMFCST ) goto EXIT2
 set time_now = `perl /home1/${LOGIN}/bin/advance_cymdh.pl $CYCLE $ncount `
 set yrw = ` echo $time_now |  cut -c1-4 `
 set mnw = ` echo $time_now |  cut -c5-6 `
 set dyw = ` echo $time_now |  cut -c7-8 `
 set hrw = ` echo $time_now |  cut -c9-10 `

 echo "P-stage" $ncount $CYCLE $time_now

 if ( $ncount > $NUMFCST_INNER ) then
  set DOMAIN = ( d03 )
 endif
 # ===================
 foreach dd ( $DOMAIN )

  if ( $dd == d01 ) then
   set wrffilesize = 85000000
  else if ( $dd == d02 ) then
   set wrffilesize = 100000000
  else if ( $dd == d03 ) then
   set wrffilesize = 200000000
  else if ( $dd == d04 ) then
   set wrffilesize = 60000000
  else if ( $dd == d05 ) then
   set wrffilesize = 90000000
  else if ( $dd == d06 ) then
   set wrffilesize = 67000000
  else if ( $dd == d07 ) then
   set wrffilesize = 45000000
  else if ( $dd == d08 ) then
   set wrffilesize = 70000000
  endif

  set nwait = 0
WAIT2: 
  set FILE1 = wrfout_${dd}_${yrw}-${mnw}-${dyw}_${hrw}:00:00
  set FILE2 = wrfout_${dd}_${yrw}-${mnw}-${dyw}_${hrw}:00:00.${MEMBER}_P+FCST
  if ( -e $CYCDIR/$TYPE/$FILE1 ) then
   set filesize = ` wc -c $CYCDIR/$TYPE/$FILE1 `
   if ( $filesize[1] >= $wrffilesize ) then
    ncks -O -d bottom_top,0,5 -d bottom_top_stag,0,6 $CYCDIR/$TYPE/$FILE1 -o ${FILE1}.nc
    # WRF_P
    set time_old = ` perl /home1/${LOGIN}/bin/advance_cymdh.pl $time_now -1 `
    set yro = ` echo $time_old |  cut -c1-4 `
    set mno = ` echo $time_old |  cut -c5-6 `
    set dyo = ` echo $time_old |  cut -c7-8 `
    set hro = ` echo $time_old |  cut -c9-10 `

    set CYCLE_OLD = ` perl /home1/${LOGIN}/bin/advance_cymdh.pl $CYCLE -$CYCINT `
    if ( $ncount == 0 ) then
     set wrf_old = /home1/$LOGIN/data/cycles/$GMID/prod/$MEMBER/icing/working/${CYCLE}/WRF_F/wrfout_${dd}_${yro}-${mno}-${dyo}_${hro}:00:00.nc
    else
     set wrf_old = wrfout_${dd}_${yro}-${mno}-${dyo}_${hro}:00:00.nc
    endif

    set wrf_new = ${FILE1}.nc
    rm -f rain1.nc
    rm -f rain2.nc

    ln -sf $wrf_new rain2.nc
    if ( -e $wrf_old ) then
     ln -sf $wrf_old rain1.nc
    else
     ln -sf $wrf_new rain1.nc
    endif

    create_1h_rainrate.csh
    rm -f rain1.nc
    rm -f rain2.nc


    rm -f wrfout_2d.gdat
    rm -f wrfout_3d.gdat
    rm -f wrf_grid_dim.dat
cat <<EOF>command
ncl 'file_in="${FILE1}.nc"' extract_wrf.ncl
EOF

    chmod u+x command
    command

    # ======  line ====== 
    rm -f fort.99
    rm -f file.in
    
    foreach nb ( $NBACK )
     set time_past = `perl /home1/${LOGIN}/bin/advance_cymdh.pl $time_now $nb `
     set yrb = ` echo $time_past |  cut -c1-4 `
     set mnb = ` echo $time_past |  cut -c5-6 `
     set dyb = ` echo $time_past |  cut -c7-8 `
     set hrb = ` echo $time_past |  cut -c9-10 `
     set FILE_TIME_SER = ${RUNNING_ASCII}/icing_combined_${yrb}-${mnb}-${dyb}_${hrb}:00:00.dat
     if ( -e $FILE_TIME_SER ) then
      set filesize = ` wc -c $FILE_TIME_SER `
      if ( $filesize[1] >= 100000 ) then
       set FLAG_TIME_SER = 1
      else
       set FLAG_TIME_SER = 0
      endif
     else
      set FLAG_TIME_SER = 0
     endif
cat <<EOF>>file.in
'${FILE_TIME_SER}'
$FLAG_TIME_SER
EOF
    end
    power_line_icing.exe < file.in
    mv fort.99 icing_${dd}_${yrw}-${mnw}-${dyw}_${hrw}:00:00.dat
    cp -p icing_${dd}_${yrw}-${mnw}-${dyw}_${hrw}:00:00.dat $ARC_ASCII/$TYPE/icing_${dd}_${yrw}-${mnw}-${dyw}_${hrw}:00:00.dat 

    # ===== 2D ======= 
    rm -f fort.99
    rm -f file.in
    
    foreach nb ( $NBACK )
     set time_past = `perl /home1/${LOGIN}/bin/advance_cymdh.pl $time_now $nb `
     set yrb = ` echo $time_past |  cut -c1-4 `
     set mnb = ` echo $time_past |  cut -c5-6 `
     set dyb = ` echo $time_past |  cut -c7-8 `
     set hrb = ` echo $time_past |  cut -c9-10 `
     set FILE_TIME_SER_2D = ${RUNNING_ASCII_2D}/${yrb}${mnb}${dyb}${hrb}.${dd}.2d.icingDATA.ascii
     if ( -e $FILE_TIME_SER_2D ) then
      set filesize = ` wc -c $FILE_TIME_SER_2D `
      if ( $filesize[1] >= 100000 ) then
       set FLAG_TIME_SER_2D = 1
      else
       set FLAG_TIME_SER_2D = 0
      endif
     else
      set FLAG_TIME_SER_2D = 0
     endif
cat <<EOF>>file.in
'${FILE_TIME_SER_2D}'
$FLAG_TIME_SER_2D
EOF
    end
    power_line_icing_2d.exe < file.in
    mv fort.99 ${yrw}${mnw}${dyw}${hrw}.${dd}.2d.icingDATA.ascii
    cp -p ${yrw}${mnw}${dyw}${hrw}.${dd}.2d.icingDATA.ascii $ARC_ASCII_2D/$TYPE/${yrw}${mnw}${dyw}${hrw}.${dd}.2d.icingDATA.ascii
    cp -p ${yrw}${mnw}${dyw}${hrw}.${dd}.2d.icingDATA.ascii ${RUNNING_ASCII_2D}

   else
    @ nwait = $nwait + 1
    sleep 120
    if ( $nwait > 16 ) then
     goto NOWAIT2
    else
     goto WAIT2
    endif
   endif 
  else if ( -e $CYCDIR/$FILE2 ) then
   set filesize = ` wc -c $CYCDIR/$FILE2 `
   if ( $filesize[1] >= $wrffilesize ) then
    ncks -O -d bottom_top,0,5 -d bottom_top_stag,0,6 $CYCDIR/$FILE2 -o ${FILE1}.nc

    # WRF_P
    set time_old = ` perl /home1/${LOGIN}/bin/advance_cymdh.pl $time_now -1 `
    set yro = ` echo $time_old |  cut -c1-4 `
    set mno = ` echo $time_old |  cut -c5-6 `
    set dyo = ` echo $time_old |  cut -c7-8 `
    set hro = ` echo $time_old |  cut -c9-10 `

    set CYCLE_OLD = ` perl /home1/${LOGIN}/bin/advance_cymdh.pl $CYCLE -$CYCINT `
    if ( $ncount == 0 ) then
     set wrf_old = /home1/$LOGIN/data/cycles/$GMID/prod/$MEMBER/icing/working/${CYCLE}/WRF_F/wrfout_${dd}_${yro}-${mno}-${dyo}_${hro}:00:00.nc
    else
     set wrf_old = wrfout_${dd}_${yro}-${mno}-${dyo}_${hro}:00:00.nc
    endif

    set wrf_new = ${FILE1}.nc
    rm -f rain1.nc
    rm -f rain2.nc

    ln -sf $wrf_new rain2.nc
    if ( -e $wrf_old ) then
     ln -sf $wrf_old rain1.nc
    else
     ln -sf $wrf_new rain1.nc
    endif

    create_1h_rainrate.csh
    rm -f rain1.nc
    rm -f rain2.nc

    rm -f wrfout_2d.gdat
    rm -f wrfout_3d.gdat
    rm -f wrf_grid_dim.dat
cat <<EOF>command
ncl 'file_in="${FILE1}.nc"' extract_wrf.ncl
EOF

    chmod u+x command
    command
 
    rm -f fort.99
    rm -f file.in
    
    foreach nb ( $NBACK )
     set time_past = `perl /home1/${LOGIN}/bin/advance_cymdh.pl $time_now $nb `
     set yrb = ` echo $time_past |  cut -c1-4 `
     set mnb = ` echo $time_past |  cut -c5-6 `
     set dyb = ` echo $time_past |  cut -c7-8 `
     set hrb = ` echo $time_past |  cut -c9-10 `
     set FILE_TIME_SER = ${RUNNING_ASCII}/icing_combined_${yrb}-${mnb}-${dyb}_${hrb}:00:00.dat
     if ( -e $FILE_TIME_SER ) then
      set filesize = ` wc -c $FILE_TIME_SER `
      if ( $filesize[1] >= 100000 ) then
       set FLAG_TIME_SER = 1
      else
       set FLAG_TIME_SER = 0
      endif
     else
      set FLAG_TIME_SER = 0
     endif
cat <<EOF>>file.in
'${FILE_TIME_SER}'
$FLAG_TIME_SER
EOF
    end
    power_line_icing.exe < file.in
    mv fort.99 icing_${dd}_${yrw}-${mnw}-${dyw}_${hrw}:00:00.dat
    cp -p icing_${dd}_${yrw}-${mnw}-${dyw}_${hrw}:00:00.dat $ARC_ASCII/$TYPE/icing_${dd}_${yrw}-${mnw}-${dyw}_${hrw}:00:00.dat 

    # ===== 2D ======= 
    rm -f fort.99
    rm -f file.in
    
    foreach nb ( $NBACK )
     set time_past = `perl /home1/${LOGIN}/bin/advance_cymdh.pl $time_now $nb `
     set yrb = ` echo $time_past |  cut -c1-4 `
     set mnb = ` echo $time_past |  cut -c5-6 `
     set dyb = ` echo $time_past |  cut -c7-8 `
     set hrb = ` echo $time_past |  cut -c9-10 `
     set FILE_TIME_SER_2D = ${RUNNING_ASCII_2D}/${yrb}${mnb}${dyb}${hrb}.${dd}.2d.icingDATA.ascii
     if ( -e $FILE_TIME_SER_2D ) then
      set filesize = ` wc -c $FILE_TIME_SER_2D `
      if ( $filesize[1] >= 100000 ) then
       set FLAG_TIME_SER_2D = 1
      else
       set FLAG_TIME_SER_2D = 0
      endif
     else
      set FLAG_TIME_SER_2D = 0
     endif
cat <<EOF>>file.in
'${FILE_TIME_SER_2D}'
$FLAG_TIME_SER_2D
EOF
    end
    power_line_icing_2d.exe < file.in
    mv fort.99 ${yrw}${mnw}${dyw}${hrw}.${dd}.2d.icingDATA.ascii
    cp -p ${yrw}${mnw}${dyw}${hrw}.${dd}.2d.icingDATA.ascii $ARC_ASCII_2D/$TYPE/${yrw}${mnw}${dyw}${hrw}.${dd}.2d.icingDATA.ascii
    cp -p ${yrw}${mnw}${dyw}${hrw}.${dd}.2d.icingDATA.ascii ${RUNNING_ASCII_2D}

   else
    @ nwait = $nwait + 1
    sleep 120
    if ( $nwait > 16 ) then
     goto NOWAIT2
    else
     goto WAIT2
    endif
   endif 
  else
   @ nwait = $nwait + 1
   sleep 120
   if ( $nwait > 16 ) then
    goto NOWAIT2
   else 
    goto WAIT2
   endif
  endif

NOWAIT2:
 end

 # === merge files ======
 set FILE_ICE1 = icing_$DOMAIN[1]_${yrw}-${mnw}-${dyw}_${hrw}:00:00.dat
 if ( -e $FILE_ICE1 ) then
  set filesize = ` wc -c $FILE_ICE1 `
  if ( $filesize[1] >= 100000 ) then
   rm -f file.in
   foreach dd ( $DOMAIN )
    set FILE_ICE2 = icing_${dd}_${yrw}-${mnw}-${dyw}_${hrw}:00:00.dat
    if ( -e $FILE_ICE2 ) then
     set filesize2 = ` wc -c $FILE_ICE2 `
     if ( $filesize2[1] >= 100000 ) then
      set FLAG_FILE_ICE2 = 1
     else
      set FLAG_FILE_ICE2 = 0
     endif
    else
     set FLAG_FILE_ICE2 = 0
    endif

cat <<EOF>>file.in
'${FILE_ICE2}'
$FLAG_FILE_ICE2
EOF
   end

   rm -f fort.99
   merge_icing_files.exe < file.in
   mv fort.99 icing_combined_${yrw}-${mnw}-${dyw}_${hrw}:00:00.dat
   cp -p icing_combined_${yrw}-${mnw}-${dyw}_${hrw}:00:00.dat $ARC_ASCII/$TYPE
   cp -p icing_combined_${yrw}-${mnw}-${dyw}_${hrw}:00:00.dat $RUNNING_ASCII

   make_icing_netcdf.csh $CYCLE $ncount
   cp -p icing_combined_${yrw}-${mnw}-${dyw}_${hrw}:00:00.nc $ARC_NCF/$TYPE
   cp -p icing_combined_${yrw}-${mnw}-${dyw}_${hrw}:00:00.nc $RUNNING_NCF

cat <<EOF>command
ncl 'file_in="wrfout_d03_${yrw}-${mnw}-${dyw}_${hrw}:00:00.nc"' plot_powerlines_icing_ALL.ncl > /dev/null
ncl 'file_in="wrfout_d03_${yrw}-${mnw}-${dyw}_${hrw}:00:00.nc"' plot_powerlines_trans_ALL.ncl > /dev/null
EOF
   chmod u+x command
   command

   foreach dd ( $DOMAIN )
    if ( $dd != $DOMAIN[1] ) then
cat <<EOF>command
ncl 'file_in="wrfout_${dd}_${yrw}-${mnw}-${dyw}_${hrw}:00:00.nc"' plot_powerlines_icing_ALL.ncl > /dev/null
ncl 'file_in="wrfout_${dd}_${yrw}-${mnw}-${dyw}_${hrw}:00:00.nc"' plot_powerlines_trans_ALL.ncl > /dev/null
EOF
    chmod u+x command
    command
    endif
   end

   cp -p icing_plots/${yrw}${mnw}${dyw}${hrw}/*.png $RUNNING_IMAGES
   cp -p icing_plots/${yrw}${mnw}${dyw}${hrw}/*.png $ARC_IMAGES/$TYPE
 
#add by yliu 20160103
   cd $RUNNING_IMAGES 
    /home1/ncar_fdda/data/GMODJOBS/GE7CHUAN/housekeeping/icing/d3.icing_maps_overlay.pl
   cd $ARC_IMAGES/$TYPE
    /home1/ncar_fdda/data/GMODJOBS/GE7CHUAN/housekeeping/icing/d3.icing_maps_overlay.pl
#

  endif
 endif
  
 goto LOOP2
EXIT2:
