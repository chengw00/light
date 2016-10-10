#!/bin/csh

set LOGIN = ` whoami `

#set CYCLE = 2015121200

#set FCST_HOUR = 1

# =============
if ($#argv < 2) then
 echo "CYCLE & FCST_HOUR are needed"
 echo "usage:  $0  CYCLE FCST_HOUR"
 exit
else
 set CYCLE = $1
 set FCST_HOUR = $2
endif

set VALID_TIME = ` /home/$LOGIN/bin/advance_cymdh.pl $CYCLE $FCST_HOUR `

# =======
set yrv = ` echo $VALID_TIME | cut -c1-4 `
set mnv = ` echo $VALID_TIME | cut -c5-6 `
set dyv = ` echo $VALID_TIME | cut -c7-8 `
set hrv = ` echo $VALID_TIME | cut -c9-10 `

set yrv1 = ` echo $yrv | cut -c1-1 `
set yrv2 = ` echo $yrv | cut -c2-2 ` 
set yrv3 = ` echo $yrv | cut -c3-3 `
set yrv4 = ` echo $yrv | cut -c4-4 `

set mnv1 = ` echo $mnv | cut -c1-1 `
set mnv2 = ` echo $mnv | cut -c2-2 `

set dyv1 = ` echo $dyv | cut -c1-1 `
set dyv2 = ` echo $dyv | cut -c2-2 `

set hrv1 = ` echo $hrv | cut -c1-1 `
set hrv2 = ` echo $hrv | cut -c2-2 `

# ===
if ( ! -e calc_unix_time.exe ) then
 echo "calc_unix_time.exe does NOT exist: EXIT"
 exit
endif

set unix_time = ` calc_unix_time.exe $yrv $mnv $dyv $hrv 00 `

# ==========
set FILE_IN  = icing_combined_${yrv}-${mnv}-${dyv}_${hrv}:00:00.dat
set FILE_OUT = icing_combined_${yrv}-${mnv}-${dyv}_${hrv}:00:00.nc

if ( -e $FILE_IN ) then
 set filesize = ` wc -c $FILE_IN `
 if ( $filesize[1] < 100000 ) goto EXIT
else
 goto EXIT
endif

rm -f $FILE_OUT

cat <<EOF>test.ncl
;********************************************************
; WRF: latitude-z cross section.
;********************************************************
load "\$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "\$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "\$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin
 fout = addfile("${FILE_OUT}","c")

 Att = True
 Att@CYCLE = $CYCLE

 fileattdef(fout,Att)
 delete (Att)

 ; =====
 time2 = 1
 ;stations2 = 34
 station_id_len2 = 6
 station_tt_len2 = station_id_len2
 date_len2 = 19

 ; ======= read station list =======
 stn_list = asciiread ("stn_list_all_use.txt",-1,"string")
 printVarSummary(stn_list)

 print(stn_list(0))
 
 stations2 = dimsizes(stn_list)

 ; ====== read lat/lon ======
 ncol = 3
 data = readAsciiTable("line_latlon_all_use.txt", ncol, "double", 0)

 lat = (/data(:,0)/)
 lon = (/data(:,1)/)

 delete(data)

 ; ======== read variables =========
 ncol = 12
 data2 = readAsciiTable("${FILE_IN}", ncol, "float", 0)

 ; ====== define dimension =====================
 dim_names = (/ "time",  "stations", "station_id_len", "station_tt_len", "date_len"/)
 dim_sizes = (/  time2,    stations2,   station_id_len2,   station_tt_len2,   date_len2/)
 dimUnlim = (/   True ,      False,           False,    False,            False/)
 filedimdef( fout, dim_names, dim_sizes, dimUnlim )

 ; =================================

 ;template_station_id = new((/stations2,station_id_len2/),character)
 ;station_id = template_station_id
 ;station_id!0 = "stations"
 ;station_id!1 = "station_id_len"


 ;station_id@long_name = "Station ID (6 characters)"

 station_id_c = stringtochar(stn_list)
 printVarSummary(station_id_c)

 filevardef(fout,"station_id","character", \
            (/"stations","station_id_len"/))
 Att           = True
 Att@long_name = "Station ID (6 characters)"
 filevarattdef (fout,"station_id",Att)
 delete (Att)
 fout->station_id=(/station_id_c(:,:5)/)

 ; ==== station name =======
 filevardef(fout,"station_name","character", \
            (/"stations","station_tt_len"/))
 Att           = True
 Att@long_name = "Station name (6 characters)"
 filevarattdef (fout,"station_name",Att)
 delete (Att)
 fout->station_name=(/station_id_c(:,:5)/)

 ; ====== latitude =========
 filevardef(fout,"ylat","double","stations")
 Att           = True
 Att@long_name = "Latitude North of stations"
 Att@units     = "Degrees"
 filevarattdef (fout,"ylat",Att)
 delete (Att)
 fout->ylat=(/lat/)
 fout->ylat@_FillValue = -999d

 ; ========longitude =======
 filevardef(fout,"xlon","double","stations")
 Att           = True
 Att@long_name = "Longitude East of stations"
 Att@units     = "Degrees"
 filevarattdef (fout,"xlon",Att)
 delete (Att)
 fout->xlon=(/lon/)
 fout->xlon@_FillValue = -999d
  
 ; ====== date =======
 template_date = new((/time2,date_len2/),character)
 date = template_date
 date!0 = "time"
 date!1 = "date_len"

 test1 = stringtochar("$yrv1")
 date(0,0) = test1(0)
 test1 = stringtochar("$yrv2")
 date(0,1) = test1(0)
 test1 = stringtochar("$yrv3")
 date(0,2) = test1(0)
 test1 = stringtochar("$yrv4")
 date(0,3) = test1(0)

 test1 = stringtochar("-")
 date(0,4) = test1(0)

 test1 = stringtochar("$mnv1")
 date(0,5) = test1(0)
 test1 = stringtochar("$mnv2")
 date(0,6) = test1(0)

 test1 = stringtochar("-")
 date(0,7) = test1(0)

 test1 = stringtochar("$dyv1")
 date(0,8) = test1(0)
 test1 = stringtochar("$dyv2")
 date(0,9) = test1(0)

 test1 = stringtochar("_")
 date(0,10) = test1(0)

 test1 = stringtochar("$hrv1")
 date(0,11) = test1(0)
 test1 = stringtochar("$hrv2")
 date(0,12) = test1(0)

 test1 = stringtochar(":")
 date(0,13) = test1(0)

 test1 = stringtochar("0")
 date(0,14) = test1(0)
 date(0,15) = test1(0)

 test1 = stringtochar(":")
 date(0,16) = test1(0)

 test1 = stringtochar("0")
 date(0,17) = test1(0)
 date(0,18) = test1(0)

 filevardef(fout,"date","character",(/"time","date_len"/))
 Att           = True
 Att@long_name = "Current Date"
 Att@units     = "yyyymmddhhmn"
 filevarattdef (fout,"date",Att)
 delete (Att)
 fout->date=(/date/)

 ; ===== Unix time ========
 unix_time = ${unix_time}.

 filevardef(fout,"time","double","time")
 Att           = True
 Att@long_name = "Current Time"
 Att@units     = "seconds since 1970-01-01_00:00:00"
 filevarattdef (fout,"time",Att)
 delete (Att)
 fout->time=(/unix_time/)
 fout->time@_FillValue = -999d

 ; ========
 ; forecast lead time (minutes)

 fhr = $FCST_HOUR * 60.

 filevardef(fout,"forecast_lead_time","float","time")
 Att           = True
 Att@long_name = "forecast lead time"
 Att@units     = "minutes"
 filevarattdef (fout,"forecast_lead_time",Att)
 delete (Att)
 fout->forecast_lead_time=(/fhr/)
 fout->forecast_lead_time@_FillValue = -999.

 ; =============
 ; output variable template
 ;
 template_output = new((/time2,stations2/),float,-999.)
 ; =========
 ; RH (%)
 RHmod = template_output
 RHmod(0,:) = data2(:,5-1)

 filevardef(fout,"RHmod","float",(/"time","stations"/))
 Att           = True
 Att@long_name = "Relative Humidity at Power Line"
 Att@units     = "%"
 filevarattdef (fout,"RHmod",Att)
 delete (Att)
 fout->RHmod=(/RHmod/)
 fout->RHmod@_FillValue = -999.

 ; =======
 ; ql (kg/kg)
 QLmod = template_output
 QLmod(0,:) = data2(:,8-1)

 filevardef(fout,"QLmod","float",(/"time","stations"/))
 Att           = True
 Att@long_name = "Liquid Water Mixing Ratio at Power Line"
 Att@units     = "kg/kg"
 filevarattdef (fout,"QLmod",Att)
 delete (Att)
 fout->QLmod=(/QLmod/)
 fout->QLmod@_FillValue = -999.

 ; ==========
 ; qcttot (kg/kg)
 QCTmod = template_output
 QCTmod(0,:) = data2(:,9-1) 

 filevardef(fout,"QCTmod","float",(/"time","stations"/))
 Att           = True
 Att@long_name = "Total Condensate Mixing Ratio at Power Line"
 Att@units     = "kg/kg"
 filevarattdef (fout,"QCTmod",Att)
 delete (Att)
 fout->QCTmod=(/QCTmod/)
 fout->QCTmod@_FillValue = -999.

 ; ==========
 ; air density (kg/m**3)
 DENmod = template_output
 DENmod(0,:) = data2(:,4-1)

 filevardef(fout,"DENmod","float",(/"time","stations"/))
 Att           = True
 Att@long_name = "Air Density at Power Line"
 Att@units     = "kg/m*3"
 filevarattdef (fout,"DENmod",Att)
 delete (Att)
 fout->DENmod=(/DENmod/)
 fout->DENmod@_FillValue = -999.

 ; ================
 ; wind speed (m/s)
 SPDmod = template_output
 SPDmod(0,:) = data2(:,6-1)

 filevardef(fout,"SPDmod","float",(/"time","stations"/))
 Att           = True
 Att@long_name = "Wind Speed at Power Line"
 Att@units     = "m/s"
 filevarattdef (fout,"SPDmod",Att)
 delete (Att)
 fout->SPDmod=(/SPDmod/)
 fout->SPDmod@_FillValue = -999.

 ; =========
 ; wind direction (deg)
 DIRmod = template_output
 DIRmod(0,:) = data2(:,7-1)

 filevardef(fout,"DIRmod","float",(/"time","stations"/))
 Att           = True
 Att@long_name = "Earth Relative Wind Direction at Power Line"
 Att@units     = "deg"
 filevarattdef (fout,"DIRmod",Att)
 delete (Att)
 fout->DIRmod=(/DIRmod/)
 fout->DIRmod@_FillValue = -999.

 ; =========
 ; temperature (deg C)
 Tmod = template_output
 Tmod(0,:) = data2(:,3-1)

 filevardef(fout,"Tmod","float",(/"time","stations"/))
 Att           = True
 Att@long_name = "Temperature at Power Line"
 Att@units     = "deg C"
 filevarattdef (fout,"Tmod",Att)
 delete (Att)
 fout->Tmod=(/Tmod/)
 fout->Tmod@_FillValue = -999.

 ; ==========
 ; downward SW radiation (W/m**2)
 SWDOWNmod = template_output
 SWDOWNmod(0,:) = data2(:,10-1)

 filevardef(fout,"SWDOWNmod","float",(/"time","stations"/))
 Att           = True
 Att@long_name = "Downward SW Radiation at Surface"
 Att@units     = "W/m**2"
 filevarattdef (fout,"SWDOWNmod",Att)
 delete (Att)
 fout->SWDOWNmod=(/SWDOWNmod/)
 fout->SWDOWNmod@_FillValue = -999.
 
 ; ========
 ; instantaneous ice growth rate (mm/h)
 INSIGRmod = template_output
 INSIGRmod(0,:) = data2(:,11-1)

 filevardef(fout,"INSIGRmod","float",(/"time","stations"/))
 Att           = True
 Att@long_name = "Instantaneous Ice Growth Rate at Power Line"
 Att@units     = "mm/h"
 filevarattdef (fout,"INSIGRmod",Att)
 delete (Att)
 fout->INSIGRmod=(/INSIGRmod/)
 fout->INSIGRmod@_FillValue = -999.

 ; ==========
 ; accumulated ice growth (mm)
 ACCIGRmod = template_output
 ACCIGRmod(0,:) = data2(:,12-1)

 filevardef(fout,"ACCIGRmod","float",(/"time","stations"/))
 Att           = True
 Att@long_name = "Accumulated Ice Growth at Power Line"
 Att@units     = "mm"
 filevarattdef (fout,"ACCIGRmod",Att)
 delete (Att)
 fout->ACCIGRmod=(/ACCIGRmod/)
 fout->ACCIGRmod@_FillValue = -999.
end
EOF

ncl test.ncl

# ======
EXIT:

