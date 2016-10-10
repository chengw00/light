;********************************************************
; WRF: latitude-z cross section.
;********************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRFUserARW.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRF_contributed.ncl"
begin
  setfileoption("bin","WriteByteOrder","LittleEndian")
  ;file_in = "wrfout.nc"

  file_wrf_grid_dim_out = "wrf_grid_dim.dat"

  file_maproj_out = "maproj.dat"
  file_truelat1_out = "truelat1.dat"
  file_truelat2_out = "truelat2.dat"
  file_stand_lon_out = "stand_lon.dat"

  file_2d_out = "wrfout_2d.gdat"
  file_3d_out = "wrfout_3d.gdat"
  
  fall      = addfile (file_in, "r")  

  ; ====== grid properties =======
  res           = True

  res@MAP_PROJ  = fall@MAP_PROJ
  res@TRUELAT1  = fall@TRUELAT1
  res@TRUELAT2  = fall@TRUELAT2
  res@STAND_LON = fall@STAND_LON
  res@DX        = fall@DX
  res@DY        = fall@DY

  asciiwrite(file_maproj_out,res@MAP_PROJ)
  asciiwrite(file_truelat1_out,res@TRUELAT1)
  asciiwrite(file_truelat2_out,res@TRUELAT2)
  asciiwrite(file_stand_lon_out,res@STAND_LON)
 
  ; ========= 2D fields ============
  xlat = fall->XLAT
  xlon = fall->XLONG
  hgt  = fall->HGT
  swdown = fall->SWDOWN
  rainrate= fall->RAINRATE

  ; ========= 3D fields =========
  it = 0
  ua = wrf_user_getvar(fall,"ua",it)
  va = wrf_user_getvar(fall,"va",it)
  tc = wrf_user_getvar(fall,"tc",it)      ; 3D tc
  pressure = wrf_user_getvar(fall,"pressure",it) ; (hPa)
  height = wrf_user_getvar(fall,"z",it)
  rh = wrf_user_getvar(fall,"rh",it)

  qcloud = fall->QCLOUD
  qrain  = fall->QRAIN
  qice   = fall->QICE
  qsnow  = fall->QSNOW
  qgraup = fall->QGRAUP

  ql     = qcloud + qrain
  qhydromet = qcloud + qrain + qice + qsnow + qgraup
  speed  = sqrt(ua*ua + va*va)

  ;printVarSummary(pressure)
  ;printVarSummary(ua)
  printVarSummary(rh)

  ; ====
  ; output grid dimensions
  dsizes_3d = dimsizes(height)
  asciiwrite(file_wrf_grid_dim_out,dsizes_3d) 

  ; =========
  fbindirwrite(file_2d_out,xlat)
  fbindirwrite(file_2d_out,xlon)
  fbindirwrite(file_2d_out,hgt)
  fbindirwrite(file_2d_out,swdown) 
  fbindirwrite(file_2d_out,rainrate)

  ; ===============
  fbindirwrite(file_3d_out,pressure)
  fbindirwrite(file_3d_out,height)
  fbindirwrite(file_3d_out,tc)
  fbindirwrite(file_3d_out,ua)
  fbindirwrite(file_3d_out,va)
  fbindirwrite(file_3d_out,rh)
  fbindirwrite(file_3d_out,ql)
  fbindirwrite(file_3d_out,qhydromet)

end
