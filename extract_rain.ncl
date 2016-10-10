;********************************************************
; WRF: latitude-z cross section.
;********************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRFUserARW.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRF_contributed.ncl"
begin
  setfileoption("bin","WriteByteOrder","LittleEndian")

  file_2d_out = "rain_2d.gdat"
  
  fall      = addfile (file_in1, "r")  
  gall      = addfile (file_in2, "r")
 
  ; ========= 2D fields ============
  rainc1  = fall->RAINC
  rainnc1 = fall->RAINNC

  rainc2  = gall->RAINC
  rainnc2 = gall->RAINNC

  ; ======
  rain1h = (rainc2+rainnc2)-(rainc1+rainnc1)

  ; =========
  fbindirwrite(file_2d_out,rain1h)


end
