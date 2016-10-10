;********************************************************
; WRF: latitude-z cross section.
;********************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRFUserARW.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRF_contributed.ncl"
begin
  setfileoption("bin","WriteByteOrder","LittleEndian")

  ff      = addfile (file_in2, "w")  
 
  ; ========= 2D fields ============
  rainc  = ff->RAINC

  RAINRATE = rainc
  dsize = dimsizes(RAINRATE) 

  n_y = dsize(1)
  n_x = dsize(2)

  dims_2d = (/n_y,n_x/)
  nrec = 0
  RAINRATE(0,:,:) = fbindirread("rain_2d.gdat", nrec, dims_2d, "float")
  RAINRATE@description = "1-h ACCUMULATED TOTAL PRECIPITATION"

  ff->RAINRATE=RAINRATE
end
