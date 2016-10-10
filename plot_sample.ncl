;*************************************************
; raster_3.ncl
;************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRFUserARW.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRF_contributed.ncl"

begin
  ; ======== number of stations for met tower =======
  num_mt = 2

  ff = addfile("wrfout.nc","r")
  setfileoption("bin","WriteByteOrder","LittleEndian")

  xlat      = ff->XLAT
  xlon      = ff->XLONG
  hgt       = ff->HGT

  phb = ff->PHB
  ph  = ff->PH
  pht = phb+ph

  ; ==============
  tlat1     = ff@TRUELAT1
  tlat2     = ff@TRUELAT2
  clat      = ff@MOAD_CEN_LAT
  clon      = ff@STAND_LON

  ; ===============
  dsizes_pht = dimsizes(pht)
  n_z = dsizes_pht(1)
  n_y = dsizes_pht(2)
  n_x = dsizes_pht(3)

  print(n_x)
  print(n_y)

  ; ==========
  hgt@lat2d = xlat(0,:,:)
  hgt@lon2d = xlon(0,:,:)

  ; ====== get coordinate of power lines ======
  stn_file = "sample.dat"

  ncol = 12
  data = readAsciiTable(stn_file, ncol, "float", 0)

  lat = (/data(:,0)/)
  lon = (/data(:,1)/)
  pwv = (/data(:,10)/)

  delete(data)

  ; ========
  res           = True

; ======= plot map ===========
  wks = gsn_open_wks("pdf","raster")

  gsn_define_colormap(wks,"gsltod")
  setvalues wks
    "wkColorMap"        : "gsltod"
    "wkForegroundColor" : (/0.,0.,0./)
    "wkBackgroundColor" : (/1.,1.,1./)
  end setvalues

  ;gsn_define_colormap(wks,"hlu_default")
  ;gsn_merge_colormaps(wks,"hlu_default","gsltod")

  ; ==== WC: define my own colors ========
  cmap = RGBtoCmap("color_ybl_precip.txt")
  gsn_define_colormap(wks,cmap)
  gsn_merge_colormaps(wks,cmap,"gsltod")

  res                             = True
  res@gsnDraw                     = False
  res@gsnFrame                    = False
  res@gsnMaximize                 = True

  res@gsnSpreadColors             = True
  res@gsnSpreadColorStart         = 32
  res@gsnSpreadColorEnd           = 62
  res@pmTickMarkDisplayMode       = "Always"

  res@cnLevelSelectionMode        = "ManualLevels"
  res@cnMinLevelValF              = 0
  res@cnMaxLevelValF              = 7000
  res@cnLevelSpacingF             = 1000

  res@gsnLeftString             =""
  res@gsnRightString             =""

  ;res@cnFillOn                    = True
  ;res@cnLinesOn                   = False
  res@cnFillOn                    = False
  res@cnLinesOn                   = True
  res@cnLineLabelsOn              = False

  res@lbLabelStride               = 20
  res@lbBoxLinesOn                = False
  res@lbLabelStride  = 3

  res@mpProjection                = "LambertConformal"
  res@mpLambertParallel1F         = tlat1
  res@mpLambertParallel2F         = tlat2
  res@mpLambertMeridianF          = clon

  res@mpLimitMode                 = "Corners"
  res@mpLeftCornerLatF            = xlat(0,0,0)
  res@mpLeftCornerLonF            = xlon(0,0,0)
  res@mpRightCornerLatF           = xlat(0,n_y-1,n_x-1)
  res@mpRightCornerLonF           = xlon(0,n_y-1,n_x-1)

  ;res@mpLeftCornerLatF            = xlat1
  ;res@mpLeftCornerLonF            = xlon1
  ;res@mpRightCornerLatF           = xlat2
  ;res@mpRightCornerLonF           = xlon2

  res@mpFillOn                    = False
  ;res@mpOutlineDrawOrder          = "PostDraw"
  res@mpFillDrawOrder             = "PreDraw"
  ;res@mpOutlineBoundarySets       = "GeophysicalAndUSStates"
  res@mpOutlineBoundarySets       = "National"
  res@mpUSStateLineColor          = "Gray10"
  res@mpUSStateLineDashPattern    = 2
  res@mpOutlineSpecifiers = (/"China:states"/)
  res@mpDataBaseVersion     = "MediumRes"
  res@mpDataSetName = "Earth..4"

  res@mpGridLineDashPattern  = 2
  res@pmTickMarkDisplayMode  = "Always"

  res@tiMainString           = "sample d03"
  res@tiMainFontHeightF      = 0.020              ; smaller title

  res@gsnAddCyclic           = False

  res@tfDoNDCOverlay = True

  map = gsn_csm_contour_map(wks,hgt(0,:,:),res)

; ========= now do the dots =========
; added by WC: 2009-02-13
; 1:   white
; 12:  purple
; 6:   (light) blue
; 3:   green
; 5:   yellow
; 2:   red
; 18:  magenta
; 13:  orange
;
;
; ===== use
; 6: light blue
; 4: blue
; 16: purple
; 3: green
; 2: red
;
;-------Options--------

  ;arr = (/0.,2.,4.,6.,8.,10.,12.,14.,16.,18.,20.,22./)    ; bin settings (bin0 = < 0.,

  ;colors = (/0,0,2,3,4,5,6,7,8,10,12,13,19/)  ; bin1 = 0.:4.999, etc.)

  arr = (/0., 0.001, 0.5, 1.0, 2.6, 5.1/)    ; bin settings (bin0 = < 0.,
  ; ======== YBL colors =======
  colors = (/0,0,4,6,8,10,12/)  ; bin1 = 0.:4.999, etc.)

  ; ==== original colors ========
  ;colors = (/0,0,6,4,16,3,2/)  ; bin1 = 0.:4.999, etc.)
  ;colors = (/0,0,2,3,4,5,6/)  ; bin1 = 0.:4.999, etc.)

 
  labels = new(dimsizes(arr)+1,string)  ; Labels for legend.

  num_distinct_markers = dimsizes(arr)+1        ; number of distinct markers
  lat_new = new((/num_distinct_markers,dimsizes(pwv)/),float,-999)
  lon_new = new((/num_distinct_markers,dimsizes(pwv)/),float,-999)

   do i = 0, num_distinct_markers-1
    if (i.eq.0) then
      indexes = ind(pwv.lt.arr(0))
      labels(i) = "x < " + arr(0)
    end if
    if (i.eq.num_distinct_markers-1) then
      indexes = ind(pwv.ge.max(arr))
      labels(i) = "wspd >= " + max(arr)
    end if
    if (i.gt.0.and.i.lt.num_distinct_markers-1) then
      indexes = ind(pwv.ge.arr(i-1).and.pwv.lt.arr(i))
      labels(i) = arr(i-1) + " <= x < " + arr(i)
    end if

;
; Now that we have the set of indexes whose values fall within
; the given range, take the corresponding lat/lon values and store
; them, so later we can color this set of markers with the appropriate
; color.
;
    if (.not.any(ismissing(indexes))) then
      npts_range = dimsizes(indexes)   ; # of points in this range.
      lat_new(i,0:npts_range-1) = lat(indexes)
      lon_new(i,0:npts_range-1) = lon(indexes)
    end if
    delete(indexes)            ; Necessary b/c "indexes" may be a different
                               ; size next time.
  end do

;
;  Create logical variables to hold the marker and text resources.
;  These markers are different than the XY markers, because they are not
;  associated with an XY plot. You can put these markers on any plot.
;
  gsres               = True
  gsres@gsMarkerIndex = 16          ; Use filled dots for markers.
  ;gsres@gsMarkerIndex =  4           ; hole in the middle

  txres               = True
  txres@txFontHeightF = 0.015

;
; Loop through each grouping of markers, and draw them one set at
; a time, assigning the proper color and size with gsn_marker.
;
; At the same time, draw a legend showing the meaning of the markers.
;
  ;xleg = (/-0.05,0.00,0.00,0.09,0.09,0.28,0.28,0.48,0.48,0.68,0.68,0.88,0.88/)   ; Location of
  ;xtxt = (/-0.14,0.09,0.00,0.18,0.18,0.37,0.37,0.57,0.57,0.78,0.78,0.98,0.98/)   ; legend markers
  ;yleg = (/-0.05,0.05,0.01,0.05,0.01,0.05,0.01,0.05,0.01,0.05,0.01,0.05,0.01/)   ; and text
  ;ytxt = (/-0.05,0.05,0.01,0.05,0.01,0.05,0.01,0.05,0.01,0.05,0.01,0.05,0.01/)   ; strings.

  xleg = (/ 0.09,0.09,0.28,0.28,0.48,0.48,0.68/)   ; Location of
  xtxt = (/ 0.18,0.18,0.37,0.37,0.57,0.57,0.78/)   ; legend markers
  yleg = (/ 0.05,0.01,0.05,0.01,0.05,0.01,0.05/)   ; and text
  ytxt = (/ 0.05,0.01,0.05,0.01,0.05,0.01,0.05/)   ; strings.

point = new(num_distinct_markers,graphic)

  do i = 0, num_distinct_markers-1
    if (.not.ismissing(lat_new(i,0)))
      gsres@gsMarkerColor      = colors(i)
      ;gsres@gsMarkerColor      = 1
      ;gsres@gsMarkerThicknessF = 0.7*(i+1)
      ;gsres@gsMarkerThicknessF = 2
      gsres@gsMarkerThicknessF = 1
      ;gsres@MarkerSizeF  = 0.4
      gsres@MarkerSizeF  = 0.05

      point(i) =  gsn_add_polymarker(wks,map,lon_new(i,:),lat_new(i,:),gsres)

; Add marker and text for the legend.

      gsn_polymarker_ndc(wks,          xleg(i),yleg(i),gsres)
      gsn_text_ndc      (wks,labels(i),xtxt(i),ytxt(i),txres)
    end if
   end do
  draw(map)

; =================================
;  Draw an array of vertical wind barbs over the above map.
;
  lat_w = new((/num_mt/),float)
  lon_w = new((/num_mt/),float)
  u_w   = new((/num_mt/),float)
  v_w   = new((/num_mt/),float)

  ;lat_w(0) = 40.95413
  ;lat_w(1) = 40.91255

  ; --- trick it not to plot
  lat_w(0) = -89.95413
  lat_w(1) = -89.91255

  lon_w(0) = -104.07798
  lon_w(1) = -103.89477

  u_w(:) =  5.
  ;u_w(:) =  0.
  v_w(:) =  0.

  wmsetp("col", 2)
  wmsetp("wbs", .1)
  wmbarbmap(wks, lat_w, lon_w,  u_w, v_w)  ; Plot barbs.

; -----------------------------------------

  frame(wks)   ; Now advance the frame.
  end
              
