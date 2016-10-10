;==============================================================================;
; 0. Define function
;==============================================================================;
  ;-------------------------------------------------------------
  ; 0.0 Load useful ncl scripts
  ;-------------------------------------------------------------
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRFUserARW.ncl"
 
  ;-------------------------------------------------------------
  function sub_string_double(s:string,i:integer,j:integer)
  begin
     s_char = stringtochar(s)

     sub_str = s_char(i:j)
     return (stringtodouble(chartostring(sub_str)))
  end

  ;-------------------------------------------------------------
  function sub_string_float(s:string,i:integer,j:integer)
  begin
     s_char = stringtochar(s)
     sub_str = s_char(i:j)
     return (stringtofloat(chartostring(sub_str)))
  end

  ;-------------------------------------------------------------
  function replace_missing(x:float)
  begin
     if (.not.ismissing(x)) then
         px = x
     else
         px = -999.
     end if
     px@_FillValue = -999.
     return(px)
  end

  ;-----------------------------------------------------------------------------
  ; 0.1 wrf_map_overlays
  ;-----------------------------------------------------------------------------  
  undef("wrf_map_overlays")
  function wrf_map_overlays(in_file[1]:file, \
                            wks:graphic, \
                            plots[*]:graphic, \
                            opt_arg[1]:logical, \
                            opt_mp[1]:logical) 
 
  ; This procedure takes an array of plots and overlays them on a
  ; base plot - map background.
  ;
  ; It will advance the plot and cleanup, unless you set the
  ; PanelPlot resource to True.
  ;
  ;  Attributes recognized by this procedure:
  ;     FramePlot
  ;     PanelPlot
  ;     NoTitles                  (don't do any titles) 
  ;     CommonTitle & PlotTile is used to overwrite field titles
  ;        CommonTitle will super-seed NoTitles
  ;
  ; If FramePlot False, then Draw the plot but do not Frame.
  ; In this case a user want to add to the drawing, and will
  ; have to advance the Frame manually in the script.
  ;
  ; If the "NoTitles" attribute exists and is set True, then
  ; don't create the top-left titles, and leave the main titles alone.
  ; This resource can be useful if you are planning to panel
  ; the plots.
  ;
  ; If PanelPlot is set to True, then this flags to wrf_map_overlays
  ; that these plots are going to be eventually paneled (likely
  ; by gsn_panel), and hence 1) draw and frame should not be called
  ; (unless gsnDraw and/or gsnFrame are explicitly set to True),
  ; and 2) the overlays and titles should not be removed with
  ; NhlRemoveOverlay and NhlRemoveAnnotation.
  ;
  begin

    ; Let's make the map first
    base = wrf_map(wks,in_file,opt_mp)

    opts = opt_arg      ; Make a copy of the resource list

    no_titles  = get_res_value(opts,"NoTitles",False)     ; Do we want field titles?
    com_title  = get_res_value(opts,"CommonTitle",False)     ; Do we have a common title?
    if ( com_title ) then
      plot_title = get_res_value(opts,"PlotTitle","  ")
      no_titles = True
    end if
  
    call_draw  = True
    call_frame = get_res_value(opts,"FramePlot",True)     ; Do we want to frame the plot?
    panel_plot = get_res_value(opts,"PanelPlot",False)    ; Are we paneling?
    opts@gsnMaximize = get_res_value_keep(opts,"gsnMaximize", True)

    nplots = dimsizes(plots)
  ;  font_color = "Black"

    do i=0,nplots-1
      if(.not.ismissing(plots(i))) then
  ;      class_name = NhlClassName(plots(i))
  ;      print(class_name)
  ;      if(class_name.eq."contourPlotClass") then
  ;        getvalues plots(i)
  ;          "cnFillOn"    : fill_on
  ;          "cnLineColor" : line_color
  ;        end getvalues
  ;        if (.not.fill_on) then
  ;          font_color = line_color      
  ;        end if       
  ;      end if
        if(.not.no_titles) then
          getvalues plots(i)
            "tiMainString" : SubTitle
          end getvalues
          if(i.eq.0) then
            SubTitles = SubTitle
          else
            SubTitles = SubTitles + "~C~" + SubTitle
          end if
        end if
        if(com_title .and. i .eq. nplots-1) then
          getvalues plots(i)
            "tiMainString" : SubTitle
          end getvalues
          SubTitles = plot_title
        end if
        setvalues plots(i)
          "tfDoNDCOverlay" : True
          "tiMainOn"       : False
        end setvalues
        overlay(base,plots(i))
      else
        print("wrf_map_overlays: Warning: overlay plot #" + i + " is not valid.")
      end if
    end do

    if(.not.no_titles .or. com_title) then
      font_height = get_res_value_keep(opts,"FontHeightF",0.01)
      txt = create "map_titles" textItemClass wks
        "txString"      : SubTitles
        "txFontHeightF" : font_height
       ;"txFontColor"   : font_color
      end create
      anno = NhlAddAnnotation(base,txt)
      setvalues anno
        "amZone"           : 3
        "amJust"           : "BottomLeft"
        "amSide"           : "Top"
        "amParallelPosF"   : 0.005
        "amOrthogonalPosF" : 0.03
        "amResizeNotify"   : False
      end setvalues
      base@map_titles = anno
    end if
  ;
  ; gsnDraw and gsnFrame default to False if panel plot.
  ;
    if(panel_plot) then
      call_draw = False
      call_frame= False
    end if


    opts@gsnDraw     = get_res_value_keep(opts,"gsnDraw",     call_draw)
    opts@gsnFrame    = get_res_value_keep(opts,"gsnFrame",    call_frame)

    draw_and_frame(wks,base,opts@gsnDraw,opts@gsnFrame,False, \
                   opts@gsnMaximize)

    if(.not.panel_plot) then
      do i=0,nplots-1
        if(.not.ismissing(plots(i))) then
          NhlRemoveOverlay(base,plots(i),False)
        else
          print("wrf_remove_map_overlays: Warning: overlay plot #" + i + " is not valid.")
          print("                        Nothing to remove.")
        end if
      end do
    end if
  
    if(.not.no_titles.and..not.panel_plot) then
      if(isatt(base,"map_titles")) then
        NhlRemoveAnnotation(base,base@map_titles)
        delete(base@map_titles)
      end if
    end if

  return(base)
  end

  ;-----------------------------------------------------------------------------
  ; 0.2 wrf_user_list_times
  ;-----------------------------------------------------------------------------  
  undef("wrf_user_list_times")
  function wrf_user_list_times( nc_file:file )

  local times, times_in_file, dims, i
  begin

     times_in_file = nc_file->Times
     dims = dimsizes(times_in_file)
     times = new(dims(0),string)
     do i=0,dims(0)-1
        times(i) = chartostring(times_in_file(i,:))
     end do
     times@description = "times in file"
     return(times)

  end
;==============================================================================;
begin

wcStrt     = systemfunc("date") 

;==============================================================================;
; 1. Define Variables
;==============================================================================;
  ;-----------------------------------------------------------------------------
  ; 1.1 Commad line
  ;-----------------------------------------------------------------------------  
  if (.not. isvar("file_in")) then
       print(" ")
       print(" You did not define the input file --> EXIT")
       exit
  end if

    if (.not. isvar("dom")) then
       print(" ")
       print(" You did not define the domain")
       filc = stringtochar(file_in)
       dom  = stringtointeger(chartostring(filc(8:9)))
       print(" You will use dom="+dom)
  end if

  ;-----------------------------------------------------------------------------
  ; 1.2 Parameters
  ;-----------------------------------------------------------------------------  
  dir      = "."
  type     = "ps"
  if (dom .eq. 4 .OR. dom .eq. 7 .OR.dom.eq.8) then
          lbpos    = 1.05
  else
	  lbpos    = 1.1
  end if

  ;-----------------------------------------------------------------------------
  ; 1.3 WRFoptions
  ;-----------------------------------------------------------------------------    
  res = True
  res@MainTitlePos                = "Center"
  res@InitTime                    = False
  res@ValidTime                   = False
  res@TimePos                     = "Right"
  res@Footer                      = False 
  res@mpOutlineBoundarySets       = "AllBoundaries"

;==============================================================================;
; 2. Load files
;==============================================================================;
  ;-----------------------------------------------------------------------------
  ; 2.1 Open the netCDF WRF-CHEM file
  ;-----------------------------------------------------------------------------    
  ;a1 = addfile(dir+"/"+file_in+".nc","r")
  a1 = addfile(dir+"/"+file_in,"r")

  ;-------------------------------------------------------------
  ; 1.2 Get zoom coordinates (if needed)
  ;-------------------------------------------------------------
   dims_all = dimsizes(a1->T2)
   nlat     = dims_all(1)
   nlon     = dims_all(2)
   if (nlat.le.0 .OR. nlon.le.0) then
       print("Problem: dim < 0 ==> EXIT")
       exit
   end if

   if (dom.gt.8) then
       if (dom.eq.9) then
           x_start = 57
           y_start = 15
           x_end   = 127
           y_end   = 94
           dom_name = "d05"
           nbcity   = 0
       end if
   else
       x_start = 0
       y_start = 0
       x_end   = nlat-1
       y_end   = nlon-1
       dom_name = sprinti("%0.2i",dom)
   end if

  LAT2D = a1->XLAT(0,x_start:x_end,y_start:y_end)
  LON2D = a1->XLONG(0,x_start:x_end,y_start:y_end)

  ;-----------------------------------------------------------------------------
  ; 2.3 Get the times to plot 
  ;-----------------------------------------------------------------------------    
  times  = wrf_user_list_times(a1)  
  ntimes = dimsizes(times)

;==============================================================================;
; 3. Plot the variables
;==============================================================================;
  time_units = "minutes since 2015-01-01 00:00:0.0"
  dow_a = (/"Sun","Mon","Tue","Wed","Thu","Fri","Sat"/)
  month_a = (/"","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"/)

  out_dir = "icing_plots"
  system ("mkdir "+out_dir)

  ;-----------------------------------------------------------------------------
  ; 3.1 Loop over each time
  ;-----------------------------------------------------------------------------   
  do it = 0,ntimes-1
     time = it
     time_c = stringtochar(times(it))
     ccyy   = chartostring(time_c(0:3))
     mm     = chartostring(time_c(5:6))
     dd     = chartostring(time_c(8:9))
     hh     = chartostring(time_c(11:12))

     year_wrf   = stringtoint(ccyy)
     month_wrf  = stringtoint(mm)
     day_wrf    = stringtoint(dd)
     hour_wrf   = stringtoint(hh)
     minute_wrf = hour_wrf*0
     second_wrf = 0
     time_wrf = ut_inv_calendar(year_wrf,month_wrf,day_wrf,hour_wrf,minute_wrf,second_wrf,time_units, 0)
 
     dow   = day_of_week(year_wrf,month_wrf,day_wrf)
     dow_c = dow_a(dow)
     month_c = month_a(month_wrf)
     year_c = sprinti("%0.2i",year_wrf-2000)
     time_utc = "Valid: "+hh+"00 UTC "+dow_c+" "+dd+" "+month_c+" "+year_c

     time_wrf_loc = time_wrf + (8*60)
     time_wrf_loc@units = time_units
     time_lab = floattoint(ut_calendar(time_wrf_loc,  0))
     dow2    =  day_of_week(time_lab(0,0),time_lab(0,1),time_lab(0,2))
     dow_loc = dow_a(dow2)
     month_loc = month_a(time_lab(0,1))
     year_loc  = sprinti("%0.2i",time_lab(0,0)-2000)
     time_MDT = sprinti("%0.2i",time_lab(0,3))+"00 CST "+dow_loc+" "+ \
                sprinti("%0.2i",time_lab(0,2))+" "+month_loc+" "+year_loc

     local_date = ""+ccyy+""+mm+""+dd+""+hh

     out_dirdate = out_dir +"/"+ local_date
     system ("mkdir "+out_dirdate)

  ;-----------------------------------------------------------------------------
  ; 3.2 Get WRF variables
  ;----------------------------------------------------------------------------- 
     u10_unzoom = wrf_user_getvar(a1,"U10",time)
     u10        = u10_unzoom(x_start:x_end,y_start:y_end)
     v10_unzoom = wrf_user_getvar(a1,"V10",time)
     v10        = v10_unzoom(x_start:x_end,y_start:y_end)

     SPD10 = sqrt(u10*u10+v10*v10)
     SPD10@description = "10m Horizontal wind"
     SPD10@units = "m/s"

     terrain_unzoom  = wrf_user_getvar(a1,"HGT",time)
     terrain         = terrain_unzoom(x_start:x_end,y_start:y_end)

     t2_unzoom = wrf_user_getvar(a1,"T2",time)
     t2        = t2_unzoom(x_start:x_end,y_start:y_end)

     td2_unzoom = wrf_user_getvar(a1,"td2",time)
     td2        = td2_unzoom(x_start:x_end,y_start:y_end)

     rh2_unzoom = wrf_user_getvar(a1,"rh2",time)
     rh2        = rh2_unzoom(x_start:x_end,y_start:y_end)

     if isfilevar(a1,"RAINRATE") then
	rr1h_unzoom = wrf_user_getvar(a1,"RAINRATE",time)
     else
     	rr1h_unzoom = wrf_user_getvar(a1,"RAINC",time) + wrf_user_getvar(a1,"RAINNC",time)
     end if
     rr1h       = rr1h_unzoom(x_start:x_end,y_start:y_end)


     delete(t2_unzoom)
     delete(td2_unzoom)
     delete(rh2_unzoom)
     delete(u10_unzoom)
     delete(v10_unzoom)
     delete(terrain_unzoom)     

  ;-----------------------------------------------------------------------------
  ; 3.3 Convert
  ;----------------------------------------------------------------------------- 
     u10_plane     = u10*1.94386
     v10_plane     = v10*1.94386
     t2_plane      = t2 -273.15

  ;-----------------------------------------------------------------------------
  ; 3.3 Loop over each level
  ;----------------------------------------------------------------------------- 
     graph_main_title   = "Domain "+ dom_name + "~C~"+time_utc+" ("+time_MDT+")"; Valid: "+hh+"00 UTC"; on "+ccyy+"/"+mm+"/"+dd
     
     res@TimeLabel                   = times(it)
     res@MainTitle                   = graph_main_title
     res@MainTitlePos                = "Center"

  ;-----------------------------------------------------------------------------
  ; 3.7 Define outputs
  ;-----------------------------------------------------------------------------

      file_out = out_dirdate+"/"+sprinti("d%i",dom)+ "_"+local_date+\
                 "_icing2d_t2_SPD10"
      wks = gsn_open_wks(type,file_out)
      gsn_define_colormap(wks,"t_9step")

      ;cmap = RGBtoCmap("./color_ybl_precip.txt")
      ;gsn_define_colormap(wks,cmap)
  ;gsn_merge_colormaps(wks,"t_8step","cmap")

      ;array = ispan(0,40,2)/2.
      array = ispan(-32,62,2)/2.
      nbval = dimsizes(array)

      array2 = (/0.001, 0.5,  1.0, 2.6,  5.1/)
      nbval2 = dimsizes(array2)

      ;color_tab = floattoint(fspan(2,101,nbval+1))
      color_tab = floattoint(fspan(2,49,nbval+1))
      color_tab2 = ispan(50,55,1);floattoint(fspan(50,58,nbval2+1))
      opts_var = res
      opts_var@FieldTitle         = "2-m Temperature"
      opts_var@UnitLabel          = "C"
      ;opts_var@ContourParameters  = (/ -10., 30., 0.5/)
      opts_var@cnLevelSelectionMode    = "ExplicitLevels"
      opts_var@cnLevels                = array
      opts_var@cnFillColors            = color_tab

      opts_var@cnFillOn           = True
      opts_var@cnFillMode = "RasterFill"
      opts_var@cnRasterSmoothingOn = "True"
      ;opts_var@gsnSpreadColors     = True
   opts_var@cnLinesOn       = False
   opts_var@cnLineLabelsOn  = False
   opts_var@cnInfoLabelOn   = False
   opts_var@lbTitlePosition = "Bottom"
   opts_var@gsnFrame        = False

      opts_var@lbPerimOn               = False
      opts_var@lbAutoManage          = False
      opts_var@lbOrientation           = "Horizontal"
      opts_var@lbTitlePosition         = "Bottom"
      opts_var@lbTitleFontHeightF      = 0.016
      opts_var@lbBoxLinesOn            = True
      opts_var@lbBoxLineThicknessF     = 0.6
      opts_var@pmLabelBarWidthF      = 0.12
      ;opts_var@pmLabelBarSide          = "Top"
      opts_var@pmLabelBarWidthF        = 0.70
      opts_var@pmLabelBarHeightF       = 0.2
      opts_var@pmLabelBarOrthogonalPosF = -0.1
      ;res@lbLabelFontHeightF     = .010
      ;res@lbLabelStride          = 5
      opts_var@lbLabelFontHeightF      = 0.012
   ;;   opts_var@lbTitleOffsetF          = -0.5

      ;   opts_var@gsnSpreadColors      = True
      ;   opts_var@gsnSpreadColorStart  = 40
      opts_var@lbLabelAutoStride    = True

      ;opts_var@cnFillMode = "RasterFill"
      ;opts_var@cnRasterSmoothingOn = "True"
      ;opts_var@lbLabelStride           = 4

  ;-----------------------------------------------------------------------------
  ; 3.7 Wind Vectors resources
  ;-----------------------------------------------------------------------------
      opts_vct = res
      opts_vct@NumVectors        = 25
      opts_vct@vcWindBarbLineThicknessF  = 2.
      ;  opts_vct@vcRefMagnitudeF         = 10.0
      ;  opts_vct@vcRefLengthF            = 0.045
      ;  opts_vct@vcMinDistanceF          = 0.017
      ;;opts_vct@vcGlyphStyl       = "LineArrow"
      ;opts_vct@vcLineArrowColor   ="red"

       ;;opts_vct@vcRefAnnoOrthogonalPosF  = .1          ; move ref vector down
      opts_vct@vcVectorDrawOrder  = "PostDraw"
      ;opts_vct@gsnDraw                  = False              ; don't draw yet
      ;opts_vct@gsnFrame                 = False              ; don't advance frame yet
      opts_vct@vcRefAnnoBackgroundColor = 0
      opts_vct@vcRefAnnoOn             = True
      opts_vct@vcRefAnnoOrthogonalPosF = -1.17          ; move ref vector
      opts_vct@vcRefAnnoPerimOn  = False
      opts_vct@vcRefAnnoParallelPosF   = 1.            ; move ref vector
      opts_vct@FieldTitle         = "Horizontal Wind"+ " at 10 m (AGL)"
      opts_vct@vcMinFracLengthF   = 0
      opts_vct@vcRefMagnitudeF    = 10.0
      opts_vct@vcRefLengthF       = 0.05
      opts_vct@vcGlyphStyle       = "CurlyVector"
      opts_vct@UnitLabel          = "m/s"
      opts_vct@vcMinFracLengthF   = 0


  ;-----------------------------------------------------------------------------
  ; 3.7 Map resources
  ;-----------------------------------------------------------------------------
      pltres = True
      ;pltres@gsnDraw  = False
      pltres@gsnFrame = False

      mpres = True
  mpres@mpDataBaseVersion = "MediumRes"
  ;mpres@mpDataBaseVersion = "HighRes"
  mpres@mpOutlineSpecifiers = (/"China:states"/)
  mpres@mpOutlineBoundarySets = "National"
  mpres@mpDataSetName = "Earth..4"
  mpres@mpGeophysicalLineColor = "Black";"DarkBrown"     ; color of continental outlines
  mpres@mpNationalLineColor     = "Black";"DarkBrown"
  mpres@mpUSStateLineColor     = "Black";"DarkBrown"
  mpres@mpGridLineDashPattern  = 2         ; lat/lon lines as dashed


;      mpres@mpGeophysicalLineColor      = "White";"Black"
;      mpres@mpNationalLineColor         = "White";"Black"
;      mpres@mpUSStateLineColor          = "White";"Black"
      mpres@mpGridLineColor             = "Black"
      mpres@mpLimbLineColor             = "Black"
      mpres@mpPerimLineColor            = "Black"
      mpres@mpGeophysicalLineThicknessF = 2.5
      mpres@mpGridLineThicknessF        = 2.0
      mpres@mpLimbLineThicknessF        = 2.0
      mpres@mpNationalLineThicknessF    = 2.5
      mpres@mpUSStateLineThicknessF     = 2.5
      mpres@ZoomIn = True
      mpres@Xstart = y_start
      mpres@Ystart = x_start
      mpres@Xend   = y_end
      mpres@Yend   = x_end
      ; mpres@gsnDraw  = False
      mpres@gsnFrame = False
      ; mpres@MainTitle                   = graph_main_title
      ; mpres@MainTitlePos                = "Center"
      ; mpres@tiMainFontThicknessF        = 0.8
      ; mpres@tiMainFontHeightF           = 0.018
      ; mpres@tiMainString                = graph_main_title
;      mpres@mpOutlineDrawOrder            = "Predraw"
  ;-----------------------------------------------------------------------------
  ; 3.7 Plot
  ;-----------------------------------------------------------------------------
      contour_var  = wrf_contour(a1,wks,t2_plane,opts_var)
      vector       = wrf_vector(a1,wks,u10, v10,opts_vct)
      plot = wrf_map_overlays(a1,wks,(/contour_var,vector/),pltres,mpres)

  QCFile_sfc = "icing_combined_"+ccyy+"-"+mm+"-"+dd+"_"+hh+":00:00.nc"
  if (isfilepresent(QCFile_sfc).AND.dom.ge.1) then
      print ("File "+QCFile_sfc+"is present and processed")
      ff = addfile(QCFile_sfc,"r")
      latitude = doubletofloat(ff->ylat)
      longitude = doubletofloat(ff->xlon)
      longitude = where(longitude.gt.180.,longitude- 360.,longitude)

      date_obs   = ff->date
      time_obs   = ut_inv_calendar(stringtoint(chartostring(date_obs(:,0:3))),   \
                                   stringtoint(chartostring(date_obs(:,5:6))),   \
                                   stringtoint(chartostring(date_obs(:,8:9))),   \
                                   stringtoint(chartostring(date_obs(:,11:12))),   \
                                   stringtoint(chartostring(date_obs(:,14:15))), \
                                   stringtoint(chartostring(date_obs(:,17:18))), time_units, 0)
      delete(date_obs)

      lat_ndc=new(dimsizes(latitude), float)
      lon_ndc=new(dimsizes(longitude), float)
      datatondc(plot,longitude,latitude,lon_ndc,lat_ndc)
      indt = ind((time_obs.ge.time_wrf-10 .AND. time_obs.lt.time_wrf+10))
      if (all(ismissing(indt))) then
          print("No stations to plot --> EXIT")
      end if

      indx = ind(( lat_ndc.gt.0.  .AND.  lat_ndc.lt.1.).AND. \
                 ( lon_ndc.gt.0.  .AND.  lon_ndc.lt.1.))

      nrect = dimsizes(.not.ismissing(indt))	
      nrec = dimsizes(.not.ismissing(indx))
      print ("Stations numbers: "+nrec)

      if (nrec.eq.0) then
          print("No stations to plot --> EXIT")
      end if

      icingrate = ff->INSIGRmod
      icingrate = where(icingrate.lt.-887,icingrate@_FillValue,icingrate)
	
      mres = True
      mres@gsMarkerIndex     = 16      ; open circle
      mres@gsMarkerSizeF     = 0.004    ; twice normal size
      vardata = icingrate(indt,indx)
      marker  = new(nrec,"graphic")
      marker2 = new(nrec,"graphic")
      lons = longitude(indx)
      lats = latitude(indx)
      lats_ndc = lat_ndc(indx)
      lons_ndc = lon_ndc(indx)

      dmax2 = 0.00004;mres@gsMarkerSizeF
      removed_list = new(nrec,integer)
      rcount = 0
      ind_all = ispan(0,nrec-1,1)
      do ibox1 = 0,nrec-1
         dist2 = sqrt( (lats_ndc(ibox1)-lats_ndc)^2 + \
                             (lons_ndc(ibox1)-lons_ndc)^2 )
         dist2@_FillValue = -999
         dist2(ibox1) = dist2@_FillValue
         if (.not.all(ismissing(removed_list))) then
             ind_r = ind(.not.ismissing(removed_list))
             dist2(removed_list(:dimsizes(ind_r)-1)) = dist2@_FillValue
             delete(ind_r)
         end if
         if (min(dist2).lt.dmax2) then
             removed_list(rcount) = ibox1
             rcount = rcount + 1
         end if
         delete(dist2)
      end do

      bres = True
      bres@gsMarkerIndex     =  4       ; open circle
      bres@gsMarkerSizeF     = 0.004
      bres@gsMarkerColor     = "Black"
      bres@gsMarkerThicknessF = 1.5

          do perc_i=0, nbval2
             if (perc_i.eq.0.OR.perc_i.eq.nbval2) then
                 if (perc_i.eq.0) then
                     ind_color = ind(vardata.lt.array2(perc_i))
                 else
                     ind_color = ind(vardata.gt.array2(perc_i-1))
                 end if
             else
                 ind_color = ind(vardata.ge.array2(perc_i-1).AND. \
                            vardata.lt.array2(perc_i))
             end if
             if (.not.all(ismissing(ind_color))) then
             do nn = 0, dimsizes(ind_color)-1
                 mres@gsMarkerColor     = color_tab2(perc_i)
		 if (rcount.eq.0) then
                     indnot = -999.
		     indnot@_FillValue = -999.
		 else
                     indnot = ind(ind_color(nn).eq.removed_list(:rcount-1))
		 end if
                 if ((ismissing(indnot))) then
                     marker(ind_color(nn)) = gsn_add_polymarker(wks,plot,lons(ind_color(nn)),\
                                                     lats(ind_color(nn)),mres)
                     ;marker2(ind_color(nn)) = gsn_add_polymarker(wks,plot,lons(ind_color(nn)),\
                     ;                                lats(ind_color(nn)),bres)
                 end if
                 delete(indnot)
             end do
             end if
             delete(ind_color)
          end do

      end if

  ;-------------------------------------------------------------
  ; 4.5 plot a label bar
  ;-------------------------------------------------------------
     nB              = nbval2
     nboxes          = nB+1
     fill_colors     = color_tab2
     lblabels        = new(nB+1, string)
     do n=0,nB-1
        if (n.eq.0) then
            lblabels(n)    = ("<"+array2(n))
        else
            lblabels(n)    = ("["+array2(n-1)+","+array2(n)+")")
        end if
     end do
     lblabels(nB) = ">"+array2(nB-1)

     lbres                       = True
     lbres@lbAutoManage          = True;False       ; we control label bar
     lbres@lbFillColors          = fill_colors ; use nice strided colors
     lbres@lbPerimOn             = False       ; Turn off labelbar perimeter.
     lbres@lbMonoFillPattern     = True        ; one pattern, default then solid
     lbres@vpWidthF              = 0.06                 ; size
     lbres@vpHeightF             = 0.3
     ;albres@lbLabelFontHeightF    = 0.03
     lbres@lbOrientation         = "vertical"
     lbres@lbTitleString         = "Icing Rate ~C~  (mm/h)"
 ;    lbres@lbTitleFontHeightF    = 0.024

     lbres@lbPerimOn               = False
     lbres@lbTitlePosition         = "Top"
      lbres@lbBoxLinesOn            = True
      lbres@lbBoxLineThicknessF     = 0.6
      ;opts_var@pmLabelBarSide          = "Top"
      lbres@pmLabelBarWidthF        = 0.2
    ;  lbres@pmLabelBarHeightF       = 0.07
     ; lbres@pmLabelBarOrthogonalPosF = -0.1
      ;res@lbLabelFontHeightF     = .010
      ;res@lbLabelStride          = 5
      lbres@lbLabelFontHeightF      = 0.02
      ;lbres@lbTitleOffsetF          = -0.5


;     lbid =gsn_create_labelbar_ndc(wks,nboxes,lblabels,0.,0.,lbres)
     lbid =gsn_create_labelbar(wks,nboxes,lblabels,lbres)

     annoid = NhlAddAnnotation(plot,lbid)

  ;-------------------------------------------------------------
  ; 4.6 Add the labelbar on the plot
  ;-------------------------------------------------------------
    setvalues annoid
        "amZone"           : 2
        "amParallelPosF"   : lbpos
        "amOrthogonalPosF" : -0.9
        ;"amResizeNotify"   : True
     end setvalues


      draw(plot)
      frame(wks)

  delete(wks)
  delete(plot)
  system("convert  -trim -density 120 "+file_out+".ps"+" "+file_out+".png")

  ;-----------------------------------------------------------------------------
  ; 3.9 Clean
  ;----------------------------------------------------------------------------- 
      ; Delete options and fields, so we don't have carry over
      print("")
      print("End of the plots")
      print("The plot file is "+file_out+"."+type)  
      print("")


      file_out = out_dirdate+"/"+sprinti("d%i",dom)+  "_"+local_date+\
                 "_icing2d_RH2_SPD10"
      wks = gsn_open_wks(type,file_out)
      cmaprh = (/"(/1.00, 1.00, 1.00/)", "(/0.00, 0.00, 0.00/)", "(/1.00, 1.00, 1.00/)", \
           "(/.62, .62, .96/)", "(/.76, .76, .98/)", "(/0.86, 0.86, 1.00/)", "(/.51, .82, 1.00/)",\
           "(/.58, .96, .96/)", "(/.73, .96, .73/)", "(/0.55, 0.92, 0.55/)", "(/.36, .82, .36/)", "(/.05, .92, .05/)", \
           "(/1.00, 1.00, 1.00/)","(/0.486275,0.996078,0.988235/)","(/0.988235,0.870588,0.0156863/)","(/0.988235,0.52549,0.0156863/)", \
           "(/0.831373,0.00784314,0.203922/)","(/0.45098,0.00784314,0.392157/)"/)

      gsn_define_colormap(wks,cmaprh)
      delete(array)
      delete(color_tab)
      delete(opts_var@cnLevels)
      delete(opts_var@cnFillColors)
      array = ispan(10,90,10)
      nbval = dimsizes(array)

      array2 = (/0.001, 0.5,  1.0, 2.6,  5.1/)
      nbval2 = dimsizes(array2)

      color_tab = ispan(2,11,1);
      color_tab2 = ispan(12,17,1);

      opts_var@FieldTitle         = "2-m Relative Humidity"
      opts_var@UnitLabel          = "%"
      opts_var@cnLevelSelectionMode    = "ExplicitLevels"
      opts_var@cnLevels                = array
      opts_var@cnFillColors            = color_tab

      contour_var  = wrf_contour(a1,wks,rh2,opts_var)
      vector       = wrf_vector(a1,wks,u10, v10,opts_vct)
      plot = wrf_map_overlays(a1,wks,(/contour_var,vector/),pltres,mpres)

  if (isfilepresent(QCFile_sfc).AND.dom.ge.1) then
      marker  = new(nrec,"graphic")
      marker2 = new(nrec,"graphic")
          do perc_i=0, nbval2
             if (perc_i.eq.0.OR.perc_i.eq.nbval2) then
                 if (perc_i.eq.0) then
                     ind_color = ind(vardata.lt.array2(perc_i))
                 else
                     ind_color = ind(vardata.gt.array2(perc_i-1))
                 end if
             else
                 ind_color = ind(vardata.ge.array2(perc_i-1).AND. \
                            vardata.lt.array2(perc_i))
             end if
             if (.not.all(ismissing(ind_color))) then
             do nn = 0, dimsizes(ind_color)-1
                 mres@gsMarkerColor     = color_tab2(perc_i)
                 if (rcount.eq.0) then
                     indnot = -999.
                     indnot@_FillValue = -999.
                 else
                     indnot = ind(ind_color(nn).eq.removed_list(:rcount-1))
                 end if
                 if ((ismissing(indnot))) then
                     marker(ind_color(nn)) = gsn_add_polymarker(wks,plot,lons(ind_color(nn)),\
                                                     lats(ind_color(nn)),mres)
                     ;marker2(ind_color(nn)) = gsn_add_polymarker(wks,plot,lons(ind_color(nn)),\
                     ;                                lats(ind_color(nn)),bres)
                 end if
                 delete(indnot)
             end do
             end if
             delete(ind_color)
          end do

      end if

     fill_colors     = color_tab2
     lbres@lbFillColors          = fill_colors
     lbid =gsn_create_labelbar(wks,nboxes,lblabels,lbres)
     annoid = NhlAddAnnotation(plot,lbid)

  ;-------------------------------------------------------------
  ; 4.6 Add the labelbar on the plot
  ;-------------------------------------------------------------
    setvalues annoid
        "amZone"           : 2
        "amParallelPosF"   : lbpos
        "amOrthogonalPosF" : -0.9
        ;"amResizeNotify"   : True
     end setvalues


      draw(plot)
      frame(wks)

  delete(wks)
  delete(plot)
  system("convert  -trim -density 120 "+file_out+".ps"+" "+file_out+".png")

  ;-----------------------------------------------------------------------------
  ; 3.9 Clean
  ;----------------------------------------------------------------------------- 
      ; Delete options and fields, so we don't have carry over
      print("")
      print("End of the plots")
      print("The plot file is "+file_out+"."+type)
      print("")




     swd_unzoom = wrf_user_getvar(a1,"SWDOWN",time)
     swd        = swd_unzoom(x_start:x_end,y_start:y_end)
     delete(swd_unzoom)
     file_out = out_dirdate+"/"+sprinti("d%i",dom)+ "_"+local_date+ \
                 "_icing2d_SWDOWN_SPD10"
     wks = gsn_open_wks(type,file_out)

     cmapsw = (/"(/1.00, 1.00, 1.00/)","(/0.00, 0.00, 0.00/)","(/0.25,0.25,0.91/)","(/0.38,0.38,1/)","(/0.51,0.51,1/)","(/0.62,0.62,0.97/)", \
                  "(/0.76,0.76,0.98/)","(/0,0.57,0/)","(/0,0.72,0/)","(/0.36,0.82,0.36/)","(/0.55,0.92,0.55/)", \
                  "(/0.73,0.96,0.73/)","(/0.74,0,0.51/)","(/0.85,0.36,0.64/)","(/0.94,0.54,0.76/)","(/0.96,0.68,0.84/)", \
                  "(/0.97,0.8,0.9/)","(/0.63,0.59,0/)","(/0.78,0.73,0.17/)","(/0.89,0.82,0/)","(/0.99,0.93,0.5/)", \
                  "(/0.99,0.95,0.66/)","(/0.47,0.47,0.47/)","(/0.57,0.57,0.57/)","(/0.67,0.67,0.67/)","(/0.78,0.78,0.78/)", \
                  "(/0.87,0.87,0.87/)", "(/1.00, 1.00, 1.00/)","(/0.486275,0.996078,0.988235/)","(/0.988235,0.870588,0.0156863/)","(/0.988235,0.52549,0.0156863/)", \
                  "(/0.831373,0.00784314,0.203922/)","(/0.45098,0.00784314,0.392157/)"/)

      gsn_define_colormap(wks,cmapsw)
      delete(array)
      delete(color_tab)
      delete(opts_var@cnLevels)
      delete(opts_var@cnFillColors)

      ;array = ispan(0,40,2)/2.
      array = ispan(50,1200,50)
      nbval = dimsizes(array)
      array2 = (/0.001, 0.5,  1.0, 2.6,  5.1/)
      nbval2 = dimsizes(array2)
      color_tab = ispan(2,26,1)
      color_tab2 = ispan(27,32,1);floattoint(fspan(50,58,nbval2+1))

      opts_var@FieldTitle         = "Downward short wave flux at ground surface"
      opts_var@UnitLabel          = "W/m2"
      opts_var@cnLevelSelectionMode    = "ExplicitLevels"
      opts_var@cnLevels                = array
      opts_var@cnFillColors            = color_tab

      opts = res
      opts@FieldTitle         = "2-m temperature"
      opts@UnitLabel          = "deg C"
      opts@cnLineColor = "Blue"
      opts@cnFillOn           = False
      opts@cnLinesOn       = True
      opts@cnLineLabelsOn  = True
      ;opts@cnHighLabelsOn = True
      ;opts@cnLowLabelsOn = True
      opts@ContourParameters = (/ -30., 30., 5. /)
      opts@cnLineLabelBackgroundColor = -1
      opts@gsnContourLineThicknessesScale = 2.0

      contour_var  = wrf_contour(a1,wks,swd,opts_var)
      wrf_smooth_2d( t2_plane, 3 )
      contour_t    = wrf_contour(a1,wks,t2_plane,opts)
      delete(opts)
      vector       = wrf_vector(a1,wks,u10, v10,opts_vct)
      plot = wrf_map_overlays(a1,wks,(/contour_var,contour_t,vector/),pltres,mpres)

  if (isfilepresent(QCFile_sfc).AND.dom.ge.1) then
      marker  = new(nrec,"graphic")
      marker2 = new(nrec,"graphic")
          do perc_i=0, nbval2
             if (perc_i.eq.0.OR.perc_i.eq.nbval2) then
                 if (perc_i.eq.0) then
                     ind_color = ind(vardata.lt.array2(perc_i))
                 else
                     ind_color = ind(vardata.gt.array2(perc_i-1))
                 end if
             else
                 ind_color = ind(vardata.ge.array2(perc_i-1).AND. \
                            vardata.lt.array2(perc_i))
             end if
             if (.not.all(ismissing(ind_color))) then
             do nn = 0, dimsizes(ind_color)-1
                 mres@gsMarkerColor     = color_tab2(perc_i)
                 if (rcount.eq.0) then
                     indnot = -999.
                     indnot@_FillValue = -999.
                 else
                     indnot = ind(ind_color(nn).eq.removed_list(:rcount-1))
                 end if
                 if ((ismissing(indnot))) then
                     marker(ind_color(nn)) = gsn_add_polymarker(wks,plot,lons(ind_color(nn)),\
                                                     lats(ind_color(nn)),mres)
                     ;marker2(ind_color(nn)) = gsn_add_polymarker(wks,plot,lons(ind_color(nn)),\
                     ;                                lats(ind_color(nn)),bres)
                 end if
                 delete(indnot)
             end do
             end if
             delete(ind_color)
          end do

      end if

     fill_colors     = color_tab2
     lbres@lbFillColors          = fill_colors
     lbid =gsn_create_labelbar(wks,nboxes,lblabels,lbres)
     annoid = NhlAddAnnotation(plot,lbid)
  ;-------------------------------------------------------------
  ; 4.6 Add the labelbar on the plot
  ;-------------------------------------------------------------
    setvalues annoid
        "amZone"           : 2
        "amParallelPosF"   : lbpos
        "amOrthogonalPosF" : -0.9
        ;"amResizeNotify"   : True
     end setvalues


      draw(plot)
      frame(wks)

  delete(wks)
  delete(plot)
  system("convert  -trim -density 120 "+file_out+".ps"+" "+file_out+".png")


      file_out = out_dirdate+"/"+sprinti("d%i",dom)+ "_"+local_date+ \
                 "_icing2d_SPD10_stream"
      wks = gsn_open_wks(type,file_out)
      gsn_define_colormap(wks,"3saw_9")

      delete(array)
      delete(color_tab)
      delete(opts_var@cnLevels)
      delete(opts_var@cnFillColors)

      range_level = ispan(0,60,1)/2.
      array = range_level
      color_level = new(dimsizes(range_level)+1,integer)
      color_level(0:6)  = floattoint(fspan(8,24,7))
      color_level(7:28) = floattoint(fspan(156,206,22))
      color_level(29:50) = floattoint(fspan(240,255,22))
      color_level(51:) = floattoint(fspan(121,131,11))
      color_tab = color_level
      nbval = dimsizes(array)

      array2 = (/0.001, 0.5,  1.0, 2.6,  5.1/)
      nbval2 = dimsizes(array2)
      color_tab2 = ispan(27,32,1);floattoint(fspan(,256,nbval2+1))

      opts_var@FieldTitle         = SPD10@description
      opts_var@UnitLabel          = SPD10@units
      opts_var@cnLevelSelectionMode    = "ExplicitLevels"
      opts_var@cnLevels                = array
      opts_var@cnFillColors            = color_tab

      delete(opts_vct)
      opts_vct = res
      opts_vct@vcWindBarbLineThicknessF  = 2.
      opts_vct@vcVectorDrawOrder  = "PostDraw"
      opts_vct@vcRefAnnoOn             = False
      opts_vct@FieldTitle         = "";"Horizontal Wind"+ " at 10 m (AGL)"
      opts_vct@gsnLeftString         = "";"Horizontal Wind"+ " at 10 m (AGL)"
      opts_vct@gsnRightString          = "";m/s"

    ;  opts_vct@stArrowLengthF     = 0.006
    ;  opts_vct@stMinArrowSpacingF = 0.01
    ;  opts_vct@stArrowStride      = 6
  opts_vct@stLineThicknessF   = 1.
  opts_vct@stLineColor        = "black"
  opts_vct@stMinDistanceF = 0.015
  opts_vct@stMinLineSpacingF = 0.0015
      opts_vct@stLineStartStride = 1
      opts_vct@gsnFrame = False
      opts_vct@gsnDraw  = False
      opts_vct@vcMinFracLengthF   = 0

  contour_var  = wrf_contour(a1,wks,SPD10,opts_var)
  vector       = gsn_csm_streamline(wks,u10, v10,opts_vct)
  plot = wrf_map_overlays(a1,wks,(/contour_var,vector/),pltres,mpres)


  if (isfilepresent(QCFile_sfc).AND.dom.ge.1) then
      marker  = new(nrec,"graphic")
      marker2 = new(nrec,"graphic")
          do perc_i=0, nbval2
             if (perc_i.eq.0.OR.perc_i.eq.nbval2) then
                 if (perc_i.eq.0) then
                     ind_color = ind(vardata.lt.array2(perc_i))
                 else
                     ind_color = ind(vardata.gt.array2(perc_i-1))
                 end if
             else
                 ind_color = ind(vardata.ge.array2(perc_i-1).AND. \
                            vardata.lt.array2(perc_i))
             end if
             if (.not.all(ismissing(ind_color))) then
             do nn = 0, dimsizes(ind_color)-1
                 mres@gsMarkerColor     = color_tab2(perc_i)
                 if (rcount.eq.0) then
                     indnot = -999.
                     indnot@_FillValue = -999.
                 else
                     indnot = ind(ind_color(nn).eq.removed_list(:rcount-1))
                 end if
                 if ((ismissing(indnot))) then
                     marker(ind_color(nn)) = gsn_add_polymarker(wks,plot,lons(ind_color(nn)),\
                                                     lats(ind_color(nn)),mres)
                     ;marker2(ind_color(nn)) = gsn_add_polymarker(wks,plot,lons(ind_color(nn)),\
                     ;                                lats(ind_color(nn)),bres)
                 end if
                 delete(indnot)
             end do
             end if
             delete(ind_color)
          end do

      end if

     fill_colors     = color_tab2
     lbres@lbFillColors          = fill_colors
     lbid =gsn_create_labelbar(wks,nboxes,lblabels,lbres)
     annoid = NhlAddAnnotation(plot,lbid)
  ;-------------------------------------------------------------
  ; 4.6 Add the labelbar on the plot
  ;-------------------------------------------------------------
    setvalues annoid
        "amZone"           : 2
        "amParallelPosF"   : lbpos
        "amOrthogonalPosF" : -0.9
        ;"amResizeNotify"   : True
     end setvalues


      draw(plot)
      frame(wks)

  delete(wks)
  delete(plot)
  system("convert  -trim -density 120 "+file_out+".ps"+" "+file_out+".png")


      file_out = out_dirdate+"/"+sprinti("d%i",dom)+ "_"+local_date+ \
                 "_icing2d_RR1H_stream"
      wks = gsn_open_wks(type,file_out)
      cmaprr = (/"(/1.00, 1.00, 1.00/)", "(/0.00, 0.00, 0.00/)", "(/1.00, 1.00, 1.00/)", \
           "(/.62, .62, .97/)", "(/.51, .51, 1.00/)", "(/0.25, 0.25, 0.91/)", "(/.86, 1.00, 0.86/)",\
           "(/0.36,0.82,0.36/)", "(/0.00,0.41,0.00/)", "(/0.98,0.89,0.94/)", "(/0.94,0.54,0.76/)", "(/0.74,0.00,0.51/)", \
           "(/.58, .96, .96/)", "(/.73, .96, .73/)", "(/0.55, 0.92, 0.55/)", "(/.36, .82, .36/)", "(/.05, .92, .05/)", \
           "(/1.00, 1.00, 1.00/)","(/0.486275,0.996078,0.988235/)","(/0.988235,0.870588,0.0156863/)","(/0.988235,0.52549,0.0156863/)", \
           "(/0.831373,0.00784314,0.203922/)","(/0.45098,0.00784314,0.392157/)"/)

      gsn_define_colormap(wks,cmaprr)

      delete(array)
      delete(color_tab)
      delete(opts_var@cnLevels)
      delete(opts_var@cnFillColors)

      array = (/0.1,0.2,0.4,0.8,1.6,3.2,6.4,12.8,25.6/)
      color_tab = ispan(2,16,1);

      nbval = dimsizes(array)

      array2 = (/0.001, 0.5,  1.0, 2.6,  5.1/)
      nbval2 = dimsizes(array2)
      color_tab2 = ispan(17,22,1);floattoint(fspan(,256,nbval2+1))

      opts_var@FieldTitle         = "1h RAIN"
      opts_var@UnitLabel          = "mm"
      ;opts_var@ContourParameters  = (/ -10., 30., 0.5/)
      opts_var@cnLevelSelectionMode    = "ExplicitLevels"
      opts_var@cnLevels                = array
      opts_var@cnFillColors            = color_tab
      opts_var@lbLabelStride           = 1

      contour_var  = wrf_contour(a1,wks,rr1h,opts_var)
      vector       = gsn_csm_streamline(wks,u10, v10,opts_vct)
      plot = wrf_map_overlays(a1,wks,(/contour_var/),pltres,mpres)

  if (isfilepresent(QCFile_sfc).AND.dom.ge.1) then
      marker  = new(nrec,"graphic")
      marker2 = new(nrec,"graphic")
          do perc_i=0, nbval2
             if (perc_i.eq.0.OR.perc_i.eq.nbval2) then
                 if (perc_i.eq.0) then
                     ind_color = ind(vardata.lt.array2(perc_i))
                 else
                     ind_color = ind(vardata.gt.array2(perc_i-1))
                 end if
             else
                 ind_color = ind(vardata.ge.array2(perc_i-1).AND. \
                            vardata.lt.array2(perc_i))
             end if
             if (.not.all(ismissing(ind_color))) then
             do nn = 0, dimsizes(ind_color)-1
                 mres@gsMarkerColor     = color_tab2(perc_i)
                 if (rcount.eq.0) then
                     indnot = -999.
                     indnot@_FillValue = -999.
                 else
                     indnot = ind(ind_color(nn).eq.removed_list(:rcount-1))
                 end if
                 if ((ismissing(indnot))) then
                     marker(ind_color(nn)) = gsn_add_polymarker(wks,plot,lons(ind_color(nn)),\
                                                     lats(ind_color(nn)),mres)
                     ;marker2(ind_color(nn)) = gsn_add_polymarker(wks,plot,lons(ind_color(nn)),\
                     ;                                lats(ind_color(nn)),bres)
                 end if
                 delete(indnot)
             end do
             end if
             delete(ind_color)
          end do

      end if

     fill_colors     = color_tab2
     lbres@lbFillColors          = fill_colors
     lbid =gsn_create_labelbar(wks,nboxes,lblabels,lbres)
     annoid = NhlAddAnnotation(plot,lbid)

  ;-------------------------------------------------------------
  ; 4.6 Add the labelbar on the plot
  ;-------------------------------------------------------------
    setvalues annoid
        "amZone"           : 2
        "amParallelPosF"   : lbpos
        "amOrthogonalPosF" : -0.9
        ;"amResizeNotify"   : True
     end setvalues


      draw(plot)
      frame(wks)

  delete(wks)
  delete(plot)
  system("convert  -trim -density 120 "+file_out+".ps"+" "+file_out+".png")



  if (isfilevar(a1, "HGT")) then
  file_out = out_dirdate+"/"+sprinti("d%i",dom)+ "_"+local_date+ \
                 "_icing2d_2drate"
  ztower = 80 
  z_unzoom = wrf_user_getvar(a1,"z",time)
  z        = z_unzoom(:,x_start:x_end,y_start:y_end)
  z!0      = "bottom_top"
  z!1      = "south_north"
  z!2      = "west_east"

  terrain_unzoom  = wrf_user_getvar(a1,"HGT",time)
  terrain         = terrain_unzoom(x_start:x_end,y_start:y_end)

     u_unzoom = wrf_user_getvar(a1,"ua",time)
     u        = u_unzoom(:,x_start:x_end,y_start:y_end)
     v_unzoom = wrf_user_getvar(a1,"va",time)
     v        = v_unzoom(:,x_start:x_end,y_start:y_end)
     u!0      = "bottom_top"
     u!1      = "south_north"
     u!2      = "west_east"
     v!0      = "bottom_top"
     v!1      = "south_north"
     v!2      = "west_east"
  
     delete(u_unzoom)
     delete(v_unzoom)
     delete(z_unzoom)
     delete(terrain_unzoom)

     tc_unzoom      = wrf_user_getvar(a1,"tc",time)
     tc             = tc_unzoom(:,x_start:x_end,y_start:y_end)
     delete(tc_unzoom)

     pres_unzoom      = wrf_user_getvar(a1,"pres",time)
     pres             = pres_unzoom(:,x_start:x_end,y_start:y_end)
     delete(pres_unzoom)

     qrain_unzoom      = wrf_user_getvar(a1,"QRAIN",time)
     qrain             = qrain_unzoom(:,x_start:x_end,y_start:y_end)
     delete(qrain_unzoom)

     qcloud_unzoom      = wrf_user_getvar(a1,"QCLOUD",time)
     qcloud             = qcloud_unzoom(:,x_start:x_end,y_start:y_end)
     delete(qcloud_unzoom)

     level = new((/1,x_end-x_start+1,y_end-y_start+1/),float)
     level!0      = "height"
     level!1      = "south_north"
     level!2      = "west_east"

     level(0,:,:) = terrain+ztower

     qliq = qcloud + qrain
     qliq!0      = "bottom_top"
     qliq!1      = "south_north"
     qliq!2      = "west_east"

     density_air = pres/(287*(tc+273.16))
     density_air!0      = "bottom_top"
     density_air!1      = "south_north"
     density_air!2      = "west_east"

     delete(pres)

     linlog =1

     z_plane      = int2p(z(south_north|:, west_east|:, bottom_top|:), \
                          z(south_north|:, west_east|:, bottom_top|:), \
                          level(south_north|:, west_east|:, height|:), linlog)

     tc_plane     = int2p(z(south_north|:, west_east|:, bottom_top|:), \
                          tc(south_north|:, west_east|:, bottom_top|:), \
                          level(south_north|:, west_east|:, height|:), linlog)

     qliq_plane     = int2p(z(south_north|:, west_east|:, bottom_top|:), \
                          qliq(south_north|:, west_east|:, bottom_top|:), \
                          level(south_north|:, west_east|:, height|:), linlog)

     density_air_plane     = int2p(z(south_north|:, west_east|:, bottom_top|:), \
                          density_air(south_north|:, west_east|:, bottom_top|:), \
                          level(south_north|:, west_east|:, height|:), linlog)
     delete(density_air)

     u_plane   = int2p(z(south_north|:, west_east|:, bottom_top|:), \
                       u(south_north|:, west_east|:, bottom_top|:), \
                       level(south_north|:, west_east|:, height|:), linlog)
     v_plane   = int2p(z(south_north|:, west_east|:, bottom_top|:), \
                       v(south_north|:, west_east|:, bottom_top|:), \
                       level(south_north|:, west_east|:, height|:), linlog)

     speed_a_plane = sqrt(u_plane*u_plane + v_plane*v_plane)
     delete(u_plane)
     delete(v_plane)
  ; ======
  ice_thick = terrain
  ice_thick@description = "icing growth rate"
  ice_thick@units       = "mm/h"
  ice_thick = 0
  ; =====
  ; parameters 
  MVD = 18e-6    ; mean volume diameter (m)
  rho_w = 1000   ; water density (kg/m**3)
  rho_i = 900    ; ice density (kg/m**3)
  d_c = 6.2e-3   ; cylinder diameter (m)
  freezing_fraction = 1

  Rtmp0 = 518.7

  ; ==========
  hgt = 0

  ;iz = 5
  Rtmp = (tc_plane(:,:,0) + 273.15)*(9./5.)
  mu_a = ( (3.62e-7)*4.44822/(0.3048000^2) ) * (Rtmp/Rtmp0)^1.5 * (Rtmp0+198.72)/(Rtmp0+198.72)
  St = (speed_a_plane(:,:,0)* MVD^2 * rho_w)/(9* mu_a* d_c)
  E  = St^2 /(St+0.7)^2
  ice_thick = 3600*1000* E*qliq_plane(:,:,0)*density_air_plane(:,:,0)*speed_a_plane(:,:,0)*freezing_fraction/rho_i
  ice_thick = where(tc_plane(:,:,0).gt.0.,0,ice_thick)

      wks = gsn_open_wks(type,file_out)
      cmaprh3 = (/"(/1.00, 1.00, 1.00/)", "(/0.00, 0.00, 0.00/)", "(/1.00, 1.00, 1.00/)","(/0.486275,0.996078,0.988235/)","(/0.988235,0.870588,0.0156863/)","(/0.988235,0.52549,0.0156863/)", \
           "(/0.831373,0.00784314,0.203922/)","(/0.45098,0.00784314,0.392157/)"/)
      gsn_define_colormap(wks,cmaprh3)

      delete(array)
      delete(color_tab)
      delete(opts_var@cnLevels)
      delete(opts_var@cnFillColors)
    
      array = (/0.001, 0.5,  1.0, 2.6,  5.1/)
      nbval = dimsizes(array)
      color_tab = ispan(2,7,1);floattoint(fspan(,256,nbval2+1))

      opts_var@FieldTitle         = ice_thick@description
      opts_var@UnitLabel          = ice_thick@units
      ;opts_var@ContourParameters  = (/ -10., 30., 0.5/)
      opts_var@cnLevelSelectionMode    = "ExplicitLevels"
      opts_var@cnLevels                = array
      opts_var@cnFillColors            = color_tab
      opts_var@lbLabelStride           = 1

      contour_var  = wrf_contour(a1,wks,ice_thick,opts_var)
 ;     vector       = gsn_csm_streamline(wks,u10, v10,opts_vct)
      plot = wrf_map_overlays(a1,wks,(/contour_var/),pltres,mpres)

      draw(plot)
      frame(wks)

  delete(wks)
  delete(plot)
  system("convert  -trim -density 120 "+file_out+".ps"+" "+file_out+".png")

  end if

  ;-----------------------------------------------------------------------------
  ; 3.11 End loop over time
  ;-----------------------------------------------------------------------------    
 end do

;==============================================================================;
; 4. End of Script
;==============================================================================;    
  ;-----------------------------------------------------------------------------
  ; 4.1 Delete everything
  ;-----------------------------------------------------------------------------  
  delete(a1)

  ;-----------------------------------------------------------------------------
  ; 4.2 Get the time used to process the file
  ;-----------------------------------------------------------------------------    
  wallClockElapseTime(wcStrt, "Plotting  "+file_out, 0)
end
