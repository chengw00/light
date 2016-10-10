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
  ;type@wkBackgroundOpacityF = 0.0 
  if (dom .eq. 4 .OR. dom .eq. 7 .OR.dom.eq.8) then
          lbpos    = 1.1
	  lbxp     = -1.
	  lby      = 0.4
  else
	  lbxp     = -1.08
          lbpos    = 1.1
	  lby      = 0.6
  end if

  if (dom .le.3) then
      vectdis = 0.2
  else
      vectdis = 0.02
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
                 "_trans_icingINST"
      wks = gsn_open_wks(type,file_out)
     cmaprh = (/"(/1.00, 1.00, 1.00/)", "(/0.00, 0.00, 0.00/)", "(/0.90, 0.90, 0.90/)", \
           "(/0.486275,0.996078,0.988235/)","(/0.988235,0.870588,0.0156863/)","(/0.988235,0.52549,0.0156863/)", \
           "(/0.831373,0.00784314,0.203922/)","(/0.45098,0.00784314,0.392157/)"/)

      gsn_define_colormap(wks,cmaprh)

      array2 = (/0.001, 0.5,  1.0, 2.6,  5.1/)
      nbval2 = dimsizes(array2)
      color_tab2 = ispan(2,7,1);

      mpres = res
  mpres@mpDataBaseVersion = "MediumRes"
  ;mpres@mpDataBaseVersion = "HighRes"
  ;mpres@mpOutlineSpecifiers = (/"China:states"/)
  mpres@mpOutlineBoundarySets = "National"
  mpres@mpDataSetName = "Earth..4"
  mpres@mpGeophysicalLineColor = "Black";"DarkBrown"     ; color of continental outlines
  mpres@mpNationalLineColor     = "Black";"DarkBrown"
  mpres@mpUSStateLineColor     = "Black";"DarkBrown"
  mpres@mpGridLineDashPattern  = 2         ; lat/lon lines as dashed
mpres@mpGridAndLimbOn             =False

mpres@gsnMaximize = False
mpres@gsnPaperOrientation = "portrait"
;mpres@wkBackgroundOpacityF = 0.

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
       mpres@MainTitle                   = graph_main_title
       mpres@MainTitlePos                = "Center"
       mpres@tiMainFontThicknessF        = 0.8
       mpres@tiMainFontHeightF           = 0.018
       mpres@tiMainString                = graph_main_title
;      mpres@mpOutlineDrawOrder            = "Predraw"
        mpres@tiMainString                = "Power Line Icing Rate (mm/h) on "+graph_main_title

  ;-----------------------------------------------------------------------------
  ; 3.7 Plot
  ;-----------------------------------------------------------------------------
;      contour_var  = wrf_contour(a1,wks,t2_plane,opts_var)
;      vector       = wrf_vector(a1,wks,u10, v10,opts_vct)
;      plot = wrf_map_overlays(a1,wks,(/contour_var,vector/),pltres,mpres)
      plot = wrf_map(wks,a1,mpres)


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
     lbres@vpWidthF              = 0.07                 ; size
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
        "amParallelPosF"   : 1.1
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


      file_out = out_dirdate+"/"+sprinti("d%i",dom)+"_"+local_date+ \
                 "_trans_icingACC"

      wks = gsn_open_wks(type,file_out)
      cmaprh = (/"(/1.00, 1.00, 1.00/)", "(/0.00, 0.00, 0.00/)", "(/0.90, 0.90, 0.90/)", \
           "(/0.486275,0.996078,0.988235/)","(/0.988235,0.870588,0.0156863/)","(/0.988235,0.52549,0.0156863/)", \
           "(/0.831373,0.00784314,0.203922/)","(/0.45098,0.00784314,0.392157/)"/)

      gsn_define_colormap(wks,cmaprh)
      mpres@tiMainString                = "Power Line Icing Accumulation (mm) on "+graph_main_title

      plot = wrf_map(wks,a1,mpres)

  if (isfilepresent(QCFile_sfc).AND.dom.ge.1) then

      icingacc = ff->ACCIGRmod
      icingacc = where(icingacc.lt.-887,icingacc@_FillValue,icingacc)

      vardata = icingacc(indt,indx)
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

  ;-------------------------------------------------------------
  ; 4.5 plot a label bar
  ;-------------------------------------------------------------
     lbres@lbFillColors          = fill_colors ; use nice strided colors
     lbres@lbTitleString         = "Total Icing Acc~C~  (mm)"
     lbid =gsn_create_labelbar(wks,nboxes,lblabels,lbres)

     annoid = NhlAddAnnotation(plot,lbid)

  ;-------------------------------------------------------------
  ; 4.6 Add the labelbar on the plot
  ;-------------------------------------------------------------
    setvalues annoid
        "amZone"           : 2
        "amParallelPosF"   : 1.1
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

      file_out = out_dirdate+"/"+sprinti("d%i",dom)+ "_"+local_date+\
                 "_trans_lineT"

      wks = gsn_open_wks(type,file_out)
      gsn_define_colormap(wks,"t_8step")
      delete(array2)
      delete(color_tab2)
      array2 = ispan(-32,62,2)/2.
      nbval2 = dimsizes(array2)
      color_tab2 = floattoint(fspan(2,49,nbval2+1))

        mpres@tiMainString                = "Power Line Temperature (deg C) on "+graph_main_title
	;mpres@gsnLeftString = "Temperature"
        ;mpres@gsnRightString = "deg C"
        ;mpres@FieldTitle         = "Temperature"
        ;mpres@UnitLabel          = "deg C"

      plot = wrf_map(wks,a1,mpres)

  if (isfilepresent(QCFile_sfc).AND.dom.ge.1) then

      t = ff->Tmod
      t = where(t.lt.-887,t@_FillValue,t)

      vardata = t(indt,indx)
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

  ;-------------------------------------------------------------
  ; 4.5 plot a label bar
  ;-------------------------------------------------------------
     nB              = nbval2
     nboxes          = nB+1
     delete(fill_colors)
     delete(lbres)
     delete(lblabels)
     fill_colors     = color_tab2
     lblabels        = new(nB+1, string)
     lblabels(:nB-1) = array2
   ;  do n=0,nB-1
   ;     if (n.eq.0) then
   ;         lblabels(n)    = ("<"+array2(n))
   ;     else
   ;         lblabels(n)    = ("["+array2(n-1)+","+array2(n)+")")
   ;     end if
   ;  end do
     lblabels(nB) = ">"+array2(nB-1)
     lblabels(0)    = ("<"+array2(0))

     lbres                       = True
     lbres@lbAutoManage          = True;False       ; we control label bar
     lbres@lbFillColors          = fill_colors ; use nice strided colors
     lbres@lbPerimOn             = False       ; Turn off labelbar perimeter.
     lbres@lbMonoFillPattern     = True        ; one pattern, default then solid
     lbres@vpWidthF              = 0.07                 ; size
     lbres@vpHeightF             = lby
     ;albres@lbLabelFontHeightF    = 0.03
     lbres@lbOrientation         = "vertical"
     lbres@lbTitleString         = "Temp (C)"
 ;    lbres@lbTitleFontHeightF    = 0.024

     lbres@lbPerimOn               = False
     lbres@lbTitlePosition         = "Top"
     lbres@lbTitleOn = False
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
        "amOrthogonalPosF" : lbxp
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

      file_out = out_dirdate+"/"+sprinti("d%i",dom)+ "_"+local_date+\
                 "_trans_lineRH"

      wks = gsn_open_wks(type,file_out)
      cmaprh2 = (/"(/1.00, 1.00, 1.00/)", "(/0.00, 0.00, 0.00/)", "(/0.90, 0.90, 0.90/)", \
           "(/.62, .62, .96/)", "(/.76, .76, .98/)", "(/0.86, 0.86, 1.00/)", "(/.51, .82, 1.00/)",\
           "(/.58, .96, .96/)", "(/.73, .96, .73/)", "(/0.55, 0.92, 0.55/)", "(/.36, .82, .36/)", "(/.05, .92, .05/)"/)

      gsn_define_colormap(wks,cmaprh2)

      delete(array2)
      delete(color_tab2)
      array2 = ispan(10,90,10)
      nbval2 = dimsizes(array2)
      color_tab2 = ispan(2,11,1);

      mpres@tiMainString                = "Power Line Relative Humidity (%) on "+graph_main_title

      plot = wrf_map(wks,a1,mpres)

  if (isfilepresent(QCFile_sfc).AND.dom.ge.1) then

      rh = ff->RHmod
      rh = where(rh.lt.-887,rh@_FillValue,rh)

      vardata = rh(indt,indx)
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

  ;-------------------------------------------------------------
  ; 4.5 plot a label bar
  ;-------------------------------------------------------------
     nB              = nbval2
     nboxes          = nB+1
     delete(fill_colors)
     delete(lbres)
     delete(lblabels)
     fill_colors     = color_tab2
     lblabels        = new(nB+1, string)
     lblabels(:nB-1) = array2
   ;  do n=0,nB-1
   ;     if (n.eq.0) then
   ;         lblabels(n)    = ("<"+array2(n))
   ;     else
   ;         lblabels(n)    = ("["+array2(n-1)+","+array2(n)+")")
   ;     end if
   ;  end do
     lblabels(nB) = ">"+array2(nB-1)
     lblabels(0)    = ("<"+array2(0))

     lbres                       = True
     lbres@lbAutoManage          = True;False       ; we control label bar
     lbres@lbFillColors          = fill_colors ; use nice strided colors
     lbres@lbPerimOn             = False       ; Turn off labelbar perimeter.
     lbres@lbMonoFillPattern     = True        ; one pattern, default then solid
     lbres@vpWidthF              = 0.07                 ; size
     lbres@vpHeightF             = lby
     ;albres@lbLabelFontHeightF    = 0.03
     lbres@lbOrientation         = "vertical"
     lbres@lbTitleString         = "RH (%)"
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
        "amOrthogonalPosF" : lbxp
        ;"amResizeNotify"   : True
     end setvalues


      draw(plot)
      frame(wks)

  delete(wks)
  delete(plot)
  system("convert  -trim -density 120 "+file_out+".ps"+" "+file_out+".png")

      print("")
      print("End of the plots")
      print("The plot file is "+file_out+"."+type)
      print("")

      file_out = out_dirdate+"/"+sprinti("d%i",dom)+ "_"+local_date+\
                 "_trans_lineSPD"

      wks = gsn_open_wks(type,file_out)
      gsn_define_colormap(wks,"3saw")

      delete(array2)
      delete(color_tab2)

      range_level = ispan(0,60,1)/2.
      array2 = range_level
      color_level = new(dimsizes(range_level)+1,integer)
      color_level(0:6)  = floattoint(fspan(8,24,7))
      color_level(7:28) = floattoint(fspan(156,206,22))
      color_level(29:50) = floattoint(fspan(240,255,22))
      color_level(51:) = floattoint(fspan(121,131,11))
      color_tab2 = color_level
      nbval2 = dimsizes(array2)

      mpres@tiMainString                = "Power Line Wind Speed (m/s) on "+graph_main_title

      plot = wrf_map(wks,a1,mpres)

  if (isfilepresent(QCFile_sfc).AND.dom.ge.1) then

      spd = ff->SPDmod
      spd = where(spd.lt.-887,spd@_FillValue,spd)

      vardata = spd(indt,indx)
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


  ;-------------------------------------------------------------
  ; 4.5 plot a label bar
  ;-------------------------------------------------------------
     nB              = nbval2
     nboxes          = nB+1
     delete(fill_colors)
     delete(lbres)
     delete(lblabels)
     fill_colors     = color_tab2
     lblabels        = new(nB+1, string)
     lblabels(:nB-1) = array2
   ;  do n=0,nB-1
   ;     if (n.eq.0) then
   ;         lblabels(n)    = ("<"+array2(n))
   ;     else
   ;         lblabels(n)    = ("["+array2(n-1)+","+array2(n)+")")
   ;     end if
   ;  end do
     lblabels(nB) = ">"+array2(nB-1)
     lblabels(0)    = ("<"+array2(0))

     lbres                       = True
     lbres@lbAutoManage          = True;False       ; we control label bar
     lbres@lbFillColors          = fill_colors ; use nice strided colors
     lbres@lbPerimOn             = False       ; Turn off labelbar perimeter.
     lbres@lbMonoFillPattern     = True        ; one pattern, default then solid
     lbres@vpWidthF              = 0.07                 ; size
     lbres@vpHeightF             = lby
     ;albres@lbLabelFontHeightF    = 0.03
     lbres@lbOrientation         = "vertical"
     lbres@lbTitleString         = "SPD (m/s)"
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
        "amOrthogonalPosF" : lbxp
        ;"amResizeNotify"   : True
     end setvalues


      draw(plot)
      frame(wks)

  delete(wks)
  delete(plot)
  system("convert  -trim -density 120 "+file_out+".ps"+" "+file_out+".png")

      print("")
      print("End of the plots")
      print("The plot file is "+file_out+"."+type)
      print("")



      file_out = out_dirdate+"/"+sprinti("d%i",dom)+ "_"+local_date+\
                 "_trans_lineVect"

      wks = gsn_open_wks(type,file_out)
      gsn_define_colormap(wks,"3saw")

      delete(array2)
      delete(color_tab2)

      range_level = ispan(0,60,1)/2.
      array2 = range_level
      color_level = new(dimsizes(range_level)+1,integer)
      color_level(0:6)  = floattoint(fspan(8,24,7))
      color_level(7:28) = floattoint(fspan(156,206,22))
      color_level(29:50) = floattoint(fspan(240,255,22))
      color_level(51:) = floattoint(fspan(121,131,11))
      color_tab2 = color_level
      nbval2 = dimsizes(array2)

      mpres@tiMainString                = "Power Line Wind Speed (m/s) on "+graph_main_title

      plot = wrf_map(wks,a1,mpres)

   if (isfilepresent(QCFile_sfc).AND.dom.ge.1) then

      spd = ff->SPDmod
      spd = where(spd.lt.-887,spd@_FillValue,spd)
      wspd  = 1.9438445*spd(indt,:)

      wdir  = ff->DIRmod(indt,:)
      wdir  = where(wdir.lt.-887,wdir@_FillValue,wdir)

      pmres               = True
      pmres@gsMarkerColor = "Red"
      pmres@gsMarkerIndex = 16
      pmres@gsMarkerThicknessF = 2.
      ;pmres@gsMarkerSizeF  = 0.001
      pmres@gsMarkerSizeF  = 0.0005

      if (nrec.gt.1) then
         ; markers = gsn_add_polymarker(wks, plot, longitude(indx), latitude(indx), pmres)
      end if
      if (nrec.gt.1) then
          sm   = new(nrec,graphic)
          anno = new(nrec,graphic)
          dmax = vectdis

          m = 0
            do n=0,nrec-1
                 far = True
                 dist = 0.
                 kk = 1
                 k = indx(n)
                 if (n.gt.0) then
                    delete(dist)
                    delete(kk)

                    dist = new(n,"float")
                    kk   = indx(0:n-1)
                    dist = sqrt( (latitude(kk)-latitude(k))^2 + \
                               (longitude(kk)-longitude(k))^2 )
                    if (min(dist) .lt. dmax) then
                        far = False
                    end if
                 end if

                 fmt = "%8.2f"
                 miss = -999.

                 if (far) then
                    ws = replace_missing(wspd(k))
                    ws@_FillValue = miss
                    wd = replace_missing(wdir(k))
                    wd@_FillValue = miss

                    rad = 0.01745329
                    Uo  = -ws*sin(wd*rad)
                    Uo@_FillValue = miss
                    Vo  = -ws*cos(wd*rad)
                    Vo@_FillValue = miss

                    wmsetp("vrs - size of reference vector in user space", 10.00)
                    wmsetp("vrn - size of reference vector on plot in NDC space", 0.05)
                    wmsetp("vcw - linewidth scale",2.)
                    wmsetp("vcc - vector color",255)
                    wmsetp("vch",0.006)
                    ;wmsetp("col", 240)    ; the vector color
                    ;          wmsetp("wbs", .03)  ; the vector arrow size
                    ;          wmsetp("vcw", 100.)   ; Increase the linewidth
                    ;wmsetp("vrn - size of reference vector on plot in NDC space", 0.08)

                    wmvectmap(wks, latitude(k), longitude(k), Uo, Vo)

                    m = m + 1
                 else
                    latitude(k) = latitude@_FillValue
                    longitude(k) = longitude@_FillValue
                 end if

           end do
        end if


   end if
      draw(plot)
      frame(wks)

  delete(wks)
  delete(plot)
  system("convert  -trim -density 120 "+file_out+".ps"+" "+file_out+".png")

      print("")
      print("End of the plots")
      print("The plot file is "+file_out+"."+type)
      print("")


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
