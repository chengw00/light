	integer, parameter :: nx_max = 600, ny_max = 600, nz_max = 65

        integer, parameter :: ntimes = 48

        integer, parameter :: nstn_max = 2500

        real, parameter :: undef = -999.,    &
                           ztower = 50.

        ! =============
        real, dimension(nx_max,ny_max,nz_max) :: p_grid,    &
                                                 z_grid,    &
                                                tc_grid,    &
                                                 u_grid,    &
                                                 v_grid,    &
                                                rh_grid,    &
                                                rh_map,     &
                                                ql_grid,    &
                                               qct_grid

        real, dimension(nx_max,ny_max) :: xlat_grid,        &
                                          xlon_grid,        &
                                          hgt_grid,         &
                                          swdown_grid

        real, dimension(nx_max,ny_max) :: ice_thick_grid,  &
                                          ice_grow,        & 
	                                  ice_melt_sub

        real, dimension(nx_max,ny_max,nz_max) :: wspd_grid, &
                                                 wdir_grid

        
        ! =============
        real, dimension(2)  :: uvgrid

        ! ===== station variables =====
        real, dimension(nstn_max) :: xlat_stn,        &
                                     xlon_stn

        real, dimension(nstn_max) :: tmp_curr_stn,    &
                                     den_curr_stn,    &
                                     rh_curr_stn,     &
                                     ws_curr_stn,     &
                                     wd_curr_stn,     &
                                     ql_curr_stn,     &
                                    qct_curr_stn,     &
                                     sw_curr_stn,     &
                                 insigr_curr_stn,     &
                                 accsigr_curr_stn 

        ! ====== station time series ========
        real, dimension(nstn_max,ntimes) :: tmp_timeser_stn, &
                                            accsigr_timeser_stn

        character (len=300), dimension(ntimes) :: file_timeser

        integer, dimension(ntimes) :: iflag_timeser
 
        ! ===================
        real plm1(2), plm2(2), plm3(2), plm4(2)  ! NCAR graphics
                                                 ! parameters

        ! ====== icing calculation parameters ======
        integer :: np_search

        real :: MVD, rho_w, rho_i, rho_a, d_c,    &
                freezing_fraction, alpha

        real :: Rtmp0, Rtmp, mu_a, St, E

        ! ==========
        np_search = 5
        alpha = 7.

        MVD = 18e-6    ! mean volume diameter (m)
        rho_w = 1000.  ! water density (kg/m**3)
        rho_i = 900.   ! ice density (kg/m**3)
        d_c = 6.2e-3   ! cylinder diameter (m)
        freezing_fraction = 1.

        Rtmp0 = 518.7

        ! ======= read file name and flags =======
        tmp_timeser_stn = undef
        accsigr_timeser_stn = undef

        do ip1=1,ntimes
         read(*,*) file_timeser(ip1)
         read(*,*) iflag_timeser(ip1)
        enddo

        ! ===============
        nstn = 0

        open(unit=10, file='line_latlon_all_use.txt', status='old')
         do ii=1,nstn_max
          read(10,*,end=100) xlat_stn(ii), xlon_stn(ii)
          nstn = nstn + 1
         enddo
100     close(10)

        write(*,*) 'nstn = ', nstn
        ! ===============
        open(unit=10, file='maproj.dat', status='old')
        read(10,*) map_proj_wrf
        close(10)

        open(unit=10, file='truelat1.dat', status='old')
        read(10,*) truelat1
        close(10)

        open(unit=10, file='truelat2.dat', status='old')
        read(10,*) truelat2
        close(10)

        open(unit=10, file='stand_lon.dat', status='old')
        read(10,*) stand_lon
        close(10)

        open(unit=10, file='wrf_grid_dim.dat', status='old')
        read(10,*) nz
        read(10,*) ny
        read(10,*) nx
        close(10)

        ! ======== set map projection parameters =====
        if (map_proj_wrf.eq.1) then
         jprj = 3  ! 1=Stereographic, 3=Lambert Conformal Conic
         tlat1 = truelat1
         tlat2 = truelat2
         clon  = stand_lon

         if (tlat1.eq.tlat2) tlat2 = tlat2 + 0.00001

         pii = 3.14159265
         radians_per_degree = pii/180.

         cone = 10**(cos(tlat1*radians_per_degree))  &
               -10**(cos(tlat2*radians_per_degree))
         cone = cone/(10**(tan(45. -abs(tlat1/2.)*radians_per_degree)) -  &
           10**(tan(45. -abs(tlat2/2.)*radians_per_degree))   )

        else
         write(*,*) 'map projection ', map_proj_wrf, ' not supported: STOP'
         stop
        endif

        plat = truelat1

        plon = stand_lon

        rota = truelat2

        ! ========== read 2d fields =========
        nrec = 0
        open(unit=10, file='wrfout_2d.gdat', access='direct', &
             form='unformatted', recl=4*nx*ny, status='old')
        nrec = nrec + 1
        read(10,rec=nrec) xlat_grid(1:nx,1:ny)
        nrec = nrec + 1
        read(10,rec=nrec) xlon_grid(1:nx,1:ny)
        nrec = nrec + 1
        read(10,rec=nrec) hgt_grid(1:nx,1:ny)
        nrec = nrec + 1
        read(10,rec=nrec) swdown_grid(1:nx,1:ny)
        close(10)

        ! ========== read 3d fields =========
        nrec = 0
        open(unit=10, file='wrfout_3d.gdat', access='direct', &
             form='unformatted', recl=4*nx*ny*nz, status='old')
        nrec = nrec + 1
        read(10,rec=nrec) p_grid(1:nx,1:ny,1:nz)
        nrec = nrec + 1
        read(10,rec=nrec) z_grid(1:nx,1:ny,1:nz)
        nrec = nrec + 1
        read(10,rec=nrec) tc_grid(1:nx,1:ny,1:nz)
        nrec = nrec + 1
        read(10,rec=nrec) u_grid(1:nx,1:ny,1:nz)
        nrec = nrec + 1
        read(10,rec=nrec) v_grid(1:nx,1:ny,1:nz)
        nrec = nrec + 1
        read(10,rec=nrec) rh_grid(1:nx,1:ny,1:nz)
        nrec = nrec + 1
        read(10,rec=nrec) ql_grid(1:nx,1:ny,1:nz)
        nrec = nrec + 1
        read(10,rec=nrec) qct_grid(1:nx,1:ny,1:nz)
        close(10)

        ! ------ wind speed ----------
        wspd_grid(1:nx,1:ny,1:nz) = sqrt(u_grid(1:nx,1:ny,1:nz)**2 + v_grid(1:nx,1:ny,1:nz)**2)

        ! ------- wind direction --------
        !wdir_grid

        if (jprj.eq.3) then
         do k=1,nz
          do j=1,ny
           do i=1,nx
            call dcomputeuvgrid(u_grid(i,j,k),           &
                                v_grid(i,j,k),           &
                                uvgrid,                  &
              xlat_grid(i,j),xlon_grid(i,j),             &
              clon,cone,radians_per_degree,1,1,1,1,1)
            u_grid(i,j,k) = uvgrid(1)
            v_grid(i,j,k) = uvgrid(2)
           enddo
          enddo
         enddo
        endif

        ! ------ find level of tower ---------
        i = 1
        j = 1
        k_tower = 3
        !z_diff = 1000000.
        !do k=1,nz
        ! if (abs(z_grid(i,j,k)-hgt_grid(i,j)).lt.z_diff) then
        !  z_diff = abs(z_grid(i,j,k)-hgt_grid(i,j))
        !  k_tower = k
        ! endif
        !enddo

        write(*,*) 'k_tower = ', k_tower

        ! ====== instantaneous icing rate ==========
        ice_thick_grid = 0.

        do j=1,ny
         do i=1,nx

          rho_a = 100*p_grid(i,j,k_tower)/ &
                      (287.*(tc_grid(i,j,k_tower)+273.16))
  
         !get rhmap
          if (rh_grid(i,j,k_tower).le.70.) then   
           rhmap(i,j,k_tower) = 0.0
          else if ((rh_grid(i,j,k_tower).gt.70.).and.(rh_grid(i,j,k_tower).le.80.)) then
           rhmap(i,j,k_tower) = ((rh_grid(i,j,k_tower)-70.)*0.025)
          else if ((rh_grid(i,j,k_tower).gt.80.).and.(rh_grid(i,j,k_tower).le.90.)) then
           rhmap(i,j,k_tower) = (0.25+(rh_grid(i,j,k_tower)-80.)*0.075)
          else 
           rhmap(i,j,k_tower) = 1.0
          end if 
          !get rhmap 
           
          !if these condition are met, then ice grow 
          if ( (tc_grid(i,j,k_tower).le.0.).and.    &
               (ql_grid(i,j,k_tower).gt.0.).and.    &
               (wspd_grid(i,j,k_tower).ne.0.) ) then
            ice_grow(i,j) = 3600*                                &
                             area_cyl*ql_grid(i,j,k_tower)*      &
                             rho_a*wspd_grid(i,j,k_tower)      !ice mass 
            ice_thick_grid(i,j)=ice_thick_grid(i,j) +  &
               1000*ice_grow(i,j)/(area_cyl*pii*rho_i)         !ice thickness in mm
            if (ice_thick_grid(i,j).lt.0.) ice_thick_grid(i,j) = 0.
          endif

          ! melting 
          if (tc_grid(i,j,k_tower).ge.0.) then
           if ( (tc_grid(i,j,k_tower).ge.0.).and.      &
                (tc_grid(i,j,k_tower).le.5.)) then
            ice_melt_sub(i,j) = 3600*(10./5.)*(tc_grid(i,j,k_tower))
           else
            ice_melt_sub(i,j) = 3600*10.
           endif
           ice_thick_grid(i,j)=ice_thick_grid(i,j)-1000*ice_melt_sub(i,j)/  &
                               (area_cyl*pii*rho_i)
           if (ice_thick_grid(i,j).lt.0.) ice_thick_grid(i,j) = 0.
          endif
 
          ! sublimation 
          if (tc_grid(i,j,k_tower).lt.0.) then
           ice_melt_sub(i,j) =                                  &
              0.2*((0.65*min(1.0,wspd_grid(i,j,k_tower)/10.))+  &
              0.35*(1.0-rhmap(i,j,k_tower)))
           ice_thick_grid(i,j)=ice_thick_grid(i,j)-1000*ice_melt_sub(i,j)/  &
                               (area_cyl*pii*rho_i)
           if (ice_thick_grid(i,j).lt.0.) ice_thick_grid(i,j) = 0.
          endif
            
         enddo
        enddo

        ! =========== get station time series ===========
        do ip1=1,ntimes

         if (real(iflag_timeser(ip1)).gt.0.5) then
          open(unit=10, file=file_timeser(ip1)(1:len_trim(file_timeser(ip1))), status='old')
          do ii=1,nstn
           read(10,*) dum_xlat_stn, &
                      dum_xlon_stn, &
                      tmp_timeser_stn(ii,ip1), &
                      dum_den_curr_stn,        &
                      dum_rh_curr_stn,         &
                      dum_ws_curr_stn,         &
                      dum_wd_curr_stn,         &
                      dum_ql_curr_stn,         &
                      dum_qct_curr_stn,        &
                      dum_sw_curr_stn,         &
                      dum_insigr_curr_stn,     & 
                      accsigr_timeser_stn(ii,ip1) 
          enddo

          close(10)
         endif
        enddo

        ! ------ do NCAR graphics thing to map grid ----------- !

        jlts = 5
        jgrd = 10
        iout = 4
        idot = 1

        plm1(1)=xlat_grid(1,1)
        plm1(2)=xlon_grid(1,1)

        plm2(1)=xlat_grid(nx,1)
        plm2(2)=xlon_grid(nx,1)

        plm3(1)=xlat_grid(nx,ny)
        plm3(2)=xlon_grid(nx,ny)

        plm4(1)=xlat_grid(1,ny)
        plm4(2)=xlon_grid(1,ny)

        call opngks

        call supmap(jprj,plat,plon,rota,plm1,plm2,     &
               plm3,plm4,jlts,jgrd,iout,idot,ierr)

        x0=xlat_grid(1,1)
        y0=xlon_grid(1,1)
        call maptrn(x0,y0,u0,v0)
        call point(u0,v0)            ! southwest corner as the origin

        x2=xlat_grid(nx,1)
        y2=xlon_grid(nx,1)
        call maptrn(x2,y2,u2,v2)
        call point(u2,v2)             ! southeast corner of WRF grid

        x3=xlat_grid(nx,ny)
        y3=xlon_grid(nx,ny)
        call maptrn(x3,y3,u3,v3)      ! northeast corner of WRF grid
        call point(u3,v3)

        ulen_wrf=u2-u0
        vlen_wrf=v3-v0

        write(*,*) 'ulen, vlen=', ulen_wrf, vlen_wrf

        ! ==============
        ! find coordinate of points 
        do ii=1,nstn
         call maptrn(xlat_stn(ii),xlon_stn(ii),u_temp,v_temp)
         bx=(u_temp-u0)*real(nx-1)/ulen_wrf+1    ! grid index of interpolated
         by=(v_temp-v0)*real(ny-1)/vlen_wrf+1    ! locations in WRF grid
         i_stn=nint(bx)
         j_stn=nint(by)
        
         if ( (i_stn.ge.1).and.(i_stn.le.nx).and.        &
              (j_stn.ge.1).and.(j_stn.le.ny) ) then
          tmp_curr_stn(ii) = tc_grid(i_stn,j_stn,k_tower)
          den_curr_stn(ii) = 100*p_grid(i_stn,j_stn,k_tower)/ &
                      (287.*(tc_grid(i_stn,j_stn,k_tower)+273.16))
          rh_curr_stn(ii) = rh_grid(i_stn,j_stn,k_tower)
          ws_curr_stn(ii) = sqrt(u_grid(i_stn,j_stn,k_tower)**2 + v_grid(i_stn,j_stn,k_tower)**2)
          call winddir(u_grid(i_stn,j_stn,k_tower),v_grid(i_stn,j_stn,k_tower),wd_curr_stn(ii),1,1,1)
          ql_curr_stn(ii) = ql_grid(i_stn,j_stn,k_tower)
          qct_curr_stn(ii) = qct_grid(i_stn,j_stn,k_tower)
          sw_curr_stn(ii) = swdown_grid(i_stn,j_stn)

          limx1 = -np_search + i_stn
          limx2 = i_stn + np_search

          limy1 = -np_search + j_stn
          limy2 = j_stn + np_search

          if (limx1 .le. 1) then
           limx1 = 1
          endif

          if (limx1 .ge. nx) then
           limx1 = nx
          end if

          if (limx2 .le. 1) then
           limx2 = 1
          endif

          if (limx2 .ge. nx) then
           limx2 = nx
          end if

          if (limy1 .le. 1) then
           limy1 = 1
          endif

          if (limy1 .ge. ny) then
           limy1 = ny
          end if

          if (limy2 .le. 1) then
           limy2 = 1
          endif

          if (limy2 .ge. ny) then
           limy2 = ny
          end if

          weight = 0.
          insigr_curr_stn(ii) = 0.
          accsigr_curr_stn(ii) = 0.

          do j=limy1,limy2
           do i=limx1,limx2
            dist = sqrt( real( (i-i_stn)**2 + (j-j_stn)**2 ) )
            weight = weight + exp(-dist**2 / alpha)
            insigr_curr_stn(ii) = insigr_curr_stn(ii) +  &
              exp(-dist**2 / alpha)*ice_thick_grid(i,j)

           enddo
          enddo
          insigr_curr_stn(ii) = insigr_curr_stn(ii)/weight

         else
          tmp_curr_stn(ii) = undef
          den_curr_stn(ii) = undef
          rh_curr_stn(ii) = undef
          ws_curr_stn(ii) = undef
          wd_curr_stn(ii) = undef
          ql_curr_stn(ii) = undef
          qct_curr_stn(ii) = undef
          sw_curr_stn(ii) = undef
          insigr_curr_stn(ii) = undef
          accsigr_curr_stn(ii) = undef
         endif
 
        enddo


        call clsgks

        ! ==
        ! 1) find most recent accumulation in the past
        !    12-h
        do ii=1,nstn
         accum_use = 0.
         if (tmp_curr_stn(ii).ne.undef) then
          do ip1=ntimes-12,ntimes
           if ( (tmp_timeser_stn(ii,ip1).ne.undef).and. &
                (accsigr_timeser_stn(ii,ip1).ne.undef) ) then
            accum_use = accsigr_timeser_stn(ii,ip1)
           endif
          enddo

          accum_use = accum_use + insigr_curr_stn(ii)
         endif

         accsigr_curr_stn(ii) = accum_use

        enddo
        ! ======== output to file =======
        do ii=1,nstn
         write(99,2000) xlat_stn(ii),  &
          xlon_stn(ii),        &
          tmp_curr_stn(ii),    &
          den_curr_stn(ii),    & 
          rh_curr_stn(ii),     & 
          ws_curr_stn(ii),     & 
          wd_curr_stn(ii),     & 
          ql_curr_stn(ii),     & 
          qct_curr_stn(ii),    & 
          sw_curr_stn(ii),     & 
          insigr_curr_stn(ii), & 
          accsigr_curr_stn(ii)

2000     format(12(e13.6,1x))
        enddo

        end

! =======================
! NCLFORTSTART
!      SUBROUTINE DCOMPUTEUVGRID(U,V,UVGRID,LONGCA,LONGCB,FLONG,FLAT, &
!                               CEN_LONG,CONE,RPD,NX,NY,NZ,NXP1,NYP1)
      SUBROUTINE DCOMPUTEUVGRID(U,V,UVGRID,FLAT,FLONG, &
                               CEN_LONG,CONE,RPD,NX,NY,NZ,NXP1,NYP1)

      IMPLICIT NONE
      INTEGER NX,NY,NZ,NXP1,NYP1,NL
      REAL U(NXP1,NY,NZ),V(NX,NYP1,NZ)
      REAL UVGRID(NX,NY,NZ,2)
      REAL FLONG(NX,NY),FLAT(NX,NY)
      REAL LONGCB(NX,NY),LONGCA(NX,NY)
      REAL CEN_LONG,CONE,RPD
! NCLEND

      INTEGER I,J,K
      DOUBLE PRECISION UK,VK


!      WRITE (6,FMT=*) ' in compute_uvmet ',NX,NY,NZ,NXP1,NYP1

      DO J = 1,NY
          DO I = 1,NX

              LONGCA(I,J) = FLONG(I,J) - CEN_LONG
              IF (LONGCA(I,J).GT.180.D0) THEN
                  LONGCA(I,J) = LONGCA(I,J) - 360.D0
              END IF
              IF (LONGCA(I,J).LT.-180.D0) THEN
                  LONGCA(I,J) = LONGCA(I,J) + 360.D0
              END IF
              IF (FLAT(I,J).LT.0.D0) THEN
                  LONGCB(I,J) = -LONGCA(I,J)*CONE*RPD
              ELSE
                  LONGCB(I,J) = LONGCA(I,J)*CONE*RPD
              END IF

              LONGCA(I,J) = COS(LONGCB(I,J))
              LONGCB(I,J) = SIN(LONGCB(I,J))

          END DO
      END DO

!      WRITE (6,FMT=*) ' computing velocities '

      DO K = 1,NZ
          DO J = 1,NY
              DO I = 1,NX
                  UK = U(I,J,K)
                  VK = V(I,J,K)

                  ! ====== original: rotate grid winds to Earth-relative winds
                  UVGRID(I,J,K,1) = VK*LONGCB(I,J) + UK*LONGCA(I,J)
                  UVGRID(I,J,K,2) = VK*LONGCA(I,J) - UK*LONGCB(I,J)
                  
                  ! ====== rotate Earth-relative to grid-relative winds
                  !UVGRID(I,J,K,1) = UK*LONGCA(I,J) - VK*LONGCB(I,J)
                  !UVGRID(I,J,K,2) = UK*LONGCB(I,J) + VK*LONGCA(I,J)
              END DO
          END DO
      END DO

      RETURN
      END

! ---------------------------
! subroutine to calculate meteorological wind direction in deg
! 0 deg = N
!
! input: u - zonal wind
!        v - meridional wind
!
! output: dir - meteorological wind direction (deg)

       subroutine winddir(u,v,dir,nx,ny,nz)

       real u(nx,ny,nz), v(nx,ny,nz), dir(nx,ny,nz)

       do k=1,nz
        do j=1,ny
         do i=1,nx

          if (u(i,j,k).eq.0.) u(i,j,k)=1.e-11
          if (v(i,j,k).eq.0.) v(i,j,k)=1.e-11

          dir(i,j,k)=atan(v(i,j,k)/u(i,j,k))

          if ( (u(i,j,k).ge.0.).and.(v(i,j,k).ge.0.) ) then
           dir(i,j,k)=270.-57.29578*dir(i,j,k)
          elseif ( (u(i,j,k).ge.0.).and.(v(i,j,k).le.0.) ) then
           dir(i,j,k)=270.+57.29578*abs(dir(i,j,k))
          elseif ( (u(i,j,k).le.0.).and.(v(i,j,k).le.0.) ) then
           dir(i,j,k)=90.-57.29578*abs(dir(i,j,k))
          elseif ( (u(i,j,k).le.0.).and.(v(i,j,k).ge.0.) ) then
           dir(i,j,k)=90.+57.29578*abs(dir(i,j,k))
          endif

         enddo
        enddo
       enddo

       return
       end

