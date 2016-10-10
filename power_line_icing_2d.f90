	integer, parameter :: nx_max = 600, ny_max = 600, nz_max = 65

        integer, parameter :: ntimes = 48

        integer, parameter :: nstn_max = nx_max*ny_max

        real, parameter :: undef = -999.,    &
                           ztower = 50.

        ! =============
        real, dimension(nx_max,ny_max,nz_max) :: p_grid,    &
                                                 z_grid,    &
                                                tc_grid,    &
                                                 u_grid,    &
                                                 v_grid,    &
                                                rh_grid,    &
                                                ql_grid,    &
                                               qct_grid

        real, dimension(nx_max,ny_max) :: xlat_grid,        &
                                          xlon_grid,        &
                                          hgt_grid,         &
                                          swdown_grid,      &
                                          rainrate_grid

        real, dimension(nx_max,ny_max) :: ice_thick_grid

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
                                 accsigr_curr_stn,    &
                                rainrate_curr_stn

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

        real :: MVD, rho_w, rho_i, d_c,    &
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
!        nstn = 0
!
!        open(unit=10, file='line_latlon_all_use.txt', status='old')
!         do ii=1,nstn_max
!          read(10,*,end=100) xlat_stn(ii), xlon_stn(ii)
!          nstn = nstn + 1
!         enddo
!100     close(10)
!
!        write(*,*) 'nstn = ', nstn
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
        nrec = nrec + 1
        read(10,rec=nrec) rainrate_grid(1:nx,1:ny)

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
          if ( (tc_grid(i,j,k_tower).le.0.).and.    &
               (ql_grid(i,j,k_tower).gt.0.) ) then

           Rtmp = (tc_grid(i,j,k_tower) + 273.15)*(9./5.)
           mu_a = ( (3.62e-7)*4.44822/(0.3048000**2) ) * (Rtmp/Rtmp0)**1.5 * (Rtmp0+198.72)/(Rtmp0+198.72)  ! Pa s
           St = (wspd_grid(i,j,k_tower)* MVD**2 * rho_w)/(9* mu_a* d_c)
           E  = St**2 /(St+0.7)**2
           density_air = 100.*p_grid(i,j,k_tower)/(287.*  &
               (tc_grid(i,j,k_tower)+273.16))

           ice_thick_grid(i,j) = 3600*1000* E*ql_grid(i,j,k_tower)*density_air*wspd_grid(i,j,k_tower)*freezing_fraction/rho_i
          endif
         enddo
        enddo

        ! =========== get station time series ===========
        nstn = nx*ny
        do ip1=1,ntimes

         if (real(iflag_timeser(ip1)).gt.0.5) then
          open(unit=10, file=file_timeser(ip1)(1:len_trim(file_timeser(ip1))), status='old')
          read(10,*)
          read(10,*)
          read(10,*)
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
                      accsigr_timeser_stn(ii,ip1), & 
                      dum_rainrate_curr_stn
          enddo

          close(10)
         endif
        enddo

        ! ===============
        ! figure out station latlon
        nstn = nx*ny
        ii = 0
        do jg=1,ny
         do ig=1,nx
          ii = ii + 1
          i_stn = ig
          j_stn = jg
          xlat_stn(ii) = xlat_grid(i_stn,j_stn) 
          xlon_stn(ii) = xlon_grid(i_stn,j_stn)
          tmp_curr_stn(ii) = tc_grid(i_stn,j_stn,k_tower)
          den_curr_stn(ii) = 100*p_grid(i_stn,j_stn,k_tower)/ &
                      (287.*(tc_grid(i_stn,j_stn,k_tower)+273.16))
          rh_curr_stn(ii) = rh_grid(i_stn,j_stn,k_tower)
          ws_curr_stn(ii) = sqrt(u_grid(i_stn,j_stn,k_tower)**2 + v_grid(i_stn,j_stn,k_tower)**2)
          call winddir(u_grid(i_stn,j_stn,k_tower),v_grid(i_stn,j_stn,k_tower),wd_curr_stn(ii),1,1,1)
          ql_curr_stn(ii) = ql_grid(i_stn,j_stn,k_tower)
          qct_curr_stn(ii) = qct_grid(i_stn,j_stn,k_tower)
          sw_curr_stn(ii) = swdown_grid(i_stn,j_stn)

          insigr_curr_stn(ii) = ice_thick_grid(i_stn,j_stn)
          rainrate_curr_stn(ii) = rainrate_grid(i_stn,j_stn)
          if (rainrate_curr_stn(ii).lt.0.) rainrate_curr_stn(ii) = 0.
         enddo
        enddo


        ! ======= check if above 5 deg C for more than ===
        !         6 hours 
        
        ! ==
        ! 1) find most recent accumulation in the past
        !    12-h
        !
        ! 2) find if T > 5 deg C for more than 6-h
        !    if so, melt accum
        !
        do ii=1,nstn
         if (tmp_curr_stn(ii).ne.undef) then
          accum_use = 0.
          ncount_melt = 0
          do ip1=ntimes-12,ntimes
           if ( (tmp_timeser_stn(ii,ip1).ne.undef).and. &
                (accsigr_timeser_stn(ii,ip1).ne.undef) ) then
            accum_use = accsigr_timeser_stn(ii,ip1)
           endif
          enddo

          do ip1=1,ntimes
           if (tmp_timeser_stn(ii,ip1).ne.undef) then
            if (tmp_timeser_stn(ii,ip1).ge.5.) then
             ncount_melt = ncount_melt + 1
            endif
           endif
          enddo

          if (ncount_melt.ge.6) accum_use = 0.
          accum_use = accum_use + insigr_curr_stn(ii)
          accsigr_curr_stn(ii) = accum_use

         endif

        enddo
        ! ======== output to file =======
        write(99,1000)  'latitude     ',      &
                        'longitude    ',      &
                        'temperature  ',      &
                        'air_density  ',      &
                        'rel_humidity ',      &
                        'wind_speed   ',      &
                        'met_wind_dir ',      &
                        'liq_mix_ratio',      &
                        'totcondmixrat',      &
                        'shortwave_rad',      &
                        'inst_ice_rate',      &
                        'total_ice_acc',      &
                        'rain_rate_1h '

        write(99,1000)  'deg          ',      &
                        'deg          ',      &
                        'deg_C        ',      &
                        'kg/m**3      ',      &
                        'percent      ',      &
                        'm/s          ',      &
                        'deg          ',      &
                        'kg/kg        ',      &
                        'kg/kg        ',      &
                        'W/m**2       ',      &
                        'mm/h         ',      &
                        'mm           ',      &
                        'mm/h         '

1000     format(13(a13,1x))
       
        write(99,1100) nx, ny, nx*ny
1100    format(2(i6,1x),i10)
 
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
          accsigr_curr_stn(ii),&
          rainrate_curr_stn(ii)
2000     format(13(e13.6,1x))
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

