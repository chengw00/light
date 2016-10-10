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
                                                ql_grid,    &
                                               qct_grid

        real, dimension(nx_max,ny_max) :: xlat_grid,        &
                                          xlon_grid,        &
                                          hgt_grid,         &
                                          swdown_grid

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

        real :: MVD, rho_w, rho_i, d_c,    &
                freezing_fraction, alpha

        real :: Rtmp0, Rtmp, mu_a, St, E

        ! ===============
        nstn = 0

        open(unit=10, file='line_latlon_all_use.txt', status='old')
         do ii=1,nstn_max
          read(10,*,end=100) xlat_stn(ii), xlon_stn(ii)
          nstn = nstn + 1
         enddo
100     close(10)

        ! ======== output to file =======
        open(unit=10, file='sample.dat', status='old')

        do ii=1,nstn
         read(10,2000) xlat_stn(ii),  &
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
        close(10)

        ! ===========
        do ii=1,nstn
         if ( (tmp_curr_stn(ii).ne.undef).and.  &
              (insigr_curr_stn(ii).gt.0.) ) then
          accsigr_curr_stn(ii) = 1+2*real(ii)/real(nstn)
         endif
        enddo

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

