        integer, parameter :: nstn_max = 2500, ndom_max = 10 

        real, parameter :: undef = -999.,    &
                           ztower = 50.

        ! ===== station variables =====
        real, dimension(nstn_max) ::                  &
                                          xlat_stn,   &
                                          xlon_stn

        real, dimension(nstn_max,ndom_max) ::         &
                                     tmp_curr_stn,    &
                                     den_curr_stn,    &
                                     rh_curr_stn,     &
                                     ws_curr_stn,     &
                                     wd_curr_stn,     &
                                     ql_curr_stn,     &
                                    qct_curr_stn,     &
                                     sw_curr_stn,     &
                                 insigr_curr_stn,     &
                                 accsigr_curr_stn 

        ! ====== file names =========
        character (len=250), dimension(ndom_max) :: file_in

        integer, dimension(ndom_max) :: flag_in

        ! =====
        tmp_curr_stn = undef
        den_curr_stn = undef
        rh_curr_stn = undef
        ws_curr_stn = undef
        wd_curr_stn = undef
        ql_curr_stn = undef
        qct_curr_stn = undef
        sw_curr_stn = undef
        insigr_curr_stn = undef
        accsigr_curr_stn = undef

        ! ==========
        ndom = 0
        do j=1,ndom_max
         read(*,*,end=100) file_in(j)
         read(*,*,end=100) flag_in(j)
         ndom = ndom + 1
        enddo
100     continue

         ! ===============
        nstn = 0

        open(unit=10, file='line_latlon_all_use.txt', status='old')
         do ii=1,nstn_max
          read(10,*,end=200) xlat_stn(ii), xlon_stn(ii)
          nstn = nstn + 1
         enddo
200     close(10)

        write(*,*) 'nstn = ', nstn

        ! ======== read from file =======
        do jj=1,ndom
         if (real(flag_in(jj)).gt.0.5) then
 
          open(unit=10, file=file_in(jj)(1:len_trim(file_in(jj))), status='old')

          do ii=1,nstn
           read(10,2000) xlat_stn(ii),  &
                         xlon_stn(ii),  &
                   tmp_curr_stn(ii,jj), &
                   den_curr_stn(ii,jj), & 
                    rh_curr_stn(ii,jj), & 
                    ws_curr_stn(ii,jj), & 
                    wd_curr_stn(ii,jj), & 
                    ql_curr_stn(ii,jj), & 
                   qct_curr_stn(ii,jj), & 
                    sw_curr_stn(ii,jj), & 
                insigr_curr_stn(ii,jj), & 
               accsigr_curr_stn(ii,jj)
2000       format(12(e13.6,1x))
          enddo
          close(10)
         endif
        enddo  ! jj-loop

        ! ===== fill outer domain with inner domain ===
        do jj=2,ndom
         if (tmp_curr_stn(ii,jj).ne.undef) then
          tmp_curr_stn(ii,1)=tmp_curr_stn(ii,jj)
          den_curr_stn(ii,1)=den_curr_stn(ii,jj)
          rh_curr_stn(ii,1)=rh_curr_stn(ii,jj)
          ws_curr_stn(ii,1)=ws_curr_stn(ii,jj)
          wd_curr_stn(ii,1)=wd_curr_stn(ii,jj)
          ql_curr_stn(ii,1)=ql_curr_stn(ii,jj)
          qct_curr_stn(ii,1)=qct_curr_stn(ii,jj)
          sw_curr_stn(ii,1)=sw_curr_stn(ii,jj)
          insigr_curr_stn(ii,1)=insigr_curr_stn(ii,jj)
          accsigr_curr_stn(ii,1)=accsigr_curr_stn(ii,jj)
         endif
        enddo

        ! ===========
        do ii=1,nstn
         write(99,2000) xlat_stn(ii),  &
          xlon_stn(ii),          &
          tmp_curr_stn(ii,1),    &
          den_curr_stn(ii,1),    &
          rh_curr_stn(ii,1),     &
          ws_curr_stn(ii,1),     &
          wd_curr_stn(ii,1),     &
          ql_curr_stn(ii,1),     &
          qct_curr_stn(ii,1),    &
          sw_curr_stn(ii,1),     &
          insigr_curr_stn(ii,1), &
          accsigr_curr_stn(ii,1)
        enddo

        end

