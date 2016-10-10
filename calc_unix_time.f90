	integer*4 :: i4time_current_cycle  ! current cycle Unix time


        integer*4 :: iyear_current_cycle,         &   ! YYYY
                     imonth_current_cycle,        &   ! MM
                     iday_current_cycle,          &   ! DD
                     ihour_current_cycle,         &   ! HH
                     iminute_current_cycle            ! mm
      
        character (len=300) :: adum


        ! =============
        call getarg(1,adum)
        read(adum,*) iyear_current_cycle
        call getarg(1,adum)
        read(adum,*) imonth_current_cycle
        call getarg(1,adum)
        read(adum,*) iday_current_cycle
        call getarg(1,adum)
        read(adum,*) ihour_current_cycle
        call getarg(1,adum)
        read(adum,*) iminute_current_cycle

        ! ============
        call calculate_i4time_unix(iyear_current_cycle,     &
                                  imonth_current_cycle,     &
                                    iday_current_cycle,     &
                                  ihour_current_cycle,      &
                                  ihour_current_cycle,      &
                                  i4time_current_cycle)

        write(*,*) i4time_current_cycle

 	end
! ========= subroutine to calculate Unix Time =========
! Input:
!    NYEAR: YYYY
!    NMONTH: MM
!    NDAY:   DD
!    NHOUR:  HH
!    NMIN:   MM

        SUBROUTINE CALCULATE_I4TIME_UNIX(NYEAR,NMONTH,NDAY,NHOUR,NMIN,I4TIME)

        INTEGER*4 NYEAR, NMONTH, NDAY, NHOUR, NMIN, NJULIAN, I
        INTEGER*4 I4TIME
!
        INTEGER*4 NJUL_DAYS(12)
        DATA NJUL_DAYS/0,31,59,90,120,151,181,212,243,273,304,334/

        NJULIAN = NJUL_DAYS(NMONTH) + NDAY

        IF (NMONTH .GT. 2 .AND. MOD (NYEAR,4) .EQ. 0)           &
                       NJULIAN = NJULIAN + 1


!       Valid for years 1970-2060
        IF ( (NYEAR .LT. 2000).AND.(NYEAR.GE. 1970) ) THEN
         I = NYEAR - 1900
        ELSEIF ( (NYEAR .LE. 2060).AND.(NYEAR.GE. 2000) ) THEN
         I = NYEAR - 2000 + 100
        ELSE
         WRITE(*,*) 'YEAR MUST BE BETWEEN 1970 to 2060'
         STOP
        ENDIF

!        LP = (I + 3 - 70) / 4
        LP = (I + 1 - 70) / 4

        I4TIME =  (I-70) * 31536000              &
         + (NJULIAN-1)   * 86400                 &
         + NHOUR      * 3600                     &
         + NMIN       * 60                        

        I4TIME = I4TIME + 86400 * LP

        RETURN
        END

