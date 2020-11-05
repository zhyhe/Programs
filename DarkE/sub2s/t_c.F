!!!	Calculate the t_c from UTC
!!!	qt0(6):UTC, year, month, day, hour, minute, second


	subroutine t_c_Func(qt0, t_c)
	   implicit none 
	   integer :: qt0(1:6), num
	   double precision :: t_c
	   double precision :: yr, month, day, hr, mmin, sec
	   double precision :: UT, aa, bb, JD, ST, t, omegar
	   double precision :: PI = 3.141592653589793D0     
	   yr    = 1.0D0 * qt0(1)
	   month = 1.0D0 * qt0(2)
	   day   = 1.0D0 * qt0(3)
	   hr    = 1.0D0 * qt0(4)
	   mmin  = 1.0D0 * qt0(5)
	   sec   = 1.0D0 * qt0(6)
	   if ( month .LE. 2 ) then
	      month = month + 12.0D0
	      yr = yr - 1.0D0
	   end if
	   UT = hr + mmin / 60.0D0 + sec / 3600.0D0
	   aa = 1.0D0 * floor(1.0D0 * yr / 100.0D0)
	   bb = 2.0D0 - aa + 1.0D0 * floor(1.0D0 * aa /4.0D0)
	   JD = 1.0D0 * floor(365.25D0 * (yr + 4716.0D0))
     $	     + 1.0D0 * floor(30.6001D0 * (month + 1.0D0))
     $	     + day + bb - 1524.5D0 + 1.0D0 * UT / 24.0D0
	   ST = (JD - 2451545.0D0) / 36525.0D0
	   t  = 280.46061837D0 + 360.98564736629D0 * (JD - 2451545.0D0)
     $	     + 0.000387933D0 * (ST**2) - 1.0D0 * (ST**3) / 38710000.0D0
	   omegar = (2.0D0 * PI) / (24.0D0 * 3600.0D0)
	   num = t / 360
	   t_c = t - num * 360.0D0
	   if(t_c .LT. 0) then
	      t_c = t_c + 360.0D0
	   end if
	   t_c = t_c * PI / 180.0D0
	   t_c = 1.0D0 * t_c / omegar
	end
	

!	program main
!	   implicit none
!	   integer :: qt0(1:6), i
!	   double precision :: t_c
!	   qt0(1) = 2017
!	   qt0(2) = 8
!	   qt0(3) = 17
!	   qt0(4) = 12
!	   qt0(5) = 41
!	   qt0(6) = 4
!	   do i = 1, 100
!	      call t_c_Func(qt0, t_c)
!	      write(*, *) qt0(4), t_c
!	      qt0(4) = qt0(4) + 1
!	   end do
!	end
