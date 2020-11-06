module fiducial
       use constants
       implicit none
       contains
               subroutine dL_Func(dL,z0)
                       implicit none
                       double precision :: dL, z0
                       double precision :: sum,z,h
                       integer          :: counter

                       sum=0D0
                       z=0D0
                       h=z0/1000

                       sum=0.5*h/(O_m*(1+z0)**3+O_Lambda)**0.5
                       sum=sum+0.5*h/(O_m+O_Lambda)**0.5
                       do counter=1,999
                       z=z+h
                       sum=sum+h/(O_m*(1+z)**3+O_Lambda)**0.5
                       enddo
                       dL=sum*(1+z0)*c0/H0
               end subroutine dL_Func
               subroutine t_c_Func(qt0,t_c)
                       implicit none
                       integer :: qt0(6),num
                       double precision :: t_c
                       double precision :: yr,month,day,hr,mmin,sec
                       double precision :: UT,aa,bb,JD,ST,t,omegar
                       yr=real(qt0(1))
                       month=real(qt0(2))
                       day=real(qt0(3))
                       hr=real(qt0(4))
                       mmin=real(qt0(5))
                       sec=real(qt0(6))
                       if (month .LE. 2)then
                               month=month+12
                               yr=yr-1
                       endif
                       UT=hr+mmin/60+sec/3600
                       aa=1._8*floor(yr/100)
                       bb=2-aa+floor(aa/4)
                       JD=floor(365.25*(yr+4716))+floor(30.6001_8*(month+1))+day+bb-1524.5+UT/24
                       ST=(JD-2451545)/36525
                       t=280.46061837_8+360.98564736629_8*(JD-2451545)+0.000387933_8*ST**2-ST**3/38710000
                       omegar=2*PI/(24*3600)
                       num=t/360
                       t_c=t-num*360
                       if(t_c .LT. 0)then
                               t_c=t_c+360
                       endif
                       t_c=t_c*PI/180/omegar    
               end subroutine t_c_Func
end module fiducial
