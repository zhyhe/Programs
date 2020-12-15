Program calpi
        implicit none
        integer,parameter :: n=10000
        real(8),parameter :: pi0=3.141592653589793_8
        integer :: i,s
        real(8) :: pi,a,b
        real(8) :: start,finish

        call cpu_time(start)
        pi=1
        do i=1,n/2
        a=2D0*i/(2*i-1)
        b=2D0*i/(2*i+1)
        pi=pi*a*b
        enddo
        pi=pi*2
        print*,pi
        call cpu_time(finish)
        print'(F18.15,ES10.0E1,F10.0)',pi,pi-pi0,(finish-start)*1D6

        call cpu_time(start)
        pi=1
        do i=2,n
        a=1D0/i**2
        pi=pi+a
        enddo
        pi=sqrt(pi*6)
        call cpu_time(finish)
        print'(F18.15,ES10.0E1,F10.0)',pi,pi-pi0,(finish-start)*1D6
        
        call cpu_time(start)
        s=1
        pi=1
        do i=1,n
        s=-s
        a=1D0*s/(2*i+1)
        pi=pi+a
        enddo
        pi=pi*4
        call cpu_time(finish)
        print'(F18.15,ES10.0E1,F10.0)',pi,pi-pi0,(finish-start)*1D6

        call cpu_time(start)
        a=1D0/5
        b=1D0/239
        s=1
        pi=4D0/5-1D0/239
        do i=1,9
        s=-s
        a=a*(1D0/25)
        b=b*(1D0/57121)
        pi=pi+s*(4*a-b)/(2*i+1)
        enddo
        pi=pi*4
        call cpu_time(finish)
        print'(F18.15,ES10.0E2,F10.0)',pi,pi-pi0,(finish-start)*1D6

end
