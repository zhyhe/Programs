program test
        implicit none
        integer i
        real x
        real a
        i=1
        !call random_seed()
        do while(i<10)
        call random_seed(size=2)
        print*,a()
        i=i+1
        end do
end

real function a()
        implicit none
        real x
        call random_number(x)
        a=x
end
