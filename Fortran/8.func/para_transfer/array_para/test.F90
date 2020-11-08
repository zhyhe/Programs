program ex0825
        implicit none
        interface !定义函数的使用接口
               function random10(lowbound,upbound)
                      implicit none
                      real :: lowbound,upbound
                      real :: random10(10) !返回值是个数组
               end function
        end interface

        real :: a(10)
        CALL RANDOM_SEED()
        a=random10(1.,10.)
        print "(10F6.2)",a
end

function random10(lowbound,upbound)
        implicit none
        real :: lowbound,upbound
        real :: len
        real :: random10(10)
        real t
        integer i
        len = upbound-lowbound
        do i=1,10
        call random_number(t)
        random10(i)=lowbound+len*t
        enddo
        return
end





