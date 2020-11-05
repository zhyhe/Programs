        implicit none
        REAL,parameter::pi=3.141592653589793,err=1.e-6
        integer,parameter::max_terms=15
        real x,term,sinx
        integer k
        print *,"请输入x的角度值："
        read *,x
        x=x*pi/180
        k=1
        term=x
        sinx=term
        do while (abs(term)>err .and. k<max_terms)
                term=-term*x*x/(2*k*(2*k+1))
                k=k+1
                sinx=sinx+term
                print 100,k,sinx
        enddo
        print "(A,F10.8)","系统计算的函数值为：",sin(x)
        100 format("第",I2,"次运算的结果为：",2X,F10.8)
        end
