program ex0827
        implicit none
        interface
                real function func(x,a,b,c)
                        implicit none
                        real x
                        real,optional :: a,b,c
                end function
        end interface

        print *,func(2.,c=1.)
        print*,func(2.,a=2.,b=1.)
end
real function func(x,a,b,c)
        implicit none
        real x
        real,optional :: a,b,c
        real ra,rb,rc
        if(present(a))then
                ra=a
        else 
                ra=0.
        endif
        if(present(b))then
                rb=b
        else
                rb=0.
        endif
        if(present(c))then
                rc=c
        else
                rc=0.
        endif
        func=ra*x**2+rb*x+rc
end

