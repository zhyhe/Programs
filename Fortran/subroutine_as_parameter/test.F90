!fortran语言允许将过程作为参数传递给其他过程，下面是示例代码：
program test
        implicit none
        real, external :: func
        real           :: x, y 
        external       :: sub
        
        x = 2.; y = 0.
        call sub1(func, x, y)  !// 将函数作为实参调用
        print*, y
        
        x = 2.; y = 0.
        call sub2(sub, x, y)  !// 将子程序作为实参调用
        print*, y
        
end program test
      
!// ==================================================
subroutine sub1(f, x, y)
        implicit none

        real, external    :: f
        real, intent(in)  :: x
        real, intent(out) :: y
        
        y = x * f(x)
        
end subroutine sub1
        
        
real function func( x )
        implicit none
        real, intent(in) :: x 
        
        func = exp(x)
end function func
        
!// ==================================================
subroutine sub2(sub, x, y)
        implicit none
        external :: sub
        real, intent(in)  :: x 
        real, intent(out) :: y 
        
        call sub(x, y)
        y = x * y
        
end subroutine sub2
        
subroutine sub(x, y)
        implicit none
        real, intent(in)  :: x 
        real, intent(out) :: y 
        
        y = exp(x)
end subroutine sub
