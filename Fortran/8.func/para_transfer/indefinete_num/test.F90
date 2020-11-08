program ex0826
        implicit none
        interface !定义函数的使用接口
                subroutine sub(a,b) !定义子程序sub的使用接口
                        implicit none
                        integer a
                        integer,optional :: b
                end subroutine sub
        end interface
        call sub(1)
        call sub(2,3)
        stop


end
subroutine sub(a,b)
        implicit none
        integer :: a
        integer,optional :: b
        if (present(b)) then !有输入b时
                print "('a=',I1,' b=',I1)",a,b
        else 
                print "('a=',I1,' b=unknown')",a
        endif
        return
end
