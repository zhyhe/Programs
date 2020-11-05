        program Ex0606
                implicit none
                integer year,month,day
                print *,"请输入年份，月份，中间以逗号分隔："
                200 read (*,*)year,month
                select case(month)
                        case(1,3,5,7,8,10,12)
                                day=31
                        case(4,6,9,11)
                                day=30
                        case(2)
                                if(mod(year,4)==0) then
                                        day=29
                                else 
                                        day=28
                                end if
                        case default
                                print *,"输入错误，请重新输入："
                                goto 200
                end select
                print 100, year,month,day
                100 format("year=",I4,2X,"month=",I2,2X,"days=",I2)
        end


