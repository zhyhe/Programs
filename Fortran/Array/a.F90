        implicit none
        real grade(1:10)
        real::a=5
        real::sum=0,average
        integer i
        grade=a
        do i=1,10
                !read *,grade(i)
                print *,grade(i)
                sum=sum+grade(i)
        enddo
        average = sum/10
        print "(A,F7.2)","总成绩为：",sum
        print "(A,F6.2)","平均成绩为：",average
        end
