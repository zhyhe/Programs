!这是一个无限死循环程序
        implicit none
        integer,parameter :: n=50
        integer :: i, a(3*n,n)

        a=0
        a(35,35)=1
        a(36,36)=1
        a(35,36)=1
        a(36,35)=1

        print'(150I1)',a
        do j=2,n-1
        do i=2,3*n-1
                do l=-1,1
                do k=-1,1
                s=s+a(i+k,j+l)
                enddo
                enddo


end
