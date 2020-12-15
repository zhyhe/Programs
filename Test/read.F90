Program reads
        real(8) b, a(6)
        integer i,n
        print*,i,n
        open(unit=10,file='~/workspace/DE.data/New_SNRall_ET2CE7200000_1to1.dat')
        read (10,*)
        read (10,*)

        do i=1,20
        read(10,*)b,a
        print'(6F10.4)',a
        enddo
        close(10)
end

