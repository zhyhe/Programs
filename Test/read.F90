Program reads
        real(8) b, a(6)
        open(unit=10,file='~/workspace/DE/New_SNRall_ET2CE7200000_1to1.dat')
        read (10,*)
        read (10,*)

        do i=1,20
        read(10,*)b,a
        print*,a
        enddo
        close(10)
end

