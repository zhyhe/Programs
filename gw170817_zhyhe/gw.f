	include "noises/NOISES.F"
        program main
            use imsl
            use sub
	    implicit none
	    integer          :: i, j, k, l
	    double precision :: DO1, DO2, DO3, DO4
	    double precision :: DOI1, DOI2, DOI3, DOI4
	    integer          ::  N_detector
	    double precision :: m1, m2
	    double precision :: DlndL, DlndLI
	    integer :: imax, n = 9
	    double precision :: Fij_s(1:7 ,1:9, 1:9)
	    double precision :: Fij(1:9, 1:9), cov(1:9, 1:9)
	    double precision :: de(9,4)
	    double precision, allocatable :: fbase(:, :), Nbase(:, :)
	    integer          :: counter, stat
	    double precision :: SNR_s(7), SNRI
	    double precision :: z0, dL, dL_c
	    double precision :: omega1, omega2
	    integer :: qt0(1:6)
	    double precision :: t_c
            real(8)          :: F8(8,8),F7(7,7),F6(6,6),Cov6(6,6)
            type(detector)   :: Detec
            type(source)     :: S


	    z0 = 2D0
	    call dL_Func(dL, z0)
	    dL_c = 1.0D0 * dL / 1000.0D0

!!!!!!!     The time of the GW event.
	    qt0 = (/2017, 8, 17, 12, 41, 4/)
	    call t_c_Func(qt0, t_c)



!	    detector data (/Latitude,Longitude,Orientation,The angle between two arms/)

	    de(1, :) = (/30.56, -90.77,  243.0, 90.0/)    !LIGO Livingston
	    de(2, :) = (/46.45, -119.41, 171.8, 90.0/)    !LIGO Handford
	    de(3, :) = (/43.63, 10.5,    116.5, 90.0/)    !VIRGO
	    de(4, :) = (/36.25, 137.18,  0.0,   90.0/)    !KAGRA
	    de(5, :) = (/19.09, 74.05,   0.0,   90.0/)    !LIGO India
            de(6, :) = (/43.54, 10.42,   19.48, 60.0/)    !ET1
            de(7, :) = (/43.54, 10.42,   139.48,60.0/)    !ET2
            de(8, :) = (/43.54, 10.42,   259.48,60.0/)    !ET3

	    imax = 4011
	    allocate(fbase(1:3, 1:imax))
	    allocate(Nbase(1:3, 1:imax))
	    open(unit=10, file="noises/ETB/ETB.txt", Status="OLD")
	    open(unit=20, file="noises/ETB/ETB.txt", Status="OLD")
	    open(unit=30, file="noises/ETB/ETB.txt", Status="OLD")
	    do counter = 1, imax, 1
	        read(10, *) fbase(1, counter), Nbase(1, counter)
	        read(20, *) fbase(2, counter), Nbase(2, counter)
	        read(30, *) fbase(3, counter), Nbase(3, counter)
	    end do
	    close(10)
	    close(20)
	    close(30)


	    N_detector=3
	    !para(1)  =  197.45  !!! alpha, degree
	    !para(2)  =  -23.381   !!! delta, degree
	    !para(3)  =  0.1   !!! varphi, degree
	    !para(4)  =  28.0   !!! iota, degree
	    !m1 = (2.26 + 1.36) / 2.0
	    !m2 = (1.36 + 0.86) / 2.0
	    !para(8)  =  (m1 + m2) * (1 + z0)                !!! M, M_sun
	    !para(9)  =  m1 * m2 / ((m1 + m2)**2)   !!! eta
	    !para(5)  =  t_c           !!! t_c: second
	    !para(6)  =  0.0          !!! psi_c
	    !para(7)  =  log(1000.0) !!! ln(d_L/Mpc)
	    
            S.alpha  =  197.45  !!! alpha, degree
	    S.delta  =  -23.381   !!! delta, degree
	    S.varphi =  0.1   !!! varphi, degree
	    S.iota   =  28.0   !!! iota, degree
	    m1 = (2.26 + 1.36) / 2.0
	    m2 = (1.36 + 0.86) / 2.0
	    S.M      =  (m1 + m2) * (1 + z0)                !!! M, M_sun
	    S.eta    =  m1 * m2 / ((m1 + m2)**2)   !!! eta
	    S.t_c    =  t_c           !!! t_c: second
	    S.psi_c  =  0.0          !!! psi_c
	    S.lndL   =  log(1000.0) !!! ln(d_L/Mpc)

            open(unit=13,file='~/workspace/DE.data/
     $New_SNRall_ET2CE7200000_1to1.dat')
                do i=1,2
                read (13,*)
                enddo
                do l=1,20
                read(13,*)DO1,S.z,S.lndL,S.alpha,S.delta,S.varphi,S.iota
            S.M=2.7_8*(1+S.z)
            S.eta=0.25_8
            S.t_c=0.
            S.psi_c=0.
            S.lndL=log(1000.0)
            call dL_Func(dL, S.z)
            dL_c = 1.0D0 * dL / 1000.0D0


!!!!!     calculate Fisher matrix in general case
	    Fij = 0.0
	    Fij_s = 0.0
	    SNRI = 0.0
	    SNR_s = 0.0
	    do k = 6, 8
                Detec=detector(de(k,1),de(k,2),de(k,3),de(k,4))

	        call Fisher_LIGO_general(Detec,        ! input: (degree, GW detector)
     $	                          S,                   ! nine parameters
     $                            fbase(k,:),Nbase(k,:),imax,   ! input for noise
     $                            Fij_s(k,:,:),SNR_s(k))               ! output
	        do i = 1,9
	            do j = 1,9
	                Fij(i,j) = Fij(i,j)+Fij_s(k,i,j)
	            end do
	        end do
	        SNRI = SNRI + SNR_s(k) * SNR_s(k)
	    end do
            
            !print'(9E15.4)',Fij
!	    write(*, *) SNR_s
	    call Delta_Omega(Fij,S.delta,DOI1,DOI2,DOI3,DlndLI)
	    SNRI = sqrt(SNRI)
	    DO1 = DO1 * dL_c * dL_c
	    DO2 = DO1 * dL_c * dL_c
	    DO3 = DO3 * dL_c * dL_c
	    DOI1 = DOI1 * dL_c * dL_c
	    DOI2 = DOI2 * dL_c * dL_c
	    DOI3 = DOI3 * dL_c * dL_c
	    SNRI = 1.0D0 * SNRI / dL_c
	    DlndL = dlndL * dL_c
	    DlndLI = DlndLI * dL_c
	    omega1 = DO1 * -1.0D0 * log(0.1D0)
	    omega2 = DOI1 * -1.0D0 * log(0.1D0)
	    !write(*, *) omega2, DlndLI
!!!!        Calculate the covariance matrix
            Fij=Fij/dL_c**2

	    !call INVERSEMATRIX(Cov, Fij, n)
            !Cov=Cov*dL_c*dL_c
	    !write(*, *) Cov(1, 1), Cov(1, 2), Cov(2, 2), Cov(7, 7)
	    !write(*, *) SNRI

            F8=del(Fij,9,1,1)
            F7=del(F8,8,1,1)
            F6=del(F7,7,2,2)
            Cov6=inverse(F6,6)

            print'(F6.4,F21.15)',S.z,Sqrt(Cov6(4,4))
            enddo
            close(13)
	end
