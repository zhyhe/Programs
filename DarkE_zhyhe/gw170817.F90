Program gw170817
        use Fisher
        implicit none
        integer,parameter:: imax=95841
        integer          :: counter,N_detector
        integer          :: i,j,k
        double precision :: z0,dL,dL_c
        double precision :: t_c
        double precision :: m1,m2
        integer          :: qt0(6)
        double precision :: de(7,4)
        double precision :: fbase(3,imax),Nbase(3,imax)
        double precision :: para(9)
        double precision :: Fij(9,9),cov(9,9),Fij_s(7,9,9)
        double precision :: SNR_s(7),SNR,SNRI
        complex(8)       :: dihat
        real(8)          :: FLSO

        real(8)    :: F1,Fx,Fb,Fs,Fv1,Fv2
        print*,H0


        z0=0.01D0
        call dl_Func(dL,z0)
        dL_c=dL/1000
        print*,'dL_c=',dL_c


        !! The time of the GW event !!
        qt0=(/2017,8,17,12,41,4/)
        call t_c_Func(qt0,t_c)
        print *,'t_c=',t_c

        !detector data (/Latitude,Longitude,Orientation,The angle between two arms/)

        de(1,:)=(/30.56_8,-90.77_8,243._8,90._8/)       !LIGO Livingston
        de(2,:)=(/46.45_8,-119.41_8,171.8_8,90._8/)  !LIGO Handford
        de(3,:)=(/43.63_8,10.5_8,116.5_8,90._8/)         !VIRGO
        de(4,:)=(/36.25_8,137.18_8,0._8,90._8/)           !KAGRA
        de(5,:)=(/19.09_8,74.05_8,0._8,90._8/)          !LIGO India
        de(6,:)=(/43.54_8,10.42_8,19.48_8,60._8/)    !ET

        open(unit=10,file="noises/L.dat",Status="OLD")
        open(unit=20,file="noises/H.dat",Status="OLD")
        open(unit=30,file="noises/V.dat",Status="OLD")

        do counter=1,imax
        read(10,*) fbase(1,counter),Nbase(1,counter)
        read(20,*) fbase(2,counter),Nbase(2,counter)
        read(30,*) fbase(3,counter),Nbase(3,counter)
        enddo
        close(10)
        close(20)
        close(30)

        N_detector=3
        para(1)=197.45_8     !!! alpha,degree
        para(2)=-23.381_8    !!! delta,degree
        para(3)=0.1_8        !!! varphi,degree
        para(4)=28           !!! iota,degree
        m1=(2.26_8+1.36_8)/2
        m2=(1.36_8+0.86_8)/2
        para(8)=(m1+m2)*(1+z0)      !!!M,M_sun
        para(9)=m1*m2/((m1+m2)**2)  !!!eta
        para(5)=t_c          !!!t_c:sec
        para(6)=0            !!!psi_c
        para(7)=log(1000.)   !!!ln(dL/Mpc)
        print*,para

        !! calculate Fisher matrix in general case !!
        Fij=0.
        Fij_s=0.
        SNRI=0.
        SNR_s=0.
        !do k=1,3
        !        call Fisher_LIGO_general(de(k,1),de(k,2),de(k,3),de(k,4),para,fbase(k,:),Nbase(k,:),imax,Fij_s(k,:,:),SNR_s(k))                                      !output
        !        do i=1,9
        !                do j=1,9
        !                        Fij(i,j)=Fij(i,j)+Fij_s(k,i,j)
        !                enddo
        !        enddo
        !        SNRI=SNRI+SNR_s(k)**2
        !enddo
        !print'(9F19.9)',Fij
        FLSO=2198.64/para(8)
        call Pat_Func(para(1),para(2),para(3),de(1,1),de(1,2),de(1,3),de(1,4),para(5),para(8),50._8,1._8,F1,Fx,Fb,Fs,Fv1,Fv2)
        print*,F1,Fx
        print*,h2f(50._8,para(8),para(9),para(5),para(6),para(4),35._8,750._8,F1,Fx)
        call dihat_general(de(1,1),de(1,2),de(1,3),de(1,4),para(1),para(2),para(3),para(4),para(8),para(9),para(7),\
        para(5),para(6),50D0,FLSO,fbase(1,:),Nbase(1,:),1,imax,1,dihat)
        print*,dihat
end
