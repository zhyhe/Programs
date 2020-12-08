module constants
        implicit none
        real(8),   parameter :: PI=3.141592653589793_8
        complex(8),parameter :: cj=(0.,1.)
        real(8),   parameter :: c0=299792.458_8!km/s
        real(8),   parameter :: Msun=1.989D30!kg
        real(8),   parameter :: GM3=1.547451289752685D-5 !PI*G*Msun/c0^3
        real(8),   parameter :: MG56C32pc6=3.668817100756728D-19!(G*Msun)^(5/6)*c0^(-3./2)/(Mpc)
        real(8),   parameter :: G=6.67259D-11!Nm^2/kg^2
        real(8),   parameter :: pc=3.0856777581467192D16!m
        real(8),   parameter :: F0=2198.49833705756 !c0^3/[6^(3/2)*PI*Msun*G]/2
        !*************************************************!
        real(8),   parameter :: w0=-1,wa=0.,Omega_k=0.
        real(8),   parameter :: Omega_m=0.2736_8,Omega_de=0.7264_8
        real(8),   parameter :: H0=67.3_8
        !*************************************************!
        real(8),   parameter :: deg2nat=1.745329300562541D-2 !pi/180
end module constants

