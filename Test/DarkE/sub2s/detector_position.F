	subroutine rI(lambda,psi,                 ! input (degree, GW detector)
     $                tc,Mc,f,onoff,              ! input, quantify the quantity: Omega*t
     $                x,y,z)                      ! output, UNITS: kilometers
	implicit none
	double precision, parameter :: PI=3.141592653589793
	double precision :: lambda, psi
	double precision :: tc, Mc, f
	double precision :: x, y, z
	double precision :: R_earth
	double precision :: theta, phi
	double precision :: deg2nat
	double precision :: Omegar, t_star, lambdar, t
	double precision :: onoff
!       if onoff=1, time dependent; onoff=0, time independent
!!! note that, we have fix t=0 at which the Greenick sidereal time is zero. If not zero but a, then lambdar=lambda+a
	lambdar=1.0*lambda ! East Long.
	deg2nat=(PI/180.0)
	R_earth=(6378.137+6356.752)/2.0   !!! kilometer
	theta = (90.0-psi)*deg2nat
	Omegar=(2.0*PI)/(24.0*3600.0)
	t_star=tc-(Mc**(-5.0/3.0))*(f**(-8.0/3.0))*(646029.24687)*onoff
	t=Omegar*t_star
	phi=lambdar*deg2nat+t
	x=R_earth*(sin(theta)*cos(phi))
	y=R_earth*(sin(theta)*sin(phi))
	z=R_earth*(cos(theta))
	end

	subroutine rI_stationary(lambda,psi,                 ! input (degree, GW detector)
     $	                         tc, Mc, f, 
     $                           x,y,z)                      ! output, UNITS: kilometers
	implicit none
	double precision :: lambda, psi
	double precision :: tc, Mc, f
	double precision :: x1, y1, z1
	double precision :: x, y, z	
	double precision :: onoff
!       if onoff=1, time dependent; onoff=0, time independent
	onoff=0.0
!	tc=0.0
!	Mc=0.0
!	f =0.0
	call  rI(lambda,psi,                 ! input (degree, GW detector)
     $           tc,Mc,f,onoff,              ! input, quantify the quantity: Omega*tutput
     $           x1,y1,z1)
	x=x1
	y=y1
	z=z1
	end 


!	program main
!	implicit none
!	double precision :: lambda, psi
!	double precision :: tc, Mc, f
!	double precision :: x, y, z
!	double precision :: onoff
!	onoff=1.0
!	lambda=-119.41
!	psi=46.45
!	tc=0.0
!	Mc=1.0
!	f=100.0
!	call rI(lambda,psi,tc,Mc,f,onoff,x,y,z)
!	write(*,*) x, y, z
!	call rI_stationary(lambda,psi,x,y,z)
!	write(*,*) x, y, z
!	end
