	subroutine n_hat(alpha,delta,              ! alpha is right ascension[Chi Jing], and delta is declination[Chi Wei]
     $                   x,y,z)                    ! output
	implicit none
	double precision, parameter :: PI=3.141592653589793
	double precision :: alpha, delta
	double precision :: x, y, z
	double precision :: theta, phi
	double precision :: deg2nat
	deg2nat=(PI/180.0)
	theta = (-delta+90.0)*deg2nat  !!! theta=-delta+PI/2
	phi   = (alpha+0.00)*deg2nat  !!! phi  =alpha
	x=1.0*(sin(theta)*cos(phi))
	y=1.0*(sin(theta)*sin(phi))
	z=1.0*(cos(theta))
	end

!	program main
!	implicit none
!!	input: alpha=right ascension of GW source; 
!!	       delta=declination of GW source; 
!	double precision :: alpha, delta
!	double precision :: x, y, z
!	alpha=-119.41
!	delta=46.45
!	call n_hat(alpha,delta,x,y,z)
!	write(*,*) x, y, z
!	end
