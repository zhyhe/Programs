	subroutine Delta_Omega(Fij,delta,DO1,DO2,DO3,DlndL) !! D1, D2 with unit: deg^2
	implicit none
	double precision, parameter :: PI=3.141592653589793
	double precision :: delta
	double precision :: Fij(1:9,1:9)
	double precision :: Fij2(1:3,1:3)
	double precision :: Fij3(1:5,1:5)
	double precision :: DO1,DO2,DO3
	double precision :: Cov(1:9,1:9)
	double precision :: Cov2(1:3,1:3)
	double precision :: Cov3(1:5,1:5)
	integer          :: n,n2,n3
	double precision :: theta
	double precision :: deg2nat
	double precision :: sd1,sd2,sd3
	double precision :: nat2deg_sq
	double precision :: DlndL
	integer          :: i,j,k
	nat2deg_sq=(180.0/PI)**2
	deg2nat=(PI/180.0)
	theta = (delta+90.0)*deg2nat !!!! theta=delta+Pi/2  or delta=theta-Pi/2
!!!!!!!! calculate D1
! in the stationary case, (combination of four angles) have the degenracy with t_c
!	do i=1,9
!	Fij(i,1)=0.0
!	Fij(1,i)=0.0
!	Fij(i,2)=0.0
!	Fij(2,i)=0.0
!	Fij(i,3)=0.0
!	Fij(3,i)=0.0
!	Fij(i,4)=0.0
!	Fij(4,i)=0.0
!	enddo
!	Fij(1,1)=1.0
!	Fij(2,2)=1.0
!	Fij(3,3)=1.0
!	Fij(4,4)=1.0
	

	n=9
	call INVERSEMATRIX(Cov,Fij,n)
!	write(*, *) Cov(1, 1), Cov(1, 2), Cov(2, 2)
	sd1=(Cov(1,1)*Cov(2,2)-Cov(1,2)*Cov(1,2))!*(deg2nat**4) 
 	DO1=(2.0*PI)*abs(sin(theta))*sqrt(sd1)!*nat2deg_sq !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!cos(theta)------>sin(theta)
	DlndL=sqrt(Cov(7,7))
!	do i=1,9
!	write(*,*) i,sqrt(Cov(i,i))
!	enddo
!	write(*,*) 'Delta_dL is', sqrt(Cov(7,7)), 
!     $             1.0/sqrt(Fij(7,7)), log(1000.0)
!!!!!!!! calculate D2
	n2=3
!	do i=1,n2
!	do j=1,n2
!	Fij2(i,j)=Fij(i,j)
!	enddo
!	enddo	
	Fij2(1,1)=Fij(1,1)
	Fij2(2,2)=Fij(2,2)
	Fij2(3,3)=Fij(5,5)
	Fij2(1,2)=Fij(1,2)
	Fij2(2,1)=Fij2(1,2)
	Fij2(1,3)=Fij(1,5)
	Fij2(3,1)=Fij2(1,3)
	Fij2(2,3)=Fij(2,5)
	Fij2(3,2)=Fij2(2,3)
	call INVERSEMATRIX(Cov2,Fij2,n2)
	sd2=(Cov2(1,1)*Cov2(2,2)-Cov2(1,2)*Cov2(1,2))!*(deg2nat**4) 
	DO2=(2.0*PI)*abs(sin(theta))*sqrt(sd2)!*nat2deg_sq
!!!!!!!! calculate D3
	n3=5
	do i=1,n3
	do j=1,n3
	Fij3(i,j)=Fij(i,j)
	enddo
	enddo	
	call INVERSEMATRIX(Cov3,Fij3,n3)
	sd3=(Cov3(1,1)*Cov3(2,2)-Cov3(1,2)*Cov3(1,2))!*(deg2nat**4) 
	DO3=(2.0*PI)*abs(sin(theta))*sqrt(sd3)!*nat2deg_sq !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!cos(theta)------>sin(theta)
	end


