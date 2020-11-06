!c	include "subs/detector_position.F"
!c	include "subs/source_direction.F"
!c	include "subs/NOISES.F"
!c	include "subs/HF2_GR.F"
!c	include "subs/response.F"
!c	include "subs/di.F"
!c	include "subs/INVERSE_FISHER.F"
!c	include "subs/Delta_Omegas_cal.F"





	subroutine Fisher_LIGO_stationary(psi,lambda,gama,zeta,  ! input: (degree, GW detector)
     $	                             para,                  ! nine parameters
     $                               fbase,Nbase,imax,      ! input for noise
     $                               Fij,SNR)               ! output
!   para(1)=alpha; para(2)=delta; para(3)=varphi; para(4)=iota
!   para(5)=t_c;   para(6)=psi_c
!   para(7)=lndL
!   para(8)=M;     para(9)=eta;   	
!	program main
	implicit none
	double precision, parameter :: PI=3.141592653589793
!	double precision :: alpha,delta,varphi
	double precision :: lambda,psi
	double precision :: gama,zeta
!	double precision :: iota
	double precision :: f
!	double precision :: M,eta,t_c,psi_c
!	double precision :: dL
	double precision :: FLSO
	double complex   :: dihat
!	double precision :: m1,m2
	double precision :: ef
	integer          :: turn_on_off
	double complex   :: dihat2(1:9),D_dihat(1:9)
	double precision :: Fij(1:9,1:9)
	double precision :: SNR,SNR2
	integer          :: i,j,k
	double precision :: para(1:9),para2(1:9),step(1:9)
	double precision :: deltaef
	double precision :: DO1,DO2,DO3
	double precision :: fbase(1:imax),Nbase(1:imax)
	integer          :: imax
	integer :: ith0, ith1, ith2, ith3, ith4, ith5
	integer :: ith6, ith7, ith8, ith9
	ith0=1
	ith1=1
	ith2=1
	ith3=1
	ith4=1
	ith5=1
	ith6=1
	ith7=1
	ith8=1
	ith9=1
!   para(1)=alpha; para(2)=delta; para(3)=varphi; para(4)=iota
!   para(5)=t_c;   para(6)=psi_c
!   para(7)=dL
!   para(8)=M;     para(9)=eta;   

!!!! input 
!c	para(1)  =  70.0 !alpha, degree
	step(1)  =  0.001
!c	para(2)  =  20.0 !delta, degree
	step(2)  =  0.001
!c	para(3)  =  35.0 !varphi, degree
	step(3)  =  0.001
!c	para(4)  =  15.0 !iota, degree
	step(4)  =  0.001
!	m1=1.4    !!! unit M_sun
!	m2=1.4    !!! unit M_sun
!	M=m1+m2   !!! unit M_sun
!	eta=(m1*m2)/(M*M)
!c	para(8)  =  2.8    !!! M, M_sun
	step(8)  =  0.00001
!c	para(9)  =  0.25   !!! eta 
	step(9)  =  0.000001
!c	para(5)  =  0.0    !!! t_c: second
	step(5)  =  0.00001
!c	para(6)  =  0.0	   !!! psi_c
	step(6)  =  0.0001
!c	para(7)  =  log(1000.0) !!! ln(d_L/Mpc)
	step(7)  =  0.001
	do i=1,9
	para2(i)=para(i)+step(i)
	enddo
!!!! GW detector parameters 
!c	psi=20.0
!c	lambda=50.0
!c	gama=10.0
!c	zeta=90.0
	turn_on_off=1
	FLSO=2198.64/para(8)  !!!!unit Hz
	deltaef=0.001
!!!!!!!! calculate the case with time-independent
	Fij=0.0
	SNR2=0.0
	do ef=log(10.001),log(2.0*FLSO),deltaef
	f=exp(ef)
	call dihat_stationary(psi,lambda,gama,zeta,  ! input: (degree, GW detector)
     $                 para(1),para(2),para(3),      ! input: (degree, GW source)
     $                 para(4),                      ! input: (degree, GW source)
     $                 para(8),para(9),para(7),      ! input: GW source masses and distance
     $                 para(5),para(6),              ! input: initial condition
     $                 f,FLSO,                       ! input: frequency and merge frequency
     $                 fbase,Nbase,ith0,imax,         ! input for noise
     $                 turn_on_off,                  ! input: =0 if not icluding S_i(f); =1 if including S_i(f)
     $                 dihat)                        ! output
	call dihat_stationary(psi,lambda,gama,zeta,  ! input: (degree, GW detector)
     $                 para2(1),para(2),para(3),      ! input: (degree, GW source)
     $                 para(4),                      ! input: (degree, GW source)
     $                 para(8),para(9),para(7),      ! input: GW source masses and distance
     $                 para(5),para(6),              ! input: initial condition
     $                 f,FLSO,                       ! input: frequency and merge frequency
     $                 fbase,Nbase,ith1,imax,         ! input for noise
     $                 turn_on_off,                  ! input: =0 if not icluding S_i(f); =1 if including S_i(f)
     $                 dihat2(1))                    ! output
	call dihat_stationary(psi,lambda,gama,zeta,  ! input: (degree, GW detector)
     $                 para(1),para2(2),para(3),      ! input: (degree, GW source)
     $                 para(4),                      ! input: (degree, GW source)
     $                 para(8),para(9),para(7),      ! input: GW source masses and distance
     $                 para(5),para(6),              ! input: initial condition
     $                 f,FLSO,                       ! input: frequency and merge frequency
     $                 fbase,Nbase,ith2,imax,         ! input for noise
     $                 turn_on_off,                  ! input: =0 if not icluding S_i(f); =1 if including S_i(f)
     $                 dihat2(2))                    ! output
	call dihat_stationary(psi,lambda,gama,zeta,  ! input: (degree, GW detector)
     $                 para(1),para(2),para2(3),      ! input: (degree, GW source)
     $                 para(4),                      ! input: (degree, GW source)
     $                 para(8),para(9),para(7),      ! input: GW source masses and distance
     $                 para(5),para(6),              ! input: initial condition
     $                 f,FLSO,                       ! input: frequency and merge frequency
     $                 fbase,Nbase,ith3,imax,         ! input for noise
     $                 turn_on_off,                  ! input: =0 if not icluding S_i(f); =1 if including S_i(f)
     $                 dihat2(3))                    ! output
	call dihat_stationary(psi,lambda,gama,zeta,  ! input: (degree, GW detector)
     $                 para(1),para(2),para(3),      ! input: (degree, GW source)
     $                 para2(4),                      ! input: (degree, GW source)
     $                 para(8),para(9),para(7),      ! input: GW source masses and distance
     $                 para(5),para(6),              ! input: initial condition
     $                 f,FLSO,                       ! input: frequency and merge frequency
     $                 fbase,Nbase,ith4,imax,         ! input for noise
     $                 turn_on_off,                  ! input: =0 if not icluding S_i(f); =1 if including S_i(f)
     $                 dihat2(4))                    ! output
	call dihat_stationary(psi,lambda,gama,zeta,  ! input: (degree, GW detector)
     $                 para(1),para(2),para(3),      ! input: (degree, GW source)
     $                 para(4),                      ! input: (degree, GW source)
     $                 para2(8),para(9),para(7),      ! input: GW source masses and distance
     $                 para(5),para(6),              ! input: initial condition
     $                 f,FLSO,                       ! input: frequency and merge frequency
     $                 fbase,Nbase,ith8,imax,         ! input for noise
     $                 turn_on_off,                  ! input: =0 if not icluding S_i(f); =1 if including S_i(f)
     $                 dihat2(8))                    ! output
	call dihat_stationary(psi,lambda,gama,zeta,  ! input: (degree, GW detector)
     $                 para(1),para(2),para(3),      ! input: (degree, GW source)
     $                 para(4),                      ! input: (degree, GW source)
     $                 para(8),para2(9),para(7),      ! input: GW source masses and distance
     $                 para(5),para(6),              ! input: initial condition
     $                 f,FLSO,                       ! input: frequency and merge frequency
     $                 fbase,Nbase,ith9,imax,         ! input for noise
     $                 turn_on_off,                  ! input: =0 if not icluding S_i(f); =1 if including S_i(f)
     $                 dihat2(9))                    ! output
	call dihat_stationary(psi,lambda,gama,zeta,  ! input: (degree, GW detector)
     $                 para(1),para(2),para(3),      ! input: (degree, GW source)
     $                 para(4),                      ! input: (degree, GW source)
     $                 para(8),para(9),para(7),      ! input: GW source masses and distance
     $                 para2(5),para(6),              ! input: initial condition
     $                 f,FLSO,                       ! input: frequency and merge frequency
     $                 fbase,Nbase,ith5,imax,         ! input for noise
     $                 turn_on_off,                  ! input: =0 if not icluding S_i(f); =1 if including S_i(f)
     $                 dihat2(5))                    ! output
	call dihat_stationary(psi,lambda,gama,zeta,  ! input: (degree, GW detector)
     $                 para(1),para(2),para(3),      ! input: (degree, GW source)
     $                 para(4),                      ! input: (degree, GW source)
     $                 para(8),para(9),para(7),      ! input: GW source masses and distance
     $                 para(5),para2(6),              ! input: initial condition
     $                 f,FLSO,                       ! input: frequency and merge frequency
     $                 fbase,Nbase,ith6,imax,         ! input for noise
     $                 turn_on_off,                  ! input: =0 if not icluding S_i(f); =1 if including S_i(f)
     $                 dihat2(6))                    ! output
	call dihat_stationary(psi,lambda,gama,zeta,  ! input: (degree, GW detector)
     $                 para(1),para(2),para(3),      ! input: (degree, GW source)
     $                 para(4),                      ! input: (degree, GW source)
     $                 para(8),para(9),para2(7),      ! input: GW source masses and distance
     $                 para(5),para(6),              ! input: initial condition
     $                 f,FLSO,                       ! input: frequency and merge frequency
     $                 fbase,Nbase,ith7,imax,         ! input for noise
     $                 turn_on_off,                  ! input: =0 if not icluding S_i(f); =1 if including S_i(f)
     $                 dihat2(7))                    ! output
	  do i=1,9
	    D_dihat(i)=(dihat2(i)-dihat)/step(i)
	  enddo
	  do i=1,9
	  do j=1,i
	  Fij(i,j)=Fij(i,j)+4.0*f*deltaef
     $            *( Real(D_dihat(i))*Real(D_dihat(j))
     $             +aimag(D_dihat(i))*aimag(D_dihat(j)))
	  Fij(j,i)=Fij(i,j)
	  enddo
	  enddo
	SNR2=SNR2+4.0*f*deltaef
     $            *( Real(dihat)*Real(dihat)
     $             +aimag(dihat)*aimag(dihat))
!	write(*,*) f, deltaef, real(dihat)
	enddo
	SNR=sqrt(SNR2)
!	Fij(7,5)=0.0
!	Fij(5,7)=0.0
!	Fij(7,6)=0.0
!	Fij(6,7)=0.0
!	write(*,*) Fij
	end
!	do i=1,9
!	do j=1,9
!	write(*,122) i,i,Fij(i,i)
!	enddo
!	enddo
!c	call Delta_Omega(Fij,para(2),DO1,DO2,DO3)
!c	write(*,*) 'time independent case'
!c	write(*,121) DO1, DO3, DO2


	subroutine Fisher_LIGO_general(psi,lambda,gama,zeta,  ! input: (degree, GW detector)
     $	                          para,                  ! nine parameters
     $                            fbase,Nbase,imax,  ! input for noise
     $                            Fij,SNR)               ! output
!   para(1)=alpha; para(2)=delta; para(3)=varphi; para(4)=iota
!   para(5)=t_c;   para(6)=psi_c
!   para(7)=dL
!   para(8)=M;     para(9)=eta;   	
!	program main
	implicit none
	double precision, parameter :: PI=3.141592653589793
!	double precision :: alpha,delta,varphi
	double precision :: lambda,psi
	double precision :: gama,zeta
!	double precision :: iota
	double precision :: f
!	double precision :: M,eta,t_c,psi_c
!	double precision :: dL
	double precision :: FLSO
	double complex   :: dihat
!	double precision :: m1,m2
	double precision :: ef
	integer          :: turn_on_off
	double complex   :: dihat2(1:9),D_dihat(1:9)
	double precision :: Fij(1:9,1:9)
	double precision :: SNR,SNR2
	integer          :: i,j,k
	double precision :: para(1:9),para2(1:9),step(1:9)
	double precision :: deltaef
	double precision :: DO1,DO2,DO3
	double precision :: fbase(1:imax),Nbase(1:imax)
	integer          :: imax
	integer :: ith0, ith1, ith2, ith3, ith4, ith5
	integer :: ith6, ith7, ith8, ith9
	ith0=1
	ith1=1
	ith2=1
	ith3=1
	ith4=1
	ith5=1
	ith6=1
	ith7=1
	ith8=1
	ith9=1
!   para(1)=alpha; para(2)=delta; para(3)=varphi; para(4)=iota
!   para(5)=t_c;   para(6)=psi_c
!   para(7)=dL
!   para(8)=M;     para(9)=eta;   

!!!! input 
!c	para(1)  =  70.0 !alpha, degree
	step(1)  =  0.001
!c	para(2)  =  20.0 !delta, degree
	step(2)  =  0.001
!c	para(3)  =  35.0 !varphi, degree
	step(3)  =  0.001
!c	para(4)  =  15.0 !iota, degree
	step(4)  =  0.001
!	m1=1.4    !!! unit M_sun
!	m2=1.4    !!! unit M_sun
!	M=m1+m2   !!! unit M_sun
!	eta=(m1*m2)/(M*M)
!c	para(8)  =  2.8    !!! M, M_sun
	step(8)  =  0.00001
!c	para(9)  =  0.25   !!! eta 
	step(9)  =  0.000001
!c	para(5)  =  0.0    !!! t_c: second
	step(5)  =  0.00001
!c	para(6)  =  0.0	   !!! psi_c
	step(6)  =  0.0001
!c	para(7)  =  log(1000.0) !!! ln(d_L/Mpc)
	step(7)  =  0.001
	do i=1,9
	para2(i)=para(i)+step(i)
	enddo
!!!! GW detector parameters 
!c	psi=20.0
!c	lambda=50.0
!c	gama=10.0
!c	zeta=90.0
	turn_on_off=1
	FLSO=2198.64/para(8)  !!!!unit Hz
	deltaef=0.001
!!!!!!!! calculate the case with time-dependent
!c	write(*,*) '*****************************************************'
	Fij=0.0
	SNR2=0.0
	do ef=log(10.001),log(2.0*FLSO),deltaef
	f=exp(ef)
	call dihat_general(psi,lambda,gama,zeta,     ! input: (degree, GW detector)
     $                 para(1),para(2),para(3),      ! input: (degree, GW source)
     $                 para(4),                      ! input: (degree, GW source)
     $                 para(8),para(9),para(7),      ! input: GW source masses and distance
     $                 para(5),para(6),              ! input: initial condition
     $                 f,FLSO,                       ! input: frequency and merge frequency
     $                 fbase,Nbase,ith0,imax,         ! input for noise
     $                 turn_on_off,                  ! input: =0 if not icluding S_i(f); =1 if including S_i(f)
     $                 dihat)                        ! output
	call dihat_general(psi,lambda,gama,zeta,     ! input: (degree, GW detector)
     $                 para2(1),para(2),para(3),      ! input: (degree, GW source)
     $                 para(4),                      ! input: (degree, GW source)
     $                 para(8),para(9),para(7),      ! input: GW source masses and distance
     $                 para(5),para(6),              ! input: initial condition
     $                 f,FLSO,                       ! input: frequency and merge frequency
     $                 fbase,Nbase,ith1,imax,         ! input for noise
     $                 turn_on_off,                  ! input: =0 if not icluding S_i(f); =1 if including S_i(f)
     $                 dihat2(1))                    ! output
	call dihat_general(psi,lambda,gama,zeta,     ! input: (degree, GW detector)
     $                 para(1),para2(2),para(3),      ! input: (degree, GW source)
     $                 para(4),                      ! input: (degree, GW source)
     $                 para(8),para(9),para(7),      ! input: GW source masses and distance
     $                 para(5),para(6),              ! input: initial condition
     $                 f,FLSO,                       ! input: frequency and merge frequency
     $                 fbase,Nbase,ith2,imax,         ! input for noise
     $                 turn_on_off,                  ! input: =0 if not icluding S_i(f); =1 if including S_i(f)
     $                 dihat2(2))                    ! output
	call dihat_general(psi,lambda,gama,zeta,     ! input: (degree, GW detector)
     $                 para(1),para(2),para2(3),      ! input: (degree, GW source)
     $                 para(4),                      ! input: (degree, GW source)
     $                 para(8),para(9),para(7),      ! input: GW source masses and distance
     $                 para(5),para(6),              ! input: initial condition
     $                 f,FLSO,                       ! input: frequency and merge frequency
     $                 fbase,Nbase,ith3,imax,         ! input for noise
     $                 turn_on_off,                  ! input: =0 if not icluding S_i(f); =1 if including S_i(f)
     $                 dihat2(3))                    ! output
	call dihat_general(psi,lambda,gama,zeta,     ! input: (degree, GW detector)
     $                 para(1),para(2),para(3),      ! input: (degree, GW source)
     $                 para2(4),                      ! input: (degree, GW source)
     $                 para(8),para(9),para(7),      ! input: GW source masses and distance
     $                 para(5),para(6),              ! input: initial condition
     $                 f,FLSO,                       ! input: frequency and merge frequency
     $                 fbase,Nbase,ith4,imax,         ! input for noise
     $                 turn_on_off,                  ! input: =0 if not icluding S_i(f); =1 if including S_i(f)
     $                 dihat2(4))                    ! output
	call dihat_general(psi,lambda,gama,zeta,     ! input: (degree, GW detector)
     $                 para(1),para(2),para(3),      ! input: (degree, GW source)
     $                 para(4),                      ! input: (degree, GW source)
     $                 para2(8),para(9),para(7),      ! input: GW source masses and distance
     $                 para(5),para(6),              ! input: initial condition
     $                 f,FLSO,                       ! input: frequency and merge frequency
     $                 fbase,Nbase,ith8,imax,         ! input for noise
     $                 turn_on_off,                  ! input: =0 if not icluding S_i(f); =1 if including S_i(f)
     $                 dihat2(8))                    ! output
	call dihat_general(psi,lambda,gama,zeta,     ! input: (degree, GW detector)
     $                 para(1),para(2),para(3),      ! input: (degree, GW source)
     $                 para(4),                      ! input: (degree, GW source)
     $                 para(8),para2(9),para(7),      ! input: GW source masses and distance
     $                 para(5),para(6),              ! input: initial condition
     $                 f,FLSO,                       ! input: frequency and merge frequency
     $                 fbase,Nbase,ith9,imax,         ! input for noise
     $                 turn_on_off,                  ! input: =0 if not icluding S_i(f); =1 if including S_i(f)
     $                 dihat2(9))                    ! output
	call dihat_general(psi,lambda,gama,zeta,     ! input: (degree, GW detector)
     $                 para(1),para(2),para(3),      ! input: (degree, GW source)
     $                 para(4),                      ! input: (degree, GW source)
     $                 para(8),para(9),para(7),      ! input: GW source masses and distance
     $                 para2(5),para(6),              ! input: initial condition
     $                 f,FLSO,                       ! input: frequency and merge frequency
     $                 fbase,Nbase,ith5,imax,         ! input for noise
     $                 turn_on_off,                  ! input: =0 if not icluding S_i(f); =1 if including S_i(f)
     $                 dihat2(5))                    ! output
	call dihat_general(psi,lambda,gama,zeta,     ! input: (degree, GW detector)
     $                 para(1),para(2),para(3),      ! input: (degree, GW source)
     $                 para(4),                      ! input: (degree, GW source)
     $                 para(8),para(9),para(7),      ! input: GW source masses and distance
     $                 para(5),para2(6),              ! input: initial condition
     $                 f,FLSO,                       ! input: frequency and merge frequency
     $                 fbase,Nbase,ith6,imax,         ! input for noise
     $                 turn_on_off,                  ! input: =0 if not icluding S_i(f); =1 if including S_i(f)
     $                 dihat2(6))                    ! output
	call dihat_general(psi,lambda,gama,zeta,     ! input: (degree, GW detector)
     $                 para(1),para(2),para(3),      ! input: (degree, GW source)
     $                 para(4),                      ! input: (degree, GW source)
     $                 para(8),para(9),para2(7),      ! input: GW source masses and distance
     $                 para(5),para(6),              ! input: initial condition
     $                 f,FLSO,                       ! input: frequency and merge frequency
     $                 fbase,Nbase,ith7,imax,         ! input for noise
     $                 turn_on_off,                  ! input: =0 if not icluding S_i(f); =1 if including S_i(f)
     $                 dihat2(7))                    ! output
	  do i=1,9
	    D_dihat(i)=(dihat2(i)-dihat)/step(i)
	  enddo
	  do i=1,9
	  do j=1,i
	  Fij(i,j)=Fij(i,j)+4.0*f*deltaef
     $            *( Real(D_dihat(i))*Real(D_dihat(j))
     $             +aimag(D_dihat(i))*aimag(D_dihat(j)))
	  Fij(j,i)=Fij(i,j)
	  enddo
	  enddo
	SNR2=SNR2+4.0*f*deltaef
     $            *( Real(dihat)*Real(dihat)
     $             +aimag(dihat)*aimag(dihat))
	enddo
	SNR=sqrt(SNR2)
!	Fij(7,5)=0.0
!	Fij(5,7)=0.0
!	Fij(7,6)=0.0
!	Fij(6,7)=0.0
	end

