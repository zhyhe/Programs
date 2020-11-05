!	include "detector_position.F"
!	include "source_direction.F"
!	include "NOISES.F"
!	include "HF2_GR.F"
!	include "response.F"

!!!     from 1003.2504
	subroutine dihat_stationary(psi,lambda,gama,zeta,  ! input: (degree, GW detector)
     $                              alpha,delta,varphi,    ! input: (degree, GW source)
     $                              iota,                  ! input: (degree, GW source)
     $                              M,eta,lndL,            ! input: GW source masses and distance
     $                              t_c,psi_c,             ! input: initial condition
     $                              f,FLSO,                ! input: frequency and merge frequency
     $                              fbase,Nbase,ith,imax,  ! input for noise
     $                              turn_on_off,           ! input: =0 if not icluding S_i(f); =1 if including S_i(f)
     $                              di_hat)                ! output
	implicit none
	double precision, parameter :: PI=3.141592653589793
	double precision :: alpha,delta,varphi
	double precision :: lambda,psi
	double precision :: gama,zeta
	double precision :: iota
	double precision :: f
	double precision :: M,eta,t_c,psi_c,Mc
	double precision :: lndL,FLSO
	integer          :: turn_on_off
	double complex   :: di_hat
	double precision :: xn,yn,zn
	double precision :: xr,yr,zr
	double precision :: nroverc
	double precision :: Fplus,Fcross
	double complex   :: h2f,d2
	double precision :: s_fit
	double precision :: fbase(1:imax),Nbase(1:imax)
	integer          :: ith,imax
	double complex   :: term1  ! exp(-i*2*PI*f*n*r/c)
	double precision :: term2  ! 1/sqrt(S_i(f))
	double complex   :: term3  ! h2f=sum_{+,x}F*h2(f)
	double precision :: dL
	Mc=M*(eta**(0.6))
!	double complex :: d2_
!!!!  calculate term1
	call n_hat(alpha,delta,xn,yn,zn)
	call rI_stationary(lambda,psi,t_c,Mc,f,xr,yr,zr)
	nroverc=(xn*xr+yn*yr+zn*zr)/(2.99792458*100000.0)
	term1=cmplx(cos(2.0*PI*f*nroverc),-sin(2.0*PI*f*nroverc))
!!!!  calculate term2
	if(turn_on_off.eq.1) then
	call noise(s_fit,f,fbase,Nbase,ith,imax)
	term2=1.0/sqrt(s_fit)
	else
	term2=1.0  !!! for comparing with the previous GR result
	endif
!!!!  calculate term3
	call Pat_stationary(alpha,varphi,delta,       ! input (degree, GW source)
     $                      psi,lambda,gama,zeta,     ! input (degree, GW detector)
     $                      t_c, Mc, f,
     $                      Fplus,Fcross)             ! output
	dL=exp(lndL)
	call HF_BH(d2, f, M, dL, eta, t_c, psi_c, iota, Fplus, Fcross)
!	d2=h2f(f,M,eta,t_c,psi_c,
!     $	            iota,lndL,FLSO,Fplus,Fcross)
!	write(*, *) f, abs(d2), abs(d2_)
!	write(*, *) f, iota, Fplus, Fcross
	term3=d2
!!!!  calculate di_hat	
	di_hat=term1*term2*term3
	end




	subroutine dihat_general(psi,lambda,gama,zeta,  ! input: (degree, GW detector)
     $                           alpha,delta,varphi,    ! input: (degree, GW source)
     $                           iota,                  ! input: (degree, GW source)
     $                           M,eta,lndL,              ! input: GW source masses and distance
     $                           t_c,psi_c,             ! input: initial condition
     $                           f,FLSO,                ! input: frequency and merge frequency
     $                           fbase,Nbase,ith,imax,  ! input for noise
     $                           turn_on_off,           ! input: =0 if not icluding S_i(f); =1 if including S_i(f)
     $                           di_hat)                ! output
	implicit none
	double precision, parameter :: PI=3.141592653589793
	double precision :: alpha,delta,varphi
	double precision :: lambda,psi
	double precision :: gama,zeta
	double precision :: iota
	double precision :: f
	double precision :: M,eta,t_c,psi_c
	double precision :: lndL,FLSO
	integer          :: turn_on_off
	double complex   :: di_hat
	double precision :: xn,yn,zn
	double precision :: xr,yr,zr
	double precision :: nroverc
	double precision :: Fplus,Fcross
	double complex   :: h2f,d2
	double precision :: s_fit
	double precision :: fbase(1:imax),Nbase(1:imax)
	integer          :: ith,imax
	double precision :: Mc,onoff
	double precision :: Fb,Fs,Fv1,Fv2
	double complex   :: term1  ! exp(-if*n*r/c)
	double precision :: term2  ! 1/sqrt(S_i(f))
	double complex   :: term3  ! h2f=sum_{+,x}F*h2(f)
	double precision :: dL
	Mc=M*(eta**(0.6))
	onoff=1.0
!!!!  calculate term1
	call n_hat(alpha,delta,xn,yn,zn)
	call rI(lambda,psi,t_c,Mc,f,onoff,xr,yr,zr)
	nroverc=(xn*xr+yn*yr+zn*zr)/(2.99792458*100000.0)
	term1=cmplx(cos(2.0*PI*f*nroverc),sin(2.0*PI*f*nroverc))
!	term1 = cmplx(1.0D0,0)
!!!!  calculate term2
	if(turn_on_off.eq.1) then
	call noise(s_fit,f,fbase,Nbase,ith,imax)
	term2=1.0/sqrt(s_fit)
	else
	term2=1.0  !!! for comparing with the previous GR result
	endif
!!!!  calculate term3
	call Pat_Func(alpha,varphi,delta,                     ! input (degree, GW source)
     $                psi,lambda,gama,zeta,                   ! input (degree, GW detector)
     $                t_c,Mc,f,onoff,                                  ! input, quantify the quantity: Omega*t
     $                Fplus,Fcross,Fb,Fs,Fv1,Fv2)             ! output
	dL = exp(lndL)
	call HF_BH(d2, f, M, dL, eta, t_c, psi_c, iota, Fplus, Fcross)
!	Fplus = -1.0D0 * Fplus
!	Fcross = -1.0D0 * Fcross
!	d2=h2f(f,M,eta,t_c,psi_c,
!     $	           iota,lndL,FLSO,Fplus,Fcross)
	term3=d2
!!!!  calculate di_hat	
	di_hat=term1*term2*term3
	end



!	program main
!	implicit none
!	double precision :: alpha,delta,varphi
!	double precision :: lambda,psi
!	double precision :: gama,zeta
!	double precision :: iota
!	double precision :: f
!	double precision :: M,eta,t_c,psi_c
!	double precision :: dL,FLSO
!	double complex   :: di_hat
!	double precision :: m1,m2
!	double precision :: ef
!	integer          :: turn_on_off
!	turn_on_off=0
!	psi=20.0
!	lambda=50.0
!	gama=10.0
!	zeta=90.0
!	alpha=70.0
!	delta=20.0
!	varphi=35.0
!	m1=1.4    !!! unit M_sun
!	m2=1.4    !!! unit M_sun
!	M=m1+m2   !!! unit M_sun
!	eta=(m1*m2)/(M*M)
!	t_c=0.0
!	psi_c=0.0
!	iota=15.0 !degree
!	dL=1.0  ! Mpc
!	fLSO=2198.64/M  !!!!unit Hz
!	open (unit=1,file='GR2.dat',status='replace',
!     $	access='sequential',form='formatted')
!	do ef=-2.3,6.9,0.01
!	f=exp(ef)
!!	f=10.0  !Hz
!	call dihat_stationary(psi,lambda,gama,zeta,  ! input: (degree, GW detector)
!     $                 alpha,delta,varphi,    ! input: (degree, GW source)
!     $                 iota,                  ! input: (degree, GW source)
!     $                 M,eta,dL,              ! input: GW source masses and distance
!     $                 t_c,psi_c,             ! input: initial condition
!     $                 f,FLSO,                ! input: frequency and merge frequency
!     $                 turn_on_off,           ! input: =0 if not icluding S_i(f); =1 if including S_i(f)
!     $                 di_hat)                ! output
!	write(1,121) f, real(di_hat), abs(di_hat)
!	enddo
!	open (unit=11,file='GR3.dat',status='replace',
!     $	access='sequential',form='formatted')
!	do ef=-2.303,6.908,0.01
!	f=exp(ef)
!!	f=10.0  !Hz
!	call dihat_general(psi,lambda,gama,zeta,  ! input: (degree, GW detector)
!     $                 alpha,delta,varphi,    ! input: (degree, GW source)
!     $                 iota,                  ! input: (degree, GW source)
!     $                 M,eta,dL,              ! input: GW source masses and distance
!     $                 t_c,psi_c,             ! input: initial condition
!     $                 f,FLSO,                ! input: frequency and merge frequency
!     $                 turn_on_off,           ! input: =0 if not icluding S_i(f); =1 if including S_i(f)
!     $                 di_hat)                ! output
!	write(11,121) f, real(di_hat), abs(di_hat)
!	enddo
!121	format(10E20.10)
!	end	

