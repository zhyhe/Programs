!!!     from gr-qc/9804014
	subroutine Pat_Func(alpha,varphi,delta,       ! input (degree, GW source)
     $                      psi,lambda,gama,zeta,     ! input (degree, GW detector)
     $                      tc,Mc,f,onoff,                       ! input, quantify the quantity: Omega*t
     $                      F1,Fx,Fb,Fs,Fv1,Fv2)      ! output
!	Program response
	implicit none
!	input: alpha=right ascension [Chi Jing] of GW source; 
!	       delta=declination [Chi Wei] of GW source; 
!	       varphi=polarization angle of GW source;
!  alpha =phi[general]                -----> phi[general]=alpha
!  delta =theta[general]-90.0deg      -----> theta[general]=delta+90.0deg
!  varphi=-varphi[general]+270.0deg   -----> varphi[general]=-varphi+270.0deg
!
!	input: psi=North Latitute of the detector's site; 
!	       lambdar=psi_r is equivalent to -L, which is the East Long.; 
!	       t=Omega_r*t_star;
!	       t_star is time at t(f)=tc-(5/256)*(G*Mc/c^3)**(-5/3)*(Pi*f)**(-8/3)
!                                    =tc-(Mc/M_sun)**(-5/3)*(f/Hz)**(-8/3)*(6.46209*10^6)
!	       tc=t0-dL/c, but here we can ignore dL/c term
!	input: gama is measured counter-clockwise from East to the bisector of the interomater arms; 
!	       zeta=angle between two arms;
!	output: F1=F_{plus}; Fx=F_{cross}; Fb=F_b; Fs=F_s; Fv1=F_{vx}; Fv2=F_{vy}
!	!响应函数计算 
!	!定义角度变量 
!	!定义响应函数变量以及文章中的响应函数变量
!	!定义变换矩阵
!	!定义探测器参考系中的Hij分量
!	!定义探测器臂的方向
!	!定义eij并赋初值
!!! note that, we have fix t=0 at which the Greenick sidereal time is zero
	double precision :: alpha, varphi, delta !! GW source
	double precision :: psi, lambda, gama, zeta !! detector    
	double precision :: tc, Mc, f, onoff
	double precision :: t, Omegar, t_star
	double precision :: F1, Fx, Fb, Fs ,Fv1, Fv2, F1a, Fxa, at, bt               
	double precision, parameter :: PI=3.141592653589793
	double precision :: M1(3, 3)                                                 
	double precision :: M2(3, 3)
	double precision :: M3(3, 3)
        double precision :: M(3, 3)
        double precision :: H1(3, 3), Hx(3, 3), Hb(3, 3), Hs(3, 3)
	double precision :: Hv1(3, 3), Hv2(3, 3) 
        double precision :: n1(3)                                               
	double precision :: n2(3)
	double precision :: e1(3, 3)               
	double precision :: ex(3, 3)
	double precision :: eb(3, 3)
	double precision :: es(3, 3)
	double precision :: ev1(3, 3)
	double precision :: ev2(3, 3)
	integer          :: i, j, k
	double precision :: lambdar
	double precision :: Salpha,Svarphi,Spsi
	double precision :: Slambdar,Sgama,Szeta,Sdelta
!       if onoff=1, time dependent; onoff=0, time independent
!!! note that, we have fix t=0 at which the Greenick sidereal time is zero. If not zero but a, then lambdar=lambda+a
	lambdar=1.0*lambda ! East Long.
	Omegar=(2.0*PI)/(24.0*3600.0)
	t_star=tc-(Mc**(-5.0/3.0))*(f**(-8.0/3.0))
     $	               *(644029.24687)*onoff
    	t=Omegar*t_star
	e1 = 0.0               
	ex = 0.0
	eb = 0.0
	es = 0.0
	ev1 = 0.0
	ev2 = 0.0
	e1(1, 1) = 1.0                            
	e1(2, 2) = -1.0
	ex(1, 2) = 1.0
	ex(2, 1) = 1.0
	eb(1, 1) = 1.0
	eb(2, 2) = 1.0
	es(3, 3) = 1.0
	ev1(1, 3) = 1.0
	ev1(3, 1) = 1.0
	ev2(2, 3) = 1.0
	ev2(3, 2) = 1.0
	Salpha   = alpha*(PI/180.0)
	Svarphi  = varphi*(PI/180.0)
	Sdelta   = delta*(PI/180.0)
	Spsi     = psi*(PI/180.0)
	Slambdar = lambdar*(PI/180.0)
	Sgama    = gama*(PI/180.0)
	Szeta    = zeta*(PI/180.0)
!	计算变换矩阵
!	write(*, *)  alpha, varphi, delta, psi, lambdar, t, gamma, zeta
	M3(1, 1) = -sin(Sgama+PI/4.0)                    
	M3(1, 2) = cos(Sgama+PI/4.0)
	M3(1, 3) = 0.0
	M3(2, 1) = -cos(Sgama+PI/4.0)
	M3(2, 2) = -sin(Sgama+PI/4.0)
	M3(2, 3) = 0.0
	M3(3, 1) = 0.0
	M3(3, 2) = 0.0
	M3(3, 3) = 1.0
	M2(1, 1) = sin(Spsi)*cos(Slambdar+t)
	M2(1, 2) = sin(Spsi)*sin(Slambdar+t)
	M2(1, 3) = -cos(Spsi)
	M2(2, 1) = -sin(Slambdar+t)
	M2(2, 2) = cos(Slambdar+t)
	M2(2, 3) = 0.0
	M2(3, 1) = cos(Spsi)*cos(Slambdar+t)
	M2(3, 2) = cos(Spsi)*sin(Slambdar+t)
	M2(3, 3) = sin(Spsi)
	M1(1, 1) = sin(Salpha)*cos(Svarphi)
     $            -cos(Salpha)*sin(Sdelta)*sin(Svarphi) 
	M1(1, 2) = -cos(Salpha)*cos(Svarphi)
     $            -sin(Salpha)*sin(Sdelta)*sin(Svarphi)
	M1(1, 3) = cos(Sdelta)*sin(Svarphi)
	M1(2, 1) = -sin(Salpha)*sin(Svarphi)
     $             -cos(Salpha)*sin(Sdelta)*cos(Svarphi)
	M1(2, 2) = cos(Salpha)*sin(Svarphi)
     $             -sin(Salpha)*sin(Sdelta)*cos(Svarphi)
	M1(2, 3) = cos(Sdelta)*cos(Svarphi)
	M1(3, 1) = -cos(Salpha)*cos(Sdelta)
	M1(3, 2) = -sin(Salpha)*cos(Sdelta)
	M1(3, 3) = -sin(Sdelta)
	n1(1) = cos(Szeta/2.0-PI/4.0)                                    
	n1(2) = sin(Szeta/2.0-PI/4.0)
	n1(3) = 0.0
	n2(1) = cos(-Szeta/2.0+3.0/4.0*PI)
	n2(2) = sin(-Szeta/2.0+3.0/4.0*PI)
	n2(3) = 0.0
!c	write(*, *) "M3"
!c	do i = 1, 3
!c	  write(*, *) M3(i, :)
!c	end do
!c	write(*, *) "M2"
!c	do i = 1, 3
!c	  write(*, *) M2(i, :)
!c	end do
!c	write(*, *) "M1"
!c	do i = 1, 3
!c	  write(*, *) M1(i, :)
!c	end do

!	计算M并计算响应函数

	M = matmul(matmul(M3, M2), transpose(M1))        
!c	write(*, *) "M"
!c	do i = 1, 3
!c	  write(*, *) M(i, :)
!c	end do  
	H1 = matmul(matmul(M, e1), transpose(M))        
	Hx = matmul(matmul(M, ex), transpose(M))
	Hb = matmul(matmul(M, eb), transpose(M))
	Hs = matmul(matmul(M, es), transpose(M))
	Hv1 = matmul(matmul(M, ev1), transpose(M))
	Hv2 = matmul(matmul(M, ev2), transpose(M))
	F1 = 0.0
	Fx = 0.0
	Fb = 0.0
	Fs = 0.0
	Fv1= 0.0
	Fv2= 0.0
	do i = 1, 3                                                          
 	 do j = 1, 3
 	  F1 = F1+0.5*n1(i)*H1(i,j)*n1(j)-0.5*n2(i)*H1(i,j)*n2(j)
 	  Fx = Fx+0.5*n1(i)*Hx(i,j)*n1(j)-0.5*n2(i)*Hx(i,j)*n2(j)
 	  Fb = Fb+0.5*n1(i)*Hb(i,j)*n1(j)-0.5*n2(i)*Hb(i,j)*n2(j)
  	  Fs = Fs+0.5*n1(i)*Hs(i,j)*n1(j)-0.5*n2(i)*Hs(i,j)*n2(j)
  	  Fv1 = Fv1+0.5*n1(i)*Hv1(i,j)*n1(j)-0.5*n2(i)*Hv1(i,j)*n2(j)
  	  Fv2 = Fv2+0.5*n1(i)*Hv2(i,j)*n1(j)-0.5*n2(i)*Hv2(i,j)*n2(j) 
	 end do
	end do
!c	write(*, *) "F+ = ",F1
!c	write(*, *) "Fx = ",Fx
!c	write(*, *) "Fb = ",Fb
!c	write(*, *) "Fs = ",Fs
!c	write(*, *) "Fv1 = ",Fv1
!c	write(*, *) "Fv2 = ",Fv2

!	计算文章中的a，b，以及两个响应函数
!	write(*,*) t
	at=0.0
	at = 1.0/16.0*sin(2.*Sgama)*(3.-cos(2.*Spsi)) 
     $	 *(3.-cos(2.*Sdelta))*cos(2.*(Salpha-Slambdar-t))       
	at = at-1.0/4.0*cos(2.*Sgama)*sin(Spsi) 
     $	 *(3.-cos(2.*Sdelta))*sin(2.*(Salpha-Slambdar-t))
	at = at+1.0/4.0*sin(2.*Sgama)*sin(2.*Spsi) 
     $	 *sin(2.*Sdelta)*cos(Salpha-Slambdar-t)
	at = at-1.0/2.0*cos(2.*Sgama)*cos(Spsi) 
     $	 *sin(2.*Sdelta)*sin(Salpha-Slambdar-t)
	at = at+3.0/4.0*sin(2.*Sgama)*cos(Spsi) 
     $	 *cos(Spsi)*cos(Sdelta)*cos(Sdelta)
	bt=0.0
	bt = cos(2*Sgama)*sin(Spsi)*sin(Sdelta)
     $      *cos(2.*(Salpha-Slambdar-t))
	bt = bt+1.0/4.0*sin(2.*Sgama)*(3.-cos(2.*Spsi)) 
     $	 *sin(Sdelta)*sin(2.*(Salpha-Slambdar-t))
	bt = bt+cos(2.*Sgama)*cos(Spsi)
     $   *cos(Sdelta)*cos(Salpha-Slambdar-t)
	bt = bt+1.0/2.0*sin(2.*Sgama)*sin(2.*Spsi)*cos(Sdelta)
     $       *sin(Salpha-Slambdar-t)
	F1a = sin(Szeta)*(at*cos(2.0*Svarphi)+bt*sin(2.0*Svarphi))
	Fxa = sin(Szeta)*(bt*cos(2.0*Svarphi)-at*sin(2.0*Svarphi))
!	write(*, *) "F+ in article = ",F1a
!	write(*, *) "Fx in article = ",Fxa
	end






	subroutine Pat_stationary(alpha,varphi,delta,       ! input (degree, GW source)
     $                            psi,lambda,gama,zeta,     ! input (degree, GW detector)
     $	                          x,y,z,
     $                            Fplus,Fcross)             ! output
	implicit none
	double precision :: alpha,varphi,delta
	double precision :: psi,lambda,gama,zeta
	double precision :: Fplus,Fcross
	double precision :: x, y, z
	double precision :: F1,Fx,Fb,Fs,Fv1,Fv2
	double precision :: onoff
!       if onoff=1, time dependent; onoff=0, time independent
!	input: alpha=right ascension of GW source; 
!	       delta=declination of GW source; 
!	       varphi=polarization angle of GW source;
!	input: psi=North Latitute of the detector's site; 
!	       lambdar=psi_r is equivalent to L, which is the East Long.; 
!	input: gama is measured counter-clockwise from East to the bisector of the interomater arms; 
!	       zeta=angle between two arms;
!	double precision :: Ftheta,Fphi,Fpsi



!	x=1.0
!	y=1.0
!	z=1.0
	onoff=0.0
	call Pat_Func(alpha,varphi,delta,       ! input (degree, GW source)
     $                psi,lambda,gama,zeta,     ! input (degree, GW detector)
     $                x,y,z,onoff,              ! input, quantify the quantity: Omega*t
     $                F1,Fx,Fb,Fs,Fv1,Fv2)      ! output
	Fplus =F1
	Fcross=Fx



!!!!!!!!!!!!!!! for check!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	Ftheta=alpha*(3.14/180.0)
!	Fphi=(90.0-delta)*(3.14/180.0)
!	Fpsi=varphi*(3.14/180.0)
!	Fplus=(0.5*(1.0+(cos(Ftheta))**2)*cos(2.0*Fphi)*cos(2.0*Fpsi)
!     $	-cos(Ftheta)*sin(2.0*Fphi)*sin(2.0*Fpsi))*(sqrt(3.0)/2.0)
!	Fcross=(0.5*(1.0+(cos(Ftheta))**2)*cos(2.0*Fphi)*sin(2.0*Fpsi)
!     $	+cos(Ftheta)*sin(2.0*Fphi)*cos(2.0*Fpsi))*(sqrt(3.0)/2.0)
	end


!	program main
!	implicit none
!	double precision :: alpha, varphi, delta  !! source
!	double precision :: psi, lambda, t, gama, zeta  !! detector  
!	double precision :: tc, Mc, f, onoff    
!	double precision :: F1, Fx, Fb, Fs ,Fv1, Fv2 
!	double precision :: Fplus, Fcross
!	onoff=0.0
!	alpha=12.0
!	delta=23.1
!	varphi=57.2
!	psi=109.8
!	lambda=23.4
!	gama=21.0
!	zeta=11.9
!	tc=0.0
!	Mc=1.2
!	f=100.0
!	call Pat_Func(alpha,varphi,delta,psi,lambda,gama,zeta,tc,Mc,f,onoff, ! input (degree)
!     $                       F1, Fx, Fb, Fs ,Fv1, Fv2)          ! output 
!	write(*, *) "F+ = ",F1
!	write(*, *) "Fx = ",Fx
!	write(*, *) "Fb = ",Fb
!	write(*, *) "Fs = ",Fs
!	write(*, *) "Fv1 = ",Fv1
!	write(*, *) "Fv2 = ",Fv2
!	call Pat_stationary(alpha,varphi,delta,psi,lambda,gama,zeta,  ! input (degree)
!     $                       Fplus, Fcross)          ! output 
!	write(*, *) "F+ = ",Fplus
!	write(*, *) "Fx = ",Fcross
!	end




