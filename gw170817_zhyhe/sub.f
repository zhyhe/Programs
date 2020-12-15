        module sub
                use typedef
                use fiducial
                implicit none
        contains
        subroutine Fisher_LIGO_general(Detec,         ! input: (degree, GW detector)
     $                            S,                  ! nine parameters
     $                            fbase,Nbase,imax,  ! input for noise
     $                            Fij,SNR)               ! output
!       program main
        implicit none
        double precision :: lambda,psi
        double precision :: gama,zeta
        double precision :: f
        double precision :: FLSO
        double complex   :: dihat
        double precision :: ef
        integer          :: turn_on_off
        double complex   :: dihat2(1:9),D_dihat(1:9)
        double precision :: Fij(1:9,1:9)
        double precision :: SNR,SNR2
        integer          :: i,j,k
        type(source)     :: S
        type(detector)   :: Detec
        double precision :: para(9),para2(9),step(9)
        double precision :: deltaef
        double precision :: DO1,DO2,DO3
        double precision :: fbase(imax),Nbase(imax)
        integer          :: imax
        integer :: ith(0:9)
        ith=1
        psi=Detec.psi
        lambda=Detec.lambda
        gama=Detec.gama
        zeta=Detec.zeta
!   para(1)=alpha; para(2)=delta; para(3)=varphi; para(4)=iota
!   para(5)=t_c;   para(6)=psi_c
!   para(7)=dL
!   para(8)=M;     para(9)=eta;
        para=(/S.alpha,S.delta,S.varphi,S.iota,
     $         S.t_c,S.psi_c,S.lndL,S.M,S.eta /)
!c      para(1)  =  70.0 !alpha, degree
        step(1)  =  0.001
!c      para(2)  =  20.0 !delta, degree
        step(2)  =  0.001
!c      para(3)  =  35.0 !varphi, degree
        step(3)  =  0.001
!c      para(4)  =  15.0 !iota, degree
        step(4)  =  0.001
!       m1=1.4    !!! unit M_sun
!       m2=1.4    !!! unit M_sun
!       M=m1+m2   !!! unit M_sun
!       eta=(m1*m2)/(M*M)
!c      para(8)  =  2.8    !!! M, M_sun
        para(8)  =  S.M    !!! M, M_sun
        step(8)  =  0.00001
!c      para(9)  =  0.25   !!! eta
        para(9)  =  S.eta   !!! eta
        step(9)  =  0.000001
!c      para(5)  =  0.0    !!! t_c: second
        step(5)  =  0.00001
!c      para(6)  =  0.0    !!! psi_c
        step(6)  =  0.0001
!c      para(7)  =  log(1000.0) !!! ln(d_L/Mpc)
        step(7)  =  0.001
        turn_on_off=1
        FLSO=F0/para(8)  !!!!unit Hz
        deltaef=0.001
!!!!!!!! calculate the case with time-dependent
        Fij=0.0
        SNR2=0.0
        open(12,file='/home/zhyhe/workspace/results/H.txt')
        do ef=log(10.001),log(2.0*FLSO),deltaef
        f=exp(ef)
        call dihat_general(psi,lambda,gama,zeta,     ! input: (degree, GW detector)
     $                 para(1),para(2),para(3),      ! input: (degree, GW source)
     $                 para(4),                      ! input: (degree, GW source)
     $                 para(8),para(9),para(7),      ! input: GW source masses and distance
     $                 para(5),para(6),              ! input: initial condition
     $                 f,FLSO,                       ! input: frequency and merge frequency
     $                 fbase,Nbase,ith(0),imax,         ! input for noise
     $                 turn_on_off,                  ! input: =0 if not icluding S_i(f); =1 if including S_i(f)
     $                 dihat)                        ! output
        write(12,*) f,',',real(dihat)
        do i=1,9
        para2=para
        para2(i)=para(i)+step(i)
        call dihat_general(psi,lambda,gama,zeta,     ! input: (degree, GW detector)
     $                 para2(1),para2(2),para2(3),      ! input: (degree, GW source)
     $                 para2(4),                      ! input: (degree, GW source)
     $                 para2(8),para2(9),para2(7),      ! input: GW source masses and distance
     $                 para2(5),para2(6),              ! input: initial condition
     $                 f,FLSO,                       ! input: frequency and merge frequency
     $                 fbase,Nbase,ith(i),imax,         ! input for noise
     $                 turn_on_off,                  ! input: =0 if not icluding S_i(f); =1 if including S_i(f)
     $                 dihat2(i))                    ! output
        enddo
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
        close(12)
        SNR=sqrt(SNR2)
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
        double complex   :: d2
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
        nroverc=(xn*xr+yn*yr+zn*zr)/c0
        term1=cmplx(cos(2.0*PI*f*nroverc),sin(2.0*PI*f*nroverc))
!       term1 = cmplx(1.0D0,0)
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
!       call HF_BH(d2, f, M, dL, eta, t_c, psi_c, iota, Fplus, Fcross)
!       Fplus = -1.0D0 * Fplus
!       Fcross = -1.0D0 * Fcross
        d2=h2f(f,M,eta,t_c,psi_c,
     $             iota,lndL,FLSO,Fplus,Fcross)
        term3=d2
!!!!  calculate di_hat  
        di_hat=term1*term2*term3
        end
        subroutine rI(lambda,psi,                 ! input (degree, GW detector)
     $                tc,Mc,f,onoff,              ! input, quantify the quantity: Omega*t
     $                x,y,z)                      ! output, UNITS: kilometers
        implicit none
        double precision :: lambda, psi
        double precision :: tc, Mc, f
        double precision :: x, y, z
        double precision :: R_earth
        double precision :: theta, phi
        double precision :: Omegar, t_star, lambdar, t
        double precision :: onoff
!       if onoff=1, time dependent; onoff=0, time independent
!!! note that, we have fix t=0 at which the Greenick sidereal time is zero. If not zero but a, then lambdar=lambda+a
        lambdar=1.0*lambda ! East Long.
        R_earth=(6378.137+6356.752)/2.0   !!! kilometer
        theta = (90.0-psi)*deg2nat
        Omegar=(2*PI)/(24.0*3600.0)
        t_star=tc-(Mc**(-5.0/3.0))*(f**(-8.0/3.0))*(646029.24687)*onoff
        t=Omegar*t_star
        phi=lambdar*deg2nat+t
        x=R_earth*(sin(theta)*cos(phi))
        y=R_earth*(sin(theta)*sin(phi))
        z=R_earth*(cos(theta))
        end
        
        subroutine n_hat(alpha,delta,              ! alpha is right ascension[Chi Jing], and delta is declination[Chi Wei]
     $                   x,y,z)                    ! output
        implicit none
        double precision :: alpha, delta
        double precision :: x, y, z
        double precision :: theta, phi
        theta = (-delta+90.0)*deg2nat  !!! theta=-delta+PI/2
        phi   = (alpha+0.00)*deg2nat  !!! phi  =alpha
        x=1.0*(sin(theta)*cos(phi))
        y=1.0*(sin(theta)*sin(phi))
        z=1.0*(cos(theta))
        end


        subroutine Pat_Func(alpha,varphi,delta,       ! input (degree, GW source)
     $                      psi,lambda,gama,zeta,     ! input (degree, GW detector)
     $                      tc,Mc,f,onoff,                       ! input, quantify the quantity: Omega*t
     $                      F1,Fx,Fb,Fs,Fv1,Fv2)      ! output
!       Program response
        implicit none
!       input: alpha=right ascension [Chi Jing] of GW source; 
!              delta=declination [Chi Wei] of GW source; 
!              varphi=polarization angle of GW source;
!  alpha =phi[general]                -----> phi[general]=alpha
!  delta =theta[general]-90.0deg      -----> theta[general]=delta+90.0deg
!  varphi=-varphi[general]+270.0deg   -----> varphi[general]=-varphi+270.0deg
!
!       input: psi=North Latitute of the detector's site; 
!              lambdar=psi_r is equivalent to -L, which is the East Long.; 
!              t=Omega_r*t_star;
!              t_star is time at t(f)=tc-(5/256)*(G*Mc/c^3)**(-5/3)*(Pi*f)**(-8/3)
!                                    =tc-(Mc/M_sun)**(-5/3)*(f/Hz)**(-8/3)*(6.46209*10^6)
!              tc=t0-dL/c, but here we can ignore dL/c term
!       input: gama is measured counter-clockwise from East to the bisector of the interomater arms; 
!              zeta=angle between two arms;
!       output: F1=F_{plus}; Fx=F_{cross}; Fb=F_b; Fs=F_s; Fv1=F_{vx}; Fv2=F_{vy}
!       !响应函数计算 
!       !定义角度变量 
!       !定义响应函数变量以及文章中的响应函数变量
!       !定义变换矩阵
!       !定义探测器参考系中的Hij分量
!       !定义探测器臂的方向
!       !定义eij并赋初值
!!! note that, we have fix t=0 at which the Greenick sidereal time is zero
        double precision :: alpha, varphi, delta !! GW source
        double precision :: psi, lambda, gama, zeta !! detector    
        double precision :: tc, Mc, f, onoff
        double precision :: t, Omegar, t_star
        double precision :: F1, Fx, Fb, Fs ,Fv1, Fv2, F1a, Fxa, at, bt
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
     $                 *(644029.24687)*onoff
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
        Salpha   = alpha*deg2nat
        Svarphi  = varphi*deg2nat
        Sdelta   = delta*deg2nat
        Spsi     = psi*deg2nat
        Slambdar = lambdar*deg2nat
        Sgama    = gama*deg2nat
        Szeta    = zeta*deg2nat
!       计算变换矩阵
!       write(*, *)  alpha, varphi, delta, psi, lambdar, t, gamma, zeta
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
        M = matmul(matmul(M3, M2), transpose(M1))
!c      write(*, *) "M"
!c      do i = 1, 3
!c        write(*, *) M(i, :)
!c      end do  
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
!c      write(*, *) "F+ = ",F1
!c      write(*, *) "Fx = ",Fx
!c      write(*, *) "Fb = ",Fb
!c      write(*, *) "Fs = ",Fs
!c      write(*, *) "Fv1 = ",Fv1
!c      write(*, *) "Fv2 = ",Fv2

!       计算文章中的a，b，以及两个响应函数
!       write(*,*) t
        at=0.0
        at = 1.0/16.0*sin(2.*Sgama)*(3.-cos(2.*Spsi))
     $   *(3.-cos(2.*Sdelta))*cos(2.*(Salpha-Slambdar-t))
        at = at-1.0/4.0*cos(2.*Sgama)*sin(Spsi)
     $   *(3.-cos(2.*Sdelta))*sin(2.*(Salpha-Slambdar-t))
        at = at+1.0/4.0*sin(2.*Sgama)*sin(2.*Spsi)
     $   *sin(2.*Sdelta)*cos(Salpha-Slambdar-t)
        at = at-1.0/2.0*cos(2.*Sgama)*cos(Spsi)
     $   *sin(2.*Sdelta)*sin(Salpha-Slambdar-t)
        at = at+3.0/4.0*sin(2.*Sgama)*cos(Spsi)
     $   *cos(Spsi)*cos(Sdelta)*cos(Sdelta)
        bt=0.0
        bt = cos(2*Sgama)*sin(Spsi)*sin(Sdelta)
     $      *cos(2.*(Salpha-Slambdar-t))
        bt = bt+1.0/4.0*sin(2.*Sgama)*(3.-cos(2.*Spsi))
     $   *sin(Sdelta)*sin(2.*(Salpha-Slambdar-t))
        bt = bt+cos(2.*Sgama)*cos(Spsi)
     $   *cos(Sdelta)*cos(Salpha-Slambdar-t)
        bt = bt+1.0/2.0*sin(2.*Sgama)*sin(2.*Spsi)*cos(Sdelta)
     $       *sin(Salpha-Slambdar-t)
        F1a = sin(Szeta)*(at*cos(2.0*Svarphi)+bt*sin(2.0*Svarphi))
        Fxa = sin(Szeta)*(bt*cos(2.0*Svarphi)-at*sin(2.0*Svarphi))
!       write(*, *) "F+ in article = ",F1a
!       write(*, *) "Fx in article = ",Fxa
        end

        double complex FUNCTION h2f(f,M,eta,t_c,psi_c,iota,lnr,fLSO,
     $               Fplus,Fcross)  !unit:10^{-20}*(1/Hz)
        implicit none
        double precision :: f,M,eta,t_c,psi_c,iota,lnr,fLSO,Fplus,Fcross
        double precision :: ef,Ftheta,Fphi,Fpsi,r
        double precision :: P(9,9),phi(9,9)
        double precision :: S1,S32,S2,S52
        double precision :: piMf,Mf_r,S548PI23,stepfun,zeta
        double complex :: factor,factor1,factor2,factor3,factor4,factor5
        r=exp(lnr)
        piMf=M*f*GM3  !!!  unit: (M/M_sun)*(f/Hz)
        Mf_r=((M**(5D0/6))*sqrt(eta))*(f**(-7D0/6))/r*MG56C32pc6
     !!!!!!!!!!!!!!!!!! unit (1/Hz)*((M/M_sun)**(5/6))*(Hz**(-7/6))
        S548PI23=0.150463491371557_8
        S1=0.5*(743._8/336+11._8/4*eta)
        S32=-2*pi
        S2=7266251._8/8128512+18913._8/16128*eta+1379._8/1152*eta*eta
        S52=-pi*4757._8/1344-3._8/16*(-63+44*pi)*eta
        call PandPHI(P,phi,iota,M,eta,Fplus,Fcross)
        factor1= cmplx(cos(phi(3,1)),-sin(phi(3,1)))*P(3,1)
        factor2=(cmplx(cos(phi(3,3)),-sin(phi(3,3)))*P(3,3)
     $        +cmplx(cos(phi(3,1)),-sin(phi(3,1)))*P(3,1)*S1)
     $       *(piMf)**(2.0/3.0)
        factor3=(cmplx(cos(phi(3,4)),-sin(phi(3,4)))*P(3,4)
     $        +cmplx(cos(phi(3,1)),-sin(phi(3,1)))*P(3,1)*S32)
     $       *(piMf)
        factor4=(cmplx(cos(phi(3,5)),-sin(phi(3,5)))*P(3,5)
     $        +cmplx(cos(phi(3,3)),-sin(phi(3,3)))*P(3,3)*S1
     $      +cmplx(cos(phi(3,1)),-sin(phi(3,1)))*P(3,1)*S2)
     $       *(piMf)**(4.0/3.0)
        factor5=(cmplx(cos(phi(3,6)),-sin(phi(3,6)))*P(3,6)
     $        +cmplx(cos(phi(3,4)),-sin(phi(3,4)))*P(3,4)*S1
     $      +cmplx(cos(phi(3,3)),-sin(phi(3,3)))*P(3,3)*S32
     $      +cmplx(cos(phi(3,1)),-sin(phi(3,1)))*P(3,1)*S52)
     $       *(piMf)**(5.0/3.0)
!       factor=factor1
        factor=factor1+factor2+factor3+factor4+factor5
!       factor=cmplx(1,0)
        if(f.le.(2.0*fLSO)) then
        stepfun=1.0
        else
        stepfun=0.0
        endif
        zeta=2.0*pi*f*t_c-pi/4.0+2.0*psi(f/2.0,psi_c,M,eta,fLSO)
        h2f=1.0/sqrt(2.0)*Mf_r*S548PI23*factor*stepfun
     $  *cmplx(cos(zeta),sin(zeta))
        end
        FUNCTION psi(f,psi_c,M,eta,fLSO)
        implicit none
        double precision :: psi,f,psi_c,M,eta,fLSO
        double precision :: mathcalM
        double precision :: psi0,psi1,psi2,psi3,psi4,psi5,psi6,psi7
        double precision :: gama,lambda,theta,piMf !!!(piMf==pi*M*f)
c       mathcalM=M*(eta**(0.6))
        piMf=M*f*GM3  !!!  unit: (M/M_sun)*(f/Hz)
        gama=0.5772D0
        lambda=-1987.0D0/3080.0D0
        theta=-11831.0D0/9240.0D0
        psi0=1.0D0
        psi1=0.0D0
        psi2=(20.0D0/9.0D0)*(743.0D0/336.0D0+11.0D0/4.0D0*eta)
        psi3=-16.0D0*pi
        psi4=10.0*(3058673.0/1016064.0+5429.0/1008.0*eta
     $  +617.0/144.0*eta*eta)
        psi5=pi*(38645.0/756.0+38645.0/252.0*log(f/fLSO)
     $    -65.0/9.0*eta*(1.0+3.0*log(f/fLSO)))
        psi6=(11583231236531.0/4694215680.0-640.0*pi*pi/3.0
     $          -6848.0*gama/21.0)
     $  +eta*(-15335597827.0/3048192.0+2255.0*pi*pi/12.0
     $            -1760.0*theta/3.0+12320.0*lambda/9.0)
     $  +76055.0/1728.0*eta*eta
     $  -127825.0/1296.0*eta*eta*eta
     $  -6848.0/21.0*log(4.0*(2.0*piMf)**(1.0/3.0))
        psi7=pi*(77096675.0/254016.0+378515.0/1512.0*eta
     $  -74045.0/756.0*eta*eta)
        psi=-psi_c+3.0/(256.0*((2.0*piMf)**(5.0/3.0)*eta))*
     $  (psi0*(2.0*piMf)**(0/3.0)
     $  +psi1*(2.0*piMf)**(1/3.0)
     $  +psi2*(2.0*piMf)**(2/3.0)
     $  +psi3*(2.0*piMf)**(3/3.0)
     $  +psi4*(2.0*piMf)**(4/3.0)
     $  +psi5*(2.0*piMf)**(5/3.0)
     $  +psi6*(2.0*piMf)**(6/3.0)
     $  +psi7*(2.0*piMf)**(7/3.0))
        end
        SUBROUTINE PandPHI(P,phi,iota,M,eta,Fplus,Fcross)
        implicit none
!!!!!!!!! NOTE that, here we have revised the unit of iota to "degree"
        double precision :: P(9,9),phi(9,9)
        double precision :: iota,M,eta,Fplus,Fcross
        double precision :: Ftheta,Fphi,Fpsi
        double precision ::Cplus(9,9),Ccross(9,9),Dplus(9,9),Dcross(9,9)
        double precision :: signn,deltaM,ci,si
        integer             i,j
!        Fplus=(0.5*(1.0+(cos(Ftheta))**2)*cos(2.0*Fphi)*cos(2.0*Fpsi)
!     $ -cos(Ftheta)*sin(2.0*Fphi)*sin(2.0*Fpsi))*(sqrt(3.0)/2.0)
!       Fcross=(0.5*(1.0+(cos(Ftheta))**2)*cos(2.0*Fphi)*sin(2.0*Fpsi)
!     $ +cos(Ftheta)*sin(2.0*Fphi)*cos(2.0*Fpsi))*(sqrt(3.0)/2.0)
        do i=1,9
        do j=1,9
        Cplus(i,j)=0.0
        Dplus(i,j)=0.0
        Ccross(i,j)=0.0
        Dcross(i,j)=0.0
        enddo
        enddo
!!!!!!! s=0  !!! NOTE: i=n+1; j=s*2+1
        ci=cos(iota*deg2nat)
        si=sin(iota*deg2nat)
        Cplus(1,1)=-1.0/96*si*si*(17.0+ci*ci)
        Cplus(3,1)=-(1.0+ci*ci)
                             !cross
        Dcross(3,1)=-2.0*ci
!!!!!!! s=0.5
        deltaM=-1.0*sqrt(1.0-4.0*eta)
        Cplus(2,2)=-si*deltaM*(5.0/8+1.0/8*ci*ci)
        Cplus(4,2)=si*deltaM*(9.0/8+9.0/8*ci*ci)
                             !cross
        Dcross(2,2)=-3.0/4*si*ci*deltaM
        Dcross(4,2)=9.0/4*si*ci*deltaM
!!!!!!! s=1
        Cplus(3,3)=19.0/6+3.0/2*ci*ci-1.0/3*ci*ci*ci*ci
     $        +eta*(-19.0/6+11.0/6*ci*ci+ci*ci*ci*ci)
        Cplus(5,3)=-4.0/3*si*si*(1.0+ci*ci)*(1.0-3.0*eta)
                             !cross
        Dcross(3,3)=ci*(17.0/3-4.0/3*ci*ci+eta*(-13.0/3+4.0*ci*ci))
        Dcross(5,3)=ci*si*si*(-8.0/3*(1.0-3.0*eta))
!!!!!!! s=1.5
        Cplus(2,4)=si*deltaM*(19.0/64+5.0/16*ci*ci-1.0/192*ci*ci*ci*ci
     $           +eta*(-49.0/96+1.0/8*ci*ci+1.0/96*ci*ci*ci*ci))
        Cplus(3,4)=-2.0*pi*(1.0+ci*ci)
        Cplus(4,4)=si*deltaM*(-657.0/128-45.0/16*ci*ci+81.0/128*(ci**4)
     $           +eta*(225.0/64-9.0/8*ci*ci-81.0/64*ci*ci*ci*ci))
        Cplus(6,4)=si*deltaM*(625.0/384*si*si*(1.0+ci*ci)*(1.0-2.0*eta))
                             !cross
        Dcross(2,4)=si*ci*deltaM*(21.0/32-5.0/96*ci*ci
     $           +eta*(-23.0/48+5.0/48*ci*ci))
        Dcross(3,4)=-4.0*pi*ci
        Dcross(4,4)=si*ci*deltaM*(-603.0/64+135.0/64*ci*ci
     $          +eta*(171.0/32-135.0/32*ci*ci))
        Dcross(6,4)=si*ci*deltaM*(625.0/192*(1.0-2.0*eta)*si*si)
!!!!!!! s=2
        Cplus(2,5)=pi*si*deltaM*(-5.0/8-1.0/8*ci*ci)
       Cplus(3,5)=11.0/60+33.0/10*(ci**2)+29.0/24*(ci**4)-1.0/24*(ci**6)
     $       +eta*(353.0/36-3.0*(ci**2)-251.0/72*(ci**4)+5.0/24*(ci**6))
     $   +eta*eta*(-49.0/12+9.0/2*(ci**2)-7.0/24*(ci**4)-5.0/24*(ci**6))
        Cplus(4,5)=pi*si*deltaM*(27.0/8*(1.0+ci*ci))
        Cplus(5,5)=2.0/15*si*si*(59.0+35.0*(ci**2)-8.0*(ci**4)
     $          -5.0/3*eta*(131.0+59.0*(ci**2)-24.0*(ci**4))
     $        +5.0*eta*eta*(21.0-3.0*(ci**2)-8.0*(ci**4)))
       Cplus(7,5)=-81.0/40*(si**4)*(1.0+ci*ci)*(1.0-5.0*eta+5.0*eta*eta)
        Dplus(2,5)=si*deltaM*(11.0/40+5.0*log(2.0)/4.0
     $                    +ci*ci*(7.0/40+log(2.0)/4))
        Dplus(4,5)=si*deltaM*(-189.0/40+27.0/4*log(1.5))*(1.0+ci*ci)
                             !cross
        Ccross(2,5)=si*ci*deltaM*(-9.0/20-3.0/2*log(2.0))
        Ccross(4,5)=si*ci*deltaM*(189.0/20-27.0/2*log(1.5))
        Dcross(2,5)=-si*ci*deltaM*(3.0*pi/4)
        Dcross(3,5)=ci*(17.0/15+113.0/30*ci*ci-1.0/4*(ci**4)
     $           +eta*(143.0/9-245.0/18*ci*ci+5.0/4*(ci**4))
     $     +eta*eta*(-14.0/3+35.0/6*ci*ci-5.0/4*(ci**4)))
        Dcross(4,5)=si*ci*deltaM*(27.0*pi/4)
        Dcross(5,5)=4.0/15*ci*si*si*(55.0-12.0*ci*ci
     $           -5.0/3*eta*(119.0-36*ci*ci)
     $           +5.0*eta*eta*(17.0-12.0*ci*ci))
        Dcross(7,5)=ci*(-81.0/20*(si**4)*(1.0-5.0*eta+5.0*eta*eta))
!!!!!!! s=2.5
        Cplus(2,6)=si*deltaM*(1771.0/5120-1667.0/5120*ci*ci
     $                  +217.0/9216*(ci**4)-1.0/9126*(ci**6)
     $          +eta*(681.0/256+13.0/768*ci*ci-35.0/768*(ci**4)
     $                    +1.0/2304*(ci**6))
     $          +eta*eta*(-3451.0/9216+673.0/3072*ci*ci
     $                    -5.0/9216*(ci**4)-1.0/3072*(ci**6)))
        Cplus(3,6)=pi*(19.0/3+3.0*ci*ci-2.0/3*(ci**4)
     $               +eta*(-16.0/3+14.0/3*ci*ci+2.0*(ci**4)))
        Cplus(4,6)=si*deltaM*(3537.0/1024-22977.0/5120*ci*ci
     $                  -15309.0/5120*(ci**4)+729.0/5120*(ci**6)
     $        +eta*(-23829.0/1280+5529.0/1280*ci*ci
     $                     +7749.0/1280*(ci**4)-729.0/1280*(ci**6))
     $          +eta*eta*(29127.0/5120-27267.0/5120*ci*ci
     $                     -1647.0/5120*(ci**4)+2187.0/5120*(ci**6)))
        Cplus(5,6)=-16.0*pi/3.0*(1.0+ci*ci)*si*si*(1.0-3.0*eta)
        Cplus(6,6)=si*deltaM*(-108125.0/9216+40625.0/9216*ci*ci
     $                     +83125.0/9216*(ci**4)-15625.0/9216*(ci**6)   
     $          +eta*(8125.0/256-40625.0/2304*ci*ci
     $                     -48125.0/2304*(ci**4)+15625.0/2304*(ci**6))
     $          +eta*eta*(-119375.0/9216+40625.0/3072*ci*ci
     $                     +44375.0/9216*(ci**4)-15625.0/3072*(ci**6)))
        Cplus(8,6)=deltaM*(117649.0/46080*(si**5)*(1.0+ci*ci)
     $                  *(1.0-4.0*eta+3.0*eta*eta))
        Dplus(3,6)=-9.0/5+14.0/5*ci*ci+7.0/5*(ci**4)
     $          +eta*(32.0+56.0/5*ci*ci-28.0/5*(ci**4))
        Dplus(5,6)=si*si*(1.0+ci*ci)*(56.0/5-32.0*log(2.0)/3.0
     $                                  +eta*(-1193.0/30+32.0*log(2.0)))
                             !cross
        Ccross(1,6)=6.0/5*si*si*ci*eta
        Ccross(3,6)=ci*(2.0-22.0/5*ci*ci+eta*(-282.0/5+94.0/5*ci*ci))
        Ccross(5,6)=ci*si*si*(-112.0/5+64.0/3*log(2.0)
     $          +eta*(1193.0/15-64.0*log(2.0)))
        Dcross(2,6)=si*ci*deltaM*(-913.0/7680+1891.0/11520*ci*ci
     $                          -7.0/4608*(ci**4)
     $          +eta*(1165.0/384-235.0/576*ci*ci+7.0/1152*(ci**4))
     $          +eta*eta*(-1301.0/4608+301.0/2304*ci*ci
     $                    -7.0/1536*(ci**4)))
        Dcross(3,6)=pi*ci*(34.0/3-8.0/3*ci*ci+eta*(-20.0/3+8.0*ci*ci))
        Dcross(4,6)=si*ci*deltaM*(12501.0/2560-12069.0/1280*ci*ci
     $                    +1701.0/2560*(ci**4)
     $          +eta*(-19581.0/640+7821.0/320*ci*ci
     $                    -1701.0/640*(ci**4))
     $          +eta*eta*(18903.0/2560-11403.0/1280*ci*ci
     $                    +5103.0/2560*(ci**4)))
        Dcross(5,6)=si*si*ci*(-32.0*pi/3*(1.0-3.0*eta))
        Dcross(6,6)=deltaM*si*ci*(-101875.0/4608+6875.0/256*ci*ci
     $                    -21875.0/4608*(ci**4)
     $          +eta*(66875.0/1152-44375.0/576*ci*ci
     $                    +21875.0/1152*(ci**4))
     $          +eta*eta*(-100625.0/4608+83125.0/2304*ci*ci
     $                    -21875.0/1536*(ci**4)))
        Dcross(8,6)=deltaM*(si**5)*ci
     $           *(117649.0/23040*(1.0-4.0*eta+3.0*eta*eta))
!!!!!!! s=3
        Cplus(2,7)=0
        Cplus(3,7)=0
        Cplus(4,7)=0
        Cplus(5,7)=0
        Cplus(7,7)=0
        Cplus(2,7)=0
        Cplus(3,7)=0
        Cplus(4,7)=0
        Cplus(5,7)=0
        Cplus(7,7)=0
        Dplus(2,7)=0
        Dplus(4,7)=0
        Dplus(2,7)=0
        Dplus(4,7)=0
                             !cross
        Ccross(2,7)=0
        Ccross(4,7)=0
        Ccross(2,7)=0
        Ccross(4,7)=0
        Dcross(2,7)=0
        Dcross(3,7)=0
        Dcross(4,7)=0
        Dcross(5,7)=0
        Dcross(7,7)=0
        Dcross(2,7)=0
        Dcross(3,7)=0
        Dcross(4,7)=0
        Dcross(5,7)=0
        Dcross(7,7)=0
        do i=1,9
        do j=1,9
                if((Cplus(i,j)*Fplus+Ccross(i,j)*Fcross).ge.0.0) then
                signn=1.0
                else
                signn=-1.0
                endif
        P(i,j)=signn*
     $    sqrt((Cplus(i,j)*Fplus+Ccross(i,j)*Fcross)**2
     $        +(Dplus(i,j)*Fplus+Dcross(i,j)*Fcross)**2)
        phi(i,j)=0.0
        phi(i,j)=atan(-(Dplus(i,j)*Fplus+Dcross(i,j)*Fcross)
     $              /(Cplus(i,j)*Fplus+Ccross(i,j)*Fcross+1D-8))
        enddo
        enddo
!       write(*, *) Cplus(3, 6), Ccross(3, 6), Cplus(3, 4), Ccross(3, 4)
!       write(*, *) Dplus(3, 6), Dcross(3, 6), Dplus(3, 4), Dcross(3, 4)
!       write(*, *)
        end
        subroutine t_c_Func(qt0, t_c)
           implicit none
           integer :: qt0(1:6), num
           double precision :: t_c
           double precision :: yr, month, day, hr, mmin, sec
           double precision :: UT, aa, bb, JD, ST, t, omegar
           yr    = 1.0D0 * qt0(1)
           month = 1.0D0 * qt0(2)
           day   = 1.0D0 * qt0(3)
           hr    = 1.0D0 * qt0(4)
           mmin  = 1.0D0 * qt0(5)
           sec   = 1.0D0 * qt0(6)
           if ( month .LE. 2 ) then
              month = month + 12.0D0
              yr = yr - 1.0D0
           end if
           UT = hr + mmin / 60.0D0 + sec / 3600.0D0
           aa = 1.0D0 * floor(1.0D0 * yr / 100.0D0)
           bb = 2.0D0 - aa + 1.0D0 * floor(1.0D0 * aa /4.0D0)
           JD = 1.0D0 * floor(365.25D0 * (yr + 4716.0D0))
     $       + 1.0D0 * floor(30.6001D0 * (month + 1.0D0))
     $       + day + bb - 1524.5D0 + 1.0D0 * UT / 24.0D0
           ST = (JD - 2451545.0D0) / 36525.0D0
           t  = 280.46061837D0 + 360.98564736629D0 * (JD - 2451545.0D0)
     $       + 0.000387933D0 * (ST**2) - 1.0D0 * (ST**3) / 38710000.0D0
           omegar = (2.0D0 * PI) / (24.0D0 * 3600.0D0)
           num = t / 360
           t_c = t - num * 360.0D0
           if(t_c .LT. 0) then
              t_c = t_c + 360.0D0
           end if
           t_c = t_c * PI / 180.0D0
           t_c = 1.0D0 * t_c / omegar
        end
        subroutine Delta_Omega(Fij,delta,DO1,DO2,DO3,DlndL) !! D1, D2 with unit: deg^2
        implicit none
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
        double precision :: sd1,sd2,sd3
        double precision :: nat2deg_sq
        double precision :: DlndL
        integer          :: i,j,k
        nat2deg_sq=(180.0/PI)**2
        theta = (delta+90.0)*deg2nat !!!! theta=delta+Pi/2  or delta=theta-Pi/2
!!!!!!!! calculate D1
! in the stationary case, (combination of four angles) have the degenracy with t_c
!       do i=1,9
!       Fij(i,1)=0.0
!       Fij(1,i)=0.0
!       Fij(i,2)=0.0
!       Fij(2,i)=0.0
!       Fij(i,3)=0.0
!       Fij(3,i)=0.0
!       Fij(i,4)=0.0
!       Fij(4,i)=0.0
!       enddo
!       Fij(1,1)=1.0
!       Fij(2,2)=1.0
!       Fij(3,3)=1.0
!       Fij(4,4)=1.0
        
        n=9
        call INVERSEMATRIX(Cov,Fij,n)
!       write(*, *) Cov(1, 1), Cov(1, 2), Cov(2, 2)
        sd1=(Cov(1,1)*Cov(2,2)-Cov(1,2)*Cov(1,2))!*(deg2nat**4) 
        DO1=(2.0*PI)*abs(sin(theta))*sqrt(sd1)!*nat2deg_sq !!!!!!!!!!!!!cos(theta)------>sin(theta)
        DlndL=sqrt(Cov(7,7))
!       do i=1,9
!       write(*,*) i,sqrt(Cov(i,i))
!       enddo
!       write(*,*) 'Delta_dL is', sqrt(Cov(7,7)), 
!     $             1.0/sqrt(Fij(7,7)), log(1000.0)
!!!!!!!! calculate D2
        n2=3
!       do i=1,n2
!       do j=1,n2
!       Fij2(i,j)=Fij(i,j)
!       enddo
!       enddo   
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
        DO3=(2.0*PI)*abs(sin(theta))*sqrt(sd3)!*nat2deg_sq !!!!!!!!!!cos(theta)------>sin(theta)
        end


        end module sub

