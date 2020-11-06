module Fisher
      use fiducial
      implicit none
      contains
              subroutine Fisher_LIGO_general(psi,lambda,gama,zeta,para,fbase,Nbase,imax,Fij,SNR)
                      implicit none
                      integer :: i,j,k
                      integer :: ith(0:9)=1
                      integer :: turn_on_off
                      integer :: imax
                      real(8) :: deltaef,ef
                      real(8) :: psi,lambda,gama,zeta
                      real(8) :: para(9),para2(9),step(9)
                      real(8) :: fbase(imax),Nbase(imax),Fij(9,9)
                      real(8) :: SNR,SNR2
                      real(8) :: FLSO,f
                      complex(8) :: dihat(0:9),D_dihat(9)


                      step=(/1D-3,1D-3,1D-3,1D-3,1D-5,1D-4,1D-3,1D-5,1D-6/)
                      para2=para+step
                      turn_on_off=1
                      FLSO=2198.64_8/para(8)!Hz
                      deltaef=0.001_8
                      Fij=0.
                      SNR2=0.
                      do ef=log(10.001_8),log(2.*FLSO),deltaef
                      f=exp(ef)
                      call dihat_general(psi,lambda,gama,zeta,para(1),para(2),para(3),para(4),para(8),para(9),para(7),para(5),para(6),f,FLSO,fbase,Nbase,ith(0),imax,turn_on_off,dihat(0))
                      call dihat_general(psi,lambda,gama,zeta,para2(1),para(2),para(3),para(4),para(8),para(9),para(7),para(5),para(6),f,FLSO,fbase,Nbase,ith(1),imax,turn_on_off,dihat(1))
                      call dihat_general(psi,lambda,gama,zeta,para(1),para2(2),para(3),para(4),para(8),para(9),para(7),para(5),para(6),f,FLSO,fbase,Nbase,ith(2),imax,turn_on_off,dihat(2))
                      call dihat_general(psi,lambda,gama,zeta,para(1),para(2),para2(3),para(4),para(8),para(9),para(7),para(5),para(6),f,FLSO,fbase,Nbase,ith(3),imax,turn_on_off,dihat(3))
                      call dihat_general(psi,lambda,gama,zeta,para(1),para(2),para(3),para2(4),para(8),para(9),para(7),para(5),para(6),f,FLSO,fbase,Nbase,ith(4),imax,turn_on_off,dihat(4))
                      call dihat_general(psi,lambda,gama,zeta,para(1),para(2),para(3),para(4),para2(8),para(9),para(7),para(5),para(6),f,FLSO,fbase,Nbase,ith(8),imax,turn_on_off,dihat(8))
                      call dihat_general(psi,lambda,gama,zeta,para(1),para(2),para(3),para(4),para(8),para2(9),para(7),para(5),para(6),f,FLSO,fbase,Nbase,ith(9),imax,turn_on_off,dihat(9))
                      call dihat_general(psi,lambda,gama,zeta,para(1),para(2),para(3),para(4),para(8),para(9),para(7),para2(5),para(6),f,FLSO,fbase,Nbase,ith(5),imax,turn_on_off,dihat(5))
                      call dihat_general(psi,lambda,gama,zeta,para(1),para(2),para(3),para(4),para(8),para(9),para(7),para(5),para2(6),f,FLSO,fbase,Nbase,ith(6),imax,turn_on_off,dihat(6))
                      call dihat_general(psi,lambda,gama,zeta,para(1),para(2),para(3),para(4),para(8),para(9),para2(7),para(5),para(6),f,FLSO,fbase,Nbase,ith(7),imax,turn_on_off,dihat(7))
                      do i=1,9
                              D_dihat(i)=(dihat(i)-dihat(0))/step(i)
                      enddo
                      do i=1,9
                      do j=1,i
                                      Fij(i,j)=Fij(i,j)+4*f*deltaef*(real(D_dihat(i))*real(D_dihat(j))+aimag(D_dihat(i))*aimag(D_dihat(j)))
                                      Fij(j,i)=Fij(i,j)
                              enddo
                      enddo
                      SNR2=SNR2+4*f*deltaef*(real(dihat(0))**2+aimag(dihat(0))**2)
                      enddo
                      SNR=sqrt(SNR2)
              end subroutine Fisher_LIGO_general

              subroutine dihat_general(psi,lambda,gama,zeta,alpha,delta,varphi,iota,M,eta,lndL,t_c,psi_c,f,FLSO,fbase,Nbase,ith,imax,turn_on_off,di_hat)
                      implicit none
                      real(8) :: psi,lambda,gama,zeta
                      real(8) :: alpha,delta,varphi,iota,M,eta,t_c,psi_c,lndL
                      real(8) :: f,FLSO,fbase(imax),Nbase(imax)
                      complex(8) :: di_hat
                      integer :: ith,imax,turn_on_off
                      real(8) :: Mc,dL,onoff
                      real(8) :: s_fit
                      real(8) :: xn,yn,zn,xr,yr,zr
                      real(8) :: Fb,Fs,Fv1,Fv2
                      real(8) :: nroverc,Fplus,Fcross
                      complex(8) :: term1,term3
                      real(8) :: term2

                      !! term1 !!
                      Mc=M*(eta**0.6_8)
                      onoff=1.
                      call n_hat(alpha,delta,xn,yn,zn)
                      call rI(lambda,psi,t_c,Mc,f,onoff,xr,yr,zr)
                      nroverc=(xn*xr+yn*yr+zn*zr)/c0
                      term1=exp(cj*(2*PI*f*nroverc))

                      !! term2 !!
                      if(turn_on_off .EQ. 1)then
                              call noise(s_fit,f,fbase,Nbase,ith,imax)
                              term2=1/sqrt(s_fit)
                      else
                              term2=1.
                      endif
                      !! term3 !!
                      call Pat_Func(alpha,delta,varphi,psi,lambda,gama,zeta,t_c,Mc,f,onoff,Fplus,Fcross,Fb,Fs,Fv1,Fv2)
                      dL=exp(lndL)
                      term3=h2f(f,M,eta,t_c,psi_c,iota,lndL,FLSO,Fplus,Fcross)
                      di_hat=term1*term2*term3
              end subroutine dihat_general

              subroutine rI(lambda,psi,tc,Mc,f,onoff,x,y,z)
                      implicit none
                      real(8) :: lambda,psi,tc,Mc,f,x,y,z,onoff
                      real(8) :: R_earth,theta,phi,Omegar,lambdar,t

                      R_earth=(6378.137_8+6356.752_8)/2!km
                      theta=(90-psi)*deg2nat
                      Omegar=PI/(12*3600) !s^-1
                      t=tc-(Mc**(-5._8/3))*(f**(-8._8/3))*(646029.24687_8)*onoff
                      phi=lambda*deg2nat+Omegar*t
                      x=R_earth*sin(theta)*cos(phi)
                      y=R_earth*sin(theta)*sin(phi)
                      z=R_earth*cos(theta)
              end subroutine rI

              subroutine n_hat(alpha,delta,x,y,z)
                      implicit none
                      real(8) :: alpha,delta,x,y,z
                      real(8) :: theta,phi
                      theta=(90-delta)*deg2nat
                      phi  =alpha*deg2nat
                      x=sin(theta)*cos(phi)
                      y=sin(theta)*sin(phi)
                      z=cos(theta)
              end subroutine n_hat

              subroutine Pat_Func(alpha,delta,varphi,psi,lambda,gama,zeta,tc,Mc,f,onoff,F1,Fx,Fb,Fs,Fv1,Fv2)
                      implicit none
                      real(8) :: alpha,delta,varphi
                      real(8) :: psi,lambda,gama,zeta
                      real(8) :: tc,Mc,f,onoff
                      real(8) :: t,Omegar,t_star
                      real(8) :: F1,Fx,Fb,Fs,Fv1,Fv2,F1a,Fxa,at,bt
                      real(8) :: M1(3,3),M2(3,3),M3(3,3),M(3,3)
                      real(8) :: H1(3,3),Hx(3,3),Hb(3,3),Hs(3,3),Hv1(3,3),Hv2(3,3)
                      real(8) :: n1(3),n2(3),e1(3,3),ex(3,3),eb(3,3),es(3,3),ev1(3,3),ev2(3,3)
                      integer :: i,j,k
                      real(8) :: lambdar
                      real(8) :: Salpha,Spsi,Svarphi
                      real(8) :: Slambdar,Sgama,Szeta,Sdelta
                      Omegar=PI/(12*3600)
                      t_star=tc-(Mc**(-5._8/3))*(f**(-8._8/3))*(646029.24687_8)*onoff
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

                !       计算变换矩阵
                      Salpha=alpha*deg2nat
                      Svarphi=varphi*deg2nat
                      Sdelta=delta*deg2nat
                      Spsi=psi*deg2nat
                      Slambdar=lambda*deg2nat
                      Sgama=gama*deg2nat
                      Szeta=zeta*deg2nat

                      M3(1,1)=-sin(Sgama+Szeta/2)
                      M3(1,2)=cos(Sgama+Szeta/2)
                      M3(1,3)=0.
                      M3(2,1)=-cos(Sgama+Szeta/2)
                      M3(2,2)=-sin(Sgama+Szeta/2)
                      M3(2,3)=0.
                      M3(3,1)=0.
                      M3(3,2)=0.
                      M3(3,3)=1.

                      M2(1,1)=sin(Spsi)*cos(Slambdar+t)
                      M2(1,2)=sin(Spsi)*sin(Slambdar+t)
                      M2(1,3)=-cos(Spsi)
                      M2(2,1)=-sin(Slambdar+t)
                      M2(2,2)=cos(Slambdar+t)
                      M2(2,3)=0.
                      M2(3,1)=cos(Spsi)*cos(Slambdar+t)
                      M2(3,2)=cos(Spsi)*sin(Slambdar+t)
                      M2(3,3)=sin(Spsi)

                      
                      M1(1,1)=sin(Salpha)*cos(Svarphi)-cos(Salpha)*sin(Sdelta)*sin(Svarphi)
                      M1(1,2)=-cos(Salpha)*cos(Svarphi)-sin(Salpha)*sin(Sdelta)*sin(Svarphi)
                      M1(1,3)=cos(Sdelta)*sin(Svarphi)
                      M1(2,1)=-sin(Salpha)*sin(Svarphi)-cos(Salpha)*sin(Sdelta)*cos(Svarphi)
                      M1(2,2)=cos(Salpha)*sin(Svarphi)-sin(Salpha)*sin(Sdelta)*cos(Svarphi)
                      M1(2,3)=cos(Sdelta)*cos(Svarphi)
                      M1(3,1)=-cos(Salpha)*cos(Sdelta)
                      M1(3,2)=-sin(Salpha)*cos(Sdelta)
                      M1(3,3)=-sin(Sdelta)

                      n1(1) = cos(Szeta/2.0-PI/4.0)
                      n1(2) = sin(Szeta/2.0-PI/4.0)
                      n1(3) = 0.0
                      n2(1) = cos(-Szeta/2.0+3.0/4.0*PI)
                      n2(2) = sin(-Szeta/2.0+3.0/4.0*PI)
                      n2(3) = 0.0

                !       计算M并计算响应函数
                      M = matmul(matmul(M3, M2), transpose(M1))
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

                !       计算文章中的a，b，以及两个响应函数
                      at=0.0
                      at = 1.0/16.0*sin(2.*Sgama)*(3.-cos(2.*Spsi))*(3.-cos(2.*Sdelta))*cos(2.*(Salpha-Slambdar-t))
                      at = at-1.0/4.0*cos(2.*Sgama)*sin(Spsi)*(3.-cos(2.*Sdelta))*sin(2.*(Salpha-Slambdar-t))
                      at = at+1.0/4.0*sin(2.*Sgama)*sin(2.*Spsi)*sin(2.*Sdelta)*cos(Salpha-Slambdar-t)
                      at = at-1.0/2.0*cos(2.*Sgama)*cos(Spsi)*sin(2.*Sdelta)*sin(Salpha-Slambdar-t)
                      at = at+3.0/4.0*sin(2.*Sgama)*cos(Spsi)*cos(Spsi)*cos(Sdelta)*cos(Sdelta)
                      bt = cos(2*Sgama)*sin(Spsi)*sin(Sdelta)*cos(2.*(Salpha-Slambdar-t))
                      bt = bt+1.0/4.0*sin(2.*Sgama)*(3.-cos(2.*Spsi))*sin(Sdelta)*sin(2.*(Salpha-Slambdar-t))
                      bt = bt+cos(2.*Sgama)*cos(Spsi)*cos(Sdelta)*cos(Salpha-Slambdar-t)
                      bt = bt+1.0/2.0*sin(2.*Sgama)*sin(2.*Spsi)*cos(Sdelta)*sin(Salpha-Slambdar-t)
                      F1a = sin(Szeta)*(at*cos(2.0*Svarphi)+bt*sin(2.0*Svarphi))
                      Fxa = sin(Szeta)*(bt*cos(2.0*Svarphi)-at*sin(2.0*Svarphi))
              end subroutine Pat_Func
              
              complex(8) FUNCTION h2f(f,M,eta,t_c,psi_c,iota,lnr,fLSO,Fplus,Fcross)  !unit:10^{-20}*(1/Hz)
                      implicit none
        double precision :: f,M,eta,t_c,psi_c,iota,lnr,fLSO,Fplus,Fcross
        double precision :: ef,Ftheta,Fphi,Fpsi,r
        double precision :: P(9,9),phi(9,9)
        double precision :: S1,S32,S2,S52
        double precision :: piMf,mathcalM56f76r,S548PI23,stepfun,zeta
        double complex   :: factor,factor1,factor2,factor3,factor4,factor5
        r=exp(lnr)
        piMf=M*f*(1.54735/100000.0)  !!!  unit: (M/M_sun)*(f/Hz)
        mathcalM56f76r=((M**(5.0/6))*sqrt(eta))*(f**(-7.0/6))/r*36.6868
     !!!!!!!!!!!!!!!!!! unit 10**(-20)*(1/Hz)*((M/M_sun)**(5/6))*(Hz**(-7/6))
        S548PI23=0.150463
        S1=0.5*(743.0/336.0+11.0/4.0*eta)
        S32=-2.0*pi
        S2=7266251.0/8128512.0+18913.0/16128.0*eta+1379.0/1152.0*eta*eta
        S52=-pi*4757.0/1344.0-3.0/16.0*(-63.0+44.0*pi)*eta
        call PandPHI(P,phi,iota,M,eta,Fplus,Fcross)
        factor1= cmplx(cos(phi(3,1)),-sin(phi(3,1)))*P(3,1)
        factor2=(cmplx(cos(phi(3,3)),-sin(phi(3,3)))*P(3,3)+cmplx(cos(phi(3,1)),-sin(phi(3,1)))*P(3,1)*S1)*(piMf)**(2.0/3.0)
        factor3=(cmplx(cos(phi(3,4)),-sin(phi(3,4)))*P(3,4)+cmplx(cos(phi(3,1)),-sin(phi(3,1)))*P(3,1)*S32)*(piMf)
        factor4=(cmplx(cos(phi(3,5)),-sin(phi(3,5)))*P(3,5)+cmplx(cos(phi(3,3)),-sin(phi(3,3)))*P(3,3)*S1+cmplx(cos(phi(3,1)),-sin(phi(3,1)))*P(3,1)*S2)*(piMf)**(4.0/3.0)
        factor5=(cmplx(cos(phi(3,6)),-sin(phi(3,6)))*P(3,6)+cmplx(cos(phi(3,4)),-sin(phi(3,4)))*P(3,4)*S1+cmplx(cos(phi(3,3)),-sin(phi(3,3)))*P(3,3)*S32\
        +cmplx(cos(phi(3,1)),-sin(phi(3,1)))*P(3,1)*S52)*(piMf)**(5.0/3.0)
!       factor=factor1
        factor=factor1+factor2+factor3+factor4+factor5
!       factor=cmplx(1,0)
        if(f.le.(2.0*fLSO)) then
        stepfun=1.0
        else
        stepfun=0.0
        endif

        zeta=2.0*pi*f*t_c-pi/4.0+2.0*psi(f/2.0,psi_c,M,eta,fLSO)
        h2f=1.0/sqrt(2.0)*mathcalM56f76r*S548PI23*factor*stepfun*cmplx(cos(zeta),sin(zeta))
        end function h2f

              FUNCTION psi(f,psi_c,M,eta,fLSO)
                      implicit none
                      double precision :: psi,f,psi_c,M,eta,fLSO
                      double precision :: mathcalM
                      double precision :: psi0,psi1,psi2,psi3,psi4,psi5,psi6,psi7
                      double precision :: gama,lambda,theta,piMf !!!(piMf==pi*M*f)
                  !    mathcalM=M*(eta**(0.6))
                      piMf=M*f*(1.54735D0/100000.0D0)  !!!  unit: (M/M_sun)*(f/Hz)
                      gama=0.5772D0
                      lambda=-1987.0D0/3080.0D0
                      theta=-11831.0D0/9240.0D0
                      psi0=1.0D0
                      psi1=0.0D0
                      psi2=(20.0D0/9.0D0)*(743.0D0/336.0D0+11.0D0/4.0D0*eta)
                      psi3=-16.0D0*pi
                      psi4=10.0*(3058673.0/1016064.0+5429.0/1008.0*eta+617.0/144.0*eta*eta)
                      psi5=pi*(38645.0/756.0+38645.0/252.0*log(f/fLSO)-65.0/9.0*eta*(1.0+3.0*log(f/fLSO)))
                      psi6=(11583231236531.0/4694215680.0-640.0*pi*pi/3.0-6848.0*gama/21.0)+eta*(-15335597827.0/3048192.0+2255.0*pi*pi/12.0\
                      -1760.0*theta/3.0+12320.0*lambda/9.0)+76055.0/1728.0*eta*eta-127825.0/1296.0*eta*eta*eta-6848.0/21.0*log(4.0*(2.0*piMf)**(1.0/3.0))
                      psi7=pi*(77096675.0/254016.0+378515.0/1512.0*eta-74045.0/756.0*eta*eta)
                      psi=-psi_c+3.0/(256.0*((2.0*piMf)**(5.0/3.0)*eta))*(psi0*(2.0*piMf)**(0/3.0)+psi1*(2.0*piMf)**(1/3.0)+psi2*(2.0*piMf)**(2/3.0)\
                      +psi3*(2.0*piMf)**(3/3.0)+psi4*(2.0*piMf)**(4/3.0)+psi5*(2.0*piMf)**(5/3.0)+psi6*(2.0*piMf)**(6/3.0)+psi7*(2.0*piMf)**(7/3.0))
              end function psi

        SUBROUTINE PandPHI(P,phi,iota,M,eta,Fplus,Fcross)
        implicit none
!!!!!!!!! NOTE that, here we have revised the unit of iota to "degree"
        double precision :: P(9,9),phi(9,9)
        double precision :: iota,M,eta,Fplus,Fcross
        double precision :: Ftheta,Fphi,Fpsi
        double precision :: Cplus(9,9),Ccross(9,9),Dplus(9,9),Dcross(9,9)
        double precision :: signn,deltaM,ci,si
        integer i,j
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
        ci=cos(iota*(PI/180.0))
        si=sin(iota*(PI/180.0))
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
        Cplus(3,3)=19.0/6+3.0/2*ci*ci-1.0/3*ci*ci*ci*ci+eta*(-19.0/6+11.0/6*ci*ci+ci*ci*ci*ci)
        Cplus(5,3)=-4.0/3*si*si*(1.0+ci*ci)*(1.0-3.0*eta)
                             !cross
        Dcross(3,3)=ci*(17.0/3-4.0/3*ci*ci+eta*(-13.0/3+4.0*ci*ci))
        Dcross(5,3)=ci*si*si*(-8.0/3*(1.0-3.0*eta))
!!!!!!! s=1.5
        Cplus(2,4)=si*deltaM*(19.0/64+5.0/16*ci*ci-1.0/192*ci*ci*ci*ci+eta*(-49.0/96+1.0/8*ci*ci+1.0/96*ci*ci*ci*ci))
        Cplus(3,4)=-2.0*pi*(1.0+ci*ci)
        Cplus(4,4)=si*deltaM*(-657.0/128-45.0/16*ci*ci+81.0/128*(ci**4)+eta*(225.0/64-9.0/8*ci*ci-81.0/64*ci*ci*ci*ci))
        Cplus(6,4)=si*deltaM*(625.0/384*si*si*(1.0+ci*ci)*(1.0-2.0*eta))
                             !cross
        Dcross(2,4)=si*ci*deltaM*(21.0/32-5.0/96*ci*ci+eta*(-23.0/48+5.0/48*ci*ci))
        Dcross(3,4)=-4.0*pi*ci
        Dcross(4,4)=si*ci*deltaM*(-603.0/64+135.0/64*ci*ci+eta*(171.0/32-135.0/32*ci*ci))
        Dcross(6,4)=si*ci*deltaM*(625.0/192*(1.0-2.0*eta)*si*si)
!!!!!!! s=2
        Cplus(2,5)=pi*si*deltaM*(-5.0/8-1.0/8*ci*ci)
        Cplus(3,5)=11.0/60+33.0/10*(ci**2)+29.0/24*(ci**4)-1.0/24*(ci**6)+eta*(353.0/36-3.0*(ci**2)-251.0/72*(ci**4)+5.0/24*(ci**6))\
        +eta*eta*(-49.0/12+9.0/2*(ci**2)-7.0/24*(ci**4)-5.0/24*(ci**6))
        Cplus(4,5)=pi*si*deltaM*(27.0/8*(1.0+ci*ci))
        Cplus(5,5)=2.0/15*si*si*(59.0+35.0*(ci**2)-8.0*(ci**4)-5.0/3*eta*(131.0+59.0*(ci**2)-24.0*(ci**4))+5.0*eta*eta*(21.0-3.0*(ci**2)-8.0*(ci**4)))
        Cplus(7,5)=-81.0/40*(si**4)*(1.0+ci*ci)*(1.0-5.0*eta+5.0*eta*eta)
        Dplus(2,5)=si*deltaM*(11.0/40+5.0*log(2.0)/4.0+ci*ci*(7.0/40+log(2.0)/4))
        Dplus(4,5)=si*deltaM*(-189.0/40+27.0/4*log(1.5))*(1.0+ci*ci)
                             !cross
        Ccross(2,5)=si*ci*deltaM*(-9.0/20-3.0/2*log(2.0))
        Ccross(4,5)=si*ci*deltaM*(189.0/20-27.0/2*log(1.5))
        Dcross(2,5)=-si*ci*deltaM*(3.0*pi/4)
        Dcross(3,5)=ci*(17.0/15+113.0/30*ci*ci-1.0/4*(ci**4)+eta*(143.0/9-245.0/18*ci*ci+5.0/4*(ci**4))+eta*eta*(-14.0/3+35.0/6*ci*ci-5.0/4*(ci**4)))
        Dcross(4,5)=si*ci*deltaM*(27.0*pi/4)
        Dcross(5,5)=4.0/15*ci*si*si*(55.0-12.0*ci*ci-5.0/3*eta*(119.0-36*ci*ci)+5.0*eta*eta*(17.0-12.0*ci*ci))
        Dcross(7,5)=ci*(-81.0/20*(si**4)*(1.0-5.0*eta+5.0*eta*eta))
!!!!!!! s=2.5
        Cplus(2,6)=si*deltaM*(1771.0/5120-1667.0/5120*ci*ci+217.0/9216*(ci**4)-1.0/9126*(ci**6)+eta*(681.0/256+13.0/768*ci*ci-35.0/768*(ci**4)\
        +1.0/2304*(ci**6))+eta*eta*(-3451.0/9216+673.0/3072*ci*ci-5.0/9216*(ci**4)-1.0/3072*(ci**6)))
        Cplus(3,6)=pi*(19.0/3+3.0*ci*ci-2.0/3*(ci**4)+eta*(-16.0/3+14.0/3*ci*ci+2.0*(ci**4)))
        Cplus(4,6)=si*deltaM*(3537.0/1024-22977.0/5120*ci*ci-15309.0/5120*(ci**4)+729.0/5120*(ci**6)+eta*(-23829.0/1280+5529.0/1280*ci*ci\
        +7749.0/1280*(ci**4)-729.0/1280*(ci**6))+eta*eta*(29127.0/5120-27267.0/5120*ci*ci-1647.0/5120*(ci**4)+2187.0/5120*(ci**6)))
        Cplus(5,6)=-16.0*pi/3.0*(1.0+ci*ci)*si*si*(1.0-3.0*eta)
        Cplus(6,6)=si*deltaM*(-108125.0/9216+40625.0/9216*ci*ci+83125.0/9216*(ci**4)-15625.0/9216*(ci**6)+eta*(8125.0/256-40625.0/2304*ci*ci\
        -48125.0/2304*(ci**4)+15625.0/2304*(ci**6))+eta*eta*(-119375.0/9216+40625.0/3072*ci*ci+44375.0/9216*(ci**4)-15625.0/3072*(ci**6)))
        Cplus(8,6)=deltaM*(117649.0/46080*(si**5)*(1.0+ci*ci)*(1.0-4.0*eta+3.0*eta*eta))
        Dplus(3,6)=-9.0/5+14.0/5*ci*ci+7.0/5*(ci**4)+eta*(32.0+56.0/5*ci*ci-28.0/5*(ci**4))
        Dplus(5,6)=si*si*(1.0+ci*ci)*(56.0/5-32.0*log(2.0)/3.0+eta*(-1193.0/30+32.0*log(2.0)))
                             !cross
        Ccross(1,6)=6.0/5*si*si*ci*eta
        Ccross(3,6)=ci*(2.0-22.0/5*ci*ci+eta*(-282.0/5+94.0/5*ci*ci))
        Ccross(5,6)=ci*si*si*(-112.0/5+64.0/3*log(2.0)+eta*(1193.0/15-64.0*log(2.0)))
        Dcross(2,6)=si*ci*deltaM*(-913.0/7680+1891.0/11520*ci*ci-7.0/4608*(ci**4)+eta*(1165.0/384-235.0/576*ci*ci+7.0/1152*(ci**4))\
        +eta*eta*(-1301.0/4608+301.0/2304*ci*ci-7.0/1536*(ci**4)))
        Dcross(3,6)=pi*ci*(34.0/3-8.0/3*ci*ci+eta*(-20.0/3+8.0*ci*ci))
        Dcross(4,6)=si*ci*deltaM*(12501.0/2560-12069.0/1280*ci*ci+1701.0/2560*(ci**4)+eta*(-19581.0/640+7821.0/320*ci*ci-1701.0/640*(ci**4))\
        +eta*eta*(18903.0/2560-11403.0/1280*ci*ci+5103.0/2560*(ci**4)))
        Dcross(5,6)=si*si*ci*(-32.0*pi/3*(1.0-3.0*eta))
        Dcross(6,6)=deltaM*si*ci*(-101875.0/4608+6875.0/256*ci*ci-21875.0/4608*(ci**4)+eta*(66875.0/1152-44375.0/576*ci*ci+21875.0/1152*(ci**4))\
        +eta*eta*(-100625.0/4608+83125.0/2304*ci*ci-21875.0/1536*(ci**4)))
        Dcross(8,6)=deltaM*(si**5)*ci*(117649.0/23040*(1.0-4.0*eta+3.0*eta*eta))
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
        P(i,j)=signn*sqrt((Cplus(i,j)*Fplus+Ccross(i,j)*Fcross)**2+(Dplus(i,j)*Fplus+Dcross(i,j)*Fcross)**2)
        phi(i,j)=0.0
        phi(i,j)=atan(-(Dplus(i,j)*Fplus+Dcross(i,j)*Fcross)/(Cplus(i,j)*Fplus+Ccross(i,j)*Fcross+0.00000001))
        enddo
        enddo
!       write(*, *) Cplus(3, 6), Ccross(3, 6), Cplus(3, 4), Ccross(3, 4)
!       write(*, *) Dplus(3, 6), Dcross(3, 6), Dplus(3, 4), Dcross(3, 4)
!       write(*, *)
        end

              subroutine noise(s_fit,f,fbase,Nbase,ith,imax)
                      implicit none 
                      double precision :: f
                      double precision :: s_fit
                      double precision :: fbase(1:imax),Nbase(1:imax)
                      integer          :: ith,imax
                      double precision :: x, y, zx, zy
                      integer          :: i
                      double precision :: fact1, fact2
                      if((ith.LE.1).or.(ith.GE.imax)) then
                              i=1
                      elseif(ith.GT.1) then
                              i=ith-1
                      endif
                      do while(f.GE.fbase(i))
                      i=i+1
                      enddo
                      zx=fbase(i)
                      zy=Nbase(i)
                      i=i+1
                      x=fbase(i+1)
                      y=Nbase(i+1)
                      close(9)
                      fact1=( x-f)/(x-zx)
                      fact2=(f-zx)/(x-zx)
                      s_fit=((zy*fact1+y*fact2)*(10.0**(20.0)))**2.0
                      !ith=i
              end subroutine noise
end module Fisher

