!This is a free format fortran program
      program main
              implicit none
              character(len=10)::str='love'
              integer(1):: n=126
              write(*,*) "Hello"
              write(*,*)n
              write(*,*)str

      end
!sub2s/HF2_GR.F
      function psi(f,psi_c,M,eta,fLSO)
      subroutine PandPHI(P,phi,iota,M,eta,Fplus,Fcross)
      function h2f(f,M,eta,t_c,psi_c,iota,lnr,fLSO,Fplus,Fcross)

!sub2s/Delta_Omegas_cal.F
      subroutine Delta_Omega(Fij,delta,DO1,DO2,DO3,DlndL)
      
!sub2s/detector_position.F
      subroutine rI(lambda,psi,tc,Mc,f,onoff,x,y,z)
      
!sub2s/di_BH.F
      subroutine
          dihat_stationary(psi,lambda,gama,zeta,alpha,delta,varphi,iota,
          $                M,eta,lndL,t_c,psi_c,f,FLSO,fbase,Nbase,ith,
          $                imax,turn_on_off,
          $                di_hat)
          dihat_general(ditto)
!sub2s/di.F
      subroutine dihat_stationary(ditto)
      subroutine dihat_general(ditto)
!sub2s/dL.F
      subroutine dL_Func(dL,z0)
!sub2s/Fisher_LIGO.F
      subroutine
          Fisher_LIGO_stationary(psi,lambda,gama,zeta,para,fbase,Nbase,imax,Fij,SNR)
          Fisher_LIGO_general(ditto)
!sub2s/HF_BH.F
      subroutine Aeff_Func(Aeff,eta,dL,M,f)
      subroutine HF_BH(hf,f,M,dL,eta,t_c,phi_c,iota,Fplus,Fcross)
!sub2s/INVERSE_FISHER.F
      subroutine ludcmp(a,n,np,indx,d)
      subroutine INVERSEMATRIX(y,a,n)
!sub2s/response.F
      subroutine
          Pat_Func(alpha,varphi,delta,psi,lambda,gama,zeta,tc,Mc,f,onoff,F1,Fx,Fb,Fs,Fv1,Fv2)
          Pat_stationary(alpha,varphi,delta,psi,lambda,gama,zeta,x,y,z,Fplus,Fcross)
!sub2s/source_direction.F
      subroutine n_hat(alpha,delta,x,y,z)
!sub2x/t_c.F
      subroutine t_c_Func(qt0,t_c)
