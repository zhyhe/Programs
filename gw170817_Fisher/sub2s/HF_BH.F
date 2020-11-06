	Subroutine Aeff_Func(Aeff, eta, dL, M, f)
	   implicit none
	   double precision :: Aeff, eta, dL, M, f
	   double precision :: C, f_merg, f_ring, sigma, f_cut, Omega, L
	   double precision :: G = 6.67408D-11
	   double precision :: msun = 1.98855D30
	   double precision :: c0 = 299792458.0D0
	   double precision :: Mpc = 3.085677580D22
	   double precision :: pi = 3.141592653589793D0
	   f_merg = (2.9740D-1 * eta * eta + 4.4810D-2 * eta + 9.5560D-2)
     $	             * (c0**3) / G / pi / msun / M
	   f_ring = (5.9941D-1 * eta * eta + 8.9794D-2 * eta + 1.9111D-1)
     $	             * (c0**3) / G / pi / msun / M
	   sigma  = (5.0801D-1 * eta * eta + 7.7515D-2 * eta + 2.2369D-2)
     $	             * (c0**3) / G / pi / msun / M
	   f_cut  = (8.4845D-1 * eta * eta + 1.2848D-1 * eta + 2.7299D-1)
     $	             * (c0**3) / G / pi / msun / M
	   C = 1.0D0 * (G**(5.0 / 6.0)) * ((M*msun)**(5.0 / 6.0)) / (c0**1.5) 
     $	        / Mpc / dL / (pi**(2.0/3.0))
     $	        * ((5.0 * eta /24.0)**0.5) * (f_merg**(-7.0 / 6.0))
!	   write(*, *) C, f_merg
	   if ( f < f_merg) then
	      Aeff = C * ((1.0D0 * f / f_merg)**(-7.0D0 / 6.0D0))
	   end if
	   if ( f >= f_merg .and. f < f_ring) then
	      Aeff = C * ((1.0D0 * f / f_merg)**(-2.0D0 / 3.0D0))
	   end if 
	   if ( f >= f_ring .and. f < f_cut) then
	      omega = 1.0D0 * pi * sigma / 2.0D0 * ((1.0D0 * f_ring
     $	               / f_merg)**(-2.0D0 / 3.0D0))
	      L = 1.0D0 * sigma / (((f - f_ring)**2) + sigma * sigma / 4.0D0)
     $	           / 2.0D0 / pi
	      Aeff = C * omega * L
	   end if
	   if ( f >= f_cut ) then
	      Aeff = 1.0D-40
	   end if
!	   write(*, *) Aeff
!	   write(*, *) f, Aeff
	end




	Subroutine Psieff_Func(psi, eta, M, f, t_c, phi_c)
	   implicit none
	   double precision :: psi, eta, M, f, t_c, phi_c
	   double precision :: phi(0:7) 
	   double precision :: pi = 3.141592653589D0
	   double precision :: G = 6.67408D-11

	   double precision :: msun = 1.98855D30
	   double precision :: c0 = 299792458.0D0
	   double precision :: Mpc = 3.08567758D22
	   integer :: i
	   psi = 0.0D0
	   phi(0) = (1.7516D-1 * eta + 7.9483D-2 - 7.2390D-2 / eta) 
     $	             * ((c0**3) / G / msun / M / pi)**(5.0D0 / 3.0D0)
	   phi(1) = 0.0D0
	   phi(2) = (-5.1571D1 * eta + -1.7595D1 + 1.3253D1 / eta)
     $	             * ((c0**3) / G / msun / M / pi)**(3.0D0 / 3.0D0)
	   phi(3) = (6.5866D2 * eta + 1.7803D2 + -1.5972D2 / eta)
     $	             * ((c0**3) / G / msun / M / pi)**(2.0D0 / 3.0D0)
	   phi(4) = (-3.9031D3 * eta + -7.7493D2 + 8.8195D2 / eta)
     $	             * ((c0**3) / G / msun / M / pi)**(1.0D0 / 3.0D0)
	   phi(5) = 0.0D0
	   phi(6) = (-2.4874D4 * eta + -1.4892D3 + 4.4588D3 / eta)
     $	             * ((c0**3) / G / msun / M / pi)**(-1.0D0 / 3.0D0)
	   phi(7) = (2.5196D4 * eta + 3.3970D2 + -3.9573D3 / eta)
     $	             * ((c0**3) / G / msun / M / pi)**(-2.0D0 / 3.0D0)
	   do i = 0, 7, 1
	      psi = psi + phi(i) * (f**((1.0D0 * i - 5.0D0) / 3.0D0))
	   end do
	   psi = psi + 2 * pi * f * t_c + phi_c
!	   write(*, *) psi
	end


	Subroutine HF_BH(hf, f, M, dL, eta, t_c, phi_c, iota, Fplus, Fcross) 
	   implicit none
	   double complex :: hf
	   double precision :: f, M, dL, eta, t_c, phi_c, iota, Fplus, Fcross
	   double precision :: Aeff, psi, Fiota, ci, si, phi
	   double precision :: pi = 3.141592653589793D0
	   ci = cos(iota * pi / 180.0D0)
	   si = sin(iota * pi / 180.0D0)
	   call Aeff_Func(Aeff, eta, dL, M, f)
	   call Psieff_Func(psi, eta, M, f, t_c, phi_c)
	   Fiota = sqrt((Fplus * (1.0D0 + ci**2))**2 
     $	            + (2.0D0 * Fcross * ci)**2) / 2.0D0
	   phi = atan(-2.0D0 * ci * Fcross / (1 + ci**2) / Fplus)
	   Aeff = Fiota * Aeff / 1.0D-20
	   psi = psi - phi
!	   write(*, *) f, Aeff, psi
	   hf = Aeff * Cmplx(cos(psi), sin(psi))
!	   write(*, *) f, hf
	end


!	program main
!	   implicit none
!	   double complex :: hf
!	   double precision :: f, M, dL, eta, t_c, phi_c, lnf
!	   double precision :: iota, Fplus, Fcross
!	   double precision :: Aeff, psi
!	   open(unit=10, file="HF_BH.dat")
!	   M = 20.0
!	   eta = 0.25
!	   dL = 500.0
!	   t_c = 0
!	   phi_c = 0
!	   iota = 0.0D0
!	   Fplus = 1.0
!	   Fcross = 0.0
!	   do lnf = log(5.001), log(5000.0), 0.01
!	      f = exp(lnf) 
!	      write(*, *) f
!	      call HF_BH(hf, f, M, dL, eta, t_c, phi_c, iota, Fplus, Fcross)
!	      write(*, *) hf
!	      Aeff = sqrt(Real(hf)**2 + Aimag(hf)**2)
!	      call Psieff_Func(psi, eta, M, f, t_c, phi_c)
!	      psi = Acos(1.0D0 * Real(hf) / Aeff)
!	      write(10, *) f, Aeff, psi
!	   end do
!	   close(10)
!	end
