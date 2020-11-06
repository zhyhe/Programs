	subroutine dL_Func(dL, z0)
	   implicit none
	   double precision :: dL, z0
	   double precision :: h, sum, z
	   double precision :: H0 = 67.3D0
	   double precision :: O_Lambda = 0.685, O_m = 0.315
	   double precision :: c0 = 299792.458D0
	   integer :: counter = 0
	   sum = 0D0
	   z = 0D0
	   h = z0 / 1000.0D0
	   sum = 0.5D0 * h / ((O_m * ((1.0D0 + z0)**3) + O_Lambda)**0.5D0)
	   sum = sum + 0.5D0 * h / ((O_m +  O_Lambda)**0.5D0) 
	   do counter = 1, 999, 1
	      z = h+ z
	      sum = sum+1.0D0*h/((O_m*((1.0D0+z)**3)+O_Lambda)**0.5D0)
	   end do
	   dL = 1.0D0 * sum * (1 + z0) * c0 / H0
	end



!	program main
!	   implicit none
!	   double precision dL, z0
!	   z0 = 0.2
!	   call dL_Func(dL, z0)
!	   write(*, *) z0, dL
!	end
