	include "NOISES.F"





	program main
	double precision :: sGEO,sILIGO,sTAMA,sVIRGO,sALIGO,sLISA
	double precision :: sET_B,sET,sideal
	double precision :: f,ef
	double precision :: lnf, sET_D, sCE
 	double precision, dimension(:), allocatable :: fbase, Nbase
!	double precision :: fbasea(1:3000),Nbasea(1:3000)
	integer          :: ith, imax, i
	double precision :: s1, s2, s3, s4
!	double precision :: fbaseb(1:6908),Nbaseb(1:6908)

!!!!!!!!! ET-B
	imax=4011
	ith=1
	allocate(fbase(1:imax),Nbase(1:imax))
	open (unit=9,file='ETB/ETB.txt',status='old',
     $	access='sequential',form='formatted')
	do i=1, imax, 1
	read(9,*) fbase(i), Nbase(i)
	enddo
	close(9)
	open (unit=1,file='ETB/ETB_check.txt',status='replace',
     $	access='sequential',form='formatted')
	do lnf=log(1.0), log(10000.0), 0.001
	f=exp(lnf)
	call noise(sET_D,f,fbase,Nbase,ith,imax)
	write(1,121) f, sqrt(sET_D)
	enddo
	deallocate(fbase)
	deallocate(Nbase)
	close(1)


!!!!!!!!! ideal
	imax=5010
	ith=1
	allocate(fbase(1:imax),Nbase(1:imax))
	open (unit=9,file='ideal/ideal.txt',status='old',
     $	access='sequential',form='formatted')
	do i=1, imax, 1
	read(9,*) fbase(i), Nbase(i)
	enddo
	close(9)
	open (unit=1,file='ideal/ideal_check.txt',status='replace',
     $	access='sequential',form='formatted')
	do lnf=log(1.0), log(10000.0), 0.001
	f=exp(lnf)
	call noise(sET_D,f,fbase,Nbase,ith,imax)
	write(1,121) f, sqrt(sET_D)
	enddo
	deallocate(fbase)
	deallocate(Nbase)
	close(1)

!!!!!!!!! ET-D
	imax=3000
	ith=1
	allocate(fbase(1:imax),Nbase(1:imax))
	open (unit=9,file='ETD/ETD.txt',status='old',
     $	access='sequential',form='formatted')
	do i=1, imax, 1
	read(9,*) fbase(i), Nbase(i)
	enddo
	close(9)
	open (unit=1,file='ETD/ETD_check.txt',status='replace',
     $	access='sequential',form='formatted')
	do lnf=log(1.0), log(10000.0), 0.001
	f=exp(lnf)
	call noise(sET_D,f,fbase,Nbase,ith,imax)
	write(1,121) f, sqrt(sET_D)
	enddo
	deallocate(fbase)
	deallocate(Nbase)
	close(1)

!!!!!!!!! CE
	imax=6908
	ith=1
	allocate(fbase(1:imax),Nbase(1:imax))
	open (unit=9,file='CE/CE.txt',status='old',
     $	access='sequential',form='formatted')
	do i=1, imax, 1
	read(9,*) fbase(i), Nbase(i)
	enddo
	close(9)
	open (unit=1,file='CE/CE_check.txt',status='replace',
     $	access='sequential',form='formatted')
	do lnf=log(5.0), log(4990.0), 0.001
	f=exp(lnf)
	call noise(sCE,f,fbase,Nbase,ith,imax)
	write(1,121) f, sqrt(sCE)
	enddo
	deallocate(fbase)
	deallocate(Nbase)
	close(1)


!!!!!!!!! L1
	imax=65536
	ith=1
	allocate(fbase(1:imax),Nbase(1:imax))
	open (unit=9,file='L1/L1-GDS-CALIB_STRAIN.txt',status='old',
     $	access='sequential',form='formatted')
	do i=1, imax, 1
	read(9,*) fbase(i), Nbase(i)
	enddo
	close(9)
	open (unit=1,file='L1/L1.txt',status='replace',
     $	access='sequential',form='formatted')
	do lnf=log(9.999), log(6001.0), 0.001
	f=exp(lnf)
	call noise(sCE,f,fbase,Nbase,ith,imax)
	write(1,121) f, sqrt(sCE)*10.0**(-20.0)
	enddo
	deallocate(fbase)
	deallocate(Nbase)
	close(1)



!!!!!!!!! H1
	imax=65536
	ith=1
	allocate(fbase(1:imax),Nbase(1:imax))
	open (unit=9,file='H1/H1-GDS-CALIB_STRAIN.txt',status='old',
     $	access='sequential',form='formatted')
	do i=1, imax, 1
	read(9,*) fbase(i), Nbase(i)
	enddo
	close(9)
	open (unit=1,file='H1/H1.txt',status='replace',
     $	access='sequential',form='formatted')
	do lnf=log(9.999), log(6001.0), 0.001
	f=exp(lnf)
	call noise(sCE,f,fbase,Nbase,ith,imax)
	write(1,121) f, sqrt(sCE)*10.0**(-20.0)
	enddo
	deallocate(fbase)
	deallocate(Nbase)
	close(1)



121	format(10e20.10)
	end





