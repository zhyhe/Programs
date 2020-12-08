!!!   NOTICE, THIS FORMULA MAYBE WRONG !!!
!!!!!!!!!!!!!!!!!!!!!!   GEO

	subroutine noise(sET_D,f,fbase,Nbase,ith,imax)
	implicit none
	double precision :: f
	double precision :: sET_D
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
	sET_D=zy*fact1+y*fact2
	ith=i
	end




	program main
	implicit none
	double precision :: lnf, f, sET_D
	double precision :: fbase(1:3000),Nbase(1:3000)
	integer          :: ith, imax, i
	double precision :: s1, s2, s3, s4
	imax=3000
	open (unit=9,file='curve_data.txt',status='old',
     $	access='sequential',form='formatted')
	do i=1, imax, 1
!	read(9,*) fbase(i), s1, Nbase(i), s2, s3, s4
	read(9,*) fbase(i), s1, s2, Nbase(i), s3, s4
	enddo
	close(9)
!	open (unit=1,file='sETD_check4.txt',status='replace',
!     $	access='sequential',form='formatted')
	open (unit=1,file='CE.txt',status='replace',
     $	access='sequential',form='formatted')
	do lnf=log(5.0), log(5000.0), 0.001
	f=exp(lnf)
	call noise(sET_D,f,fbase,Nbase,ith,imax)
	write(1,121) f, sET_D
	enddo
	close(1)
121	format(10E20.10)
	end
