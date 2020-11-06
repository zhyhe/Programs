!!!   NOTICE, THIS FORMULA MAYBE WRONG !!!
!!!!!!!!!!!!!!!!!!!!!!   GEO

	FUNCTION sET_D(f)
	implicit none
	double precision :: f
	double precision :: sET_D
	double precision :: x, y, zx, zy
	integer          :: i
	double precision :: fact1, fact2
	double precision :: s1, s2, s3, s4
	open (unit=9,file='curve_data.txt',status='old',
     $	access='sequential',form='formatted')
	read(9,*) x, s1, y, s2, s3, s4
	do while(f.GE.x)
	zx=x
	zy=y
	read(9,*) x, s1, y, s2, s3, s4
	enddo
	zx=x
	zy=y
	read(9,*) x, s1, y, s2, s3, s4
	close(9)
	fact1=( x-f)/(x-zx)
	fact2=(f-zx)/(x-zx)
	sET_D=zy*fact1+y*fact2
	end


	program main
	implicit none
	double precision :: lnf, f, sET_D
	open (unit=1,file='sETD_check.txt',status='replace',
     $	access='sequential',form='formatted')
	do lnf=log(5.0), log(4988.0), 0.001
	f=exp(lnf)
	write(1,121) f, sET_D(f)
	enddo
	close(1)
121	format(10E20.10)
	end
