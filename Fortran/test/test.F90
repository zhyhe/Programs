	program main
	integer::m=8
	integer(1)::x1
	integer(2)::x2
	integer(4)::x3
      double precision :: pi=3.141592653589793D0
	logical(2)::y2
	logical(4)::y4
	real(4)   ::r4
	real(8)   ::r8
	namelist /list/ x1,x2,x3 /list2/ y2,y4
	write(*,list)
	write(*,list2)
	print 100, m
	100 format(I10)
      print *,pi
	end
