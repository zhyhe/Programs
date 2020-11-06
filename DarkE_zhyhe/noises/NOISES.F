!!!   NOTICE, THIS FORMULA MAYBE WRONG !!!
!!!!!!!!!!!!!!!!!!!!!!   GEO
	FUNCTION sGEO(f)  !!!!!! unit: 10^{-40}*(1/Hz)
	double precision sGEO,f,fs,f0,x,s0
	fs=40.0
	f0=150.0
	s0=1.0*(10.0**(-6.0))
	x=f/f0
	if(f.ge.fs) then
	sGEO=s0*((3.4*x)**(-30)+34.0/x
     $	+(20.0*(1.0-(x*x)+0.5*(x**4)))/(1.0+0.5*x*x))
	else
	sGEO=10.0**(20)
	endif
	end

	FUNCTION sILIGO(f)  !!!!!! unit: 10^{-40}*(1/Hz)
	double precision sILIGO,f,fs,f0,x,s0
	fs=40.0
	f0=150.0
	s0=9.0*(10.0**(-6.0))
	x=f/f0
	if(f.ge.fs) then
	sILIGO=s0*((4.49*x)**(-56)+0.16*(x**(-4.52))+0.52+0.32*x*x)
	else
	sILIGO=10.0**(20)
	endif
	end

	FUNCTION sTAMA(f)  !!!!!! unit: 10^{-40}*(1/Hz)
	double precision sTAMA,f,fs,f0,x,s0
	fs=75.0
	f0=400.0
	s0=7.5*(10.0**(-6.0))
	x=f/f0
	if(f.ge.fs) then
	sTAMA=s0*((x)**(-5)+13.0/x+9.0*(1.0+x*x))
	else
	sTAMA=10.0**(20)
	endif
	end

	FUNCTION sVIRGO(f)  !!!!!! unit: 10^{-40}*(1/Hz)
	double precision sVIRGO,f,fs,f0,x,s0
	fs=20.0
	f0=500.0
	s0=3.2*(10.0**(-6.0))
	x=f/f0
	if(f.ge.fs) then
	sVIRGO=s0*((7.8*x)**(-5)+2.0/x+0.63+x*x)
	else
	sVIRGO=10.0**(20)
	endif
	end

	FUNCTION sALIGO(f)  !!!!!! unit: 10^{-40}*(1/Hz)
	double precision sALIGO,f,fs,f0,x,s0
	fs=20.0
	f0=215.0
	s0=10.0**(-9.0)
	x=f/f0
	if(f.ge.fs) then
	sALIGO=s0*((x)**(-4.14)-5.0/x/x
     $	+(111.0*(1.0-x*x+0.5*(x**4)))/(1.0+0.5*x*x))
	else
	sALIGO=10.0**(20)
	endif
	end

	FUNCTION sET(f)  !!!!!! unit: 10^{-40}*(1/Hz)
	double precision sET,f,fs,f0,x,s0
	double precision alpha,beta,a0,b0,b1,b2,b3,b4,b5,b6
	double precision c1,c2,c3,c4
	alpha=-4.05
	beta=-0.69
	a0=185.62
	b0=232.56
	b1=31.18
	b2=-64.72
	b3=52.24
	b4=-42.16
	b5=10.17
	b6=11.53
	c1=13.58
	c2=-36.46
	c3=18.56
	c4=27.43
	fs=1.0
	f0=200.0
	s0=1.449*10.0**(-12.0)
	x=f/f0
	if(f.ge.fs) then
	sET=s0*(x**(alpha)+a0*x**(beta)
     $  +(b0*(1.0+b1*x+b2*x*x+b3*(x**3)+b4*(x**4)+b5*(x**5)+b6*(x**6)))
     $  /(1.0+c1*x+c2*x*x+c3*x*x*x+c4*x*x*x*x))
	else
	sET=10.0**(20)
	endif
!	sET=sET*0.1
!	sET=0.000000001  !!!! FOR TEST!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	end

	FUNCTION sLISA(f)  !!!!!! unit: 10^{-40}*(1/Hz)
	double precision sLISA,f,fs,f0,x,s0
	fs=10.0**(-5.0)
	f0=10.0**(-3.0)
	s0=920.0*(10.0**(-6.0))
	x=f/f0
	if(f.ge.fs) then
	sLISA=s0*((x/10.0)**(-4.0)+173.0+x*x)
	else
	sLISA=10.0**(20)
	endif
	end

	FUNCTION sBBO(f)  !!!!!! unit: 10^{-40}*(1/Hz)
	double precision sBBO,f,fs,f0,x,s0
	fs=0.001
	s0=10.0**(-8.0)
	if(f.ge.fs) then
	sBBO=s0*(6.15/1000.0/f/f/f/f+1.95+1.2*f*f)
	else
	sBBO=10.0**(20)
	endif
	end


	FUNCTION sET_B(f) !!!!!! unit: 10^{-40}*(1/Hz)
	double precision sET_B,f,s0
	double precision BA1,BA2,BA3,BA4,a1,a2,a3,a4
	s0=10.0**(-10.0)
	BA1=4.558*10000.0
	BA2=6.805*1000.0
	BA3=3.041
	BA4=2.568/1000.0
	a1=-15.64
	a2=-2.145
	a3=-0.119
	a4=1.101
	sET_B=s0*(BA1*f**(a1)+BA2*f**(a2)+BA3*f**(a3)+BA4*f**(a4))**(2.0)
!	sET_B=0.000000001  !!!! FOR
	end


	FUNCTION sideal(f) !!!!!! unit: 10^{-40}*(1/Hz)
	double precision sideal,f
	sideal=0.000000001
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
	ith=i
	end

















