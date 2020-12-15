        module fiducial
                use constants
                implicit none
        contains
        subroutine dL_Func(dL, z0)
           implicit none
           double precision :: dL, z0
           double precision :: h, sum, z
           double precision :: O_Lambda = 0.685, O_m = 0.315
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
        SUBROUTINE INVERSEMATRIX(y,a,n)
!!!    y is output
!!!    a is  input
        implicit none
        integer ii,jj
        double precision cc(n,n)
        integer n,indx(n),i,j
        double precision a(n,n),y(n,n),d,aa(n,n)
        do ii=1,n
        do jj=1,n
        aa(ii,jj)=a(ii,jj)
        enddo
        enddo
        do i=1,n
                do j=1,n
                y(i,j)=0
                enddo
        y(i,i)=1.0
        enddo
        call LUDCMP(aa,n,n,indx,d)
        do j=1,n
        call LUBKSB(aa,n,n,indx,y(1,j))
        enddo
        end
      SUBROUTINE ludcmp(a,n,np,indx,d)
      implicit none
      INTEGER n,np,indx(n)
      double precision d,a(np,np)
!      PARAMETER (NMAX=500,TINY=1.0e-20)
      integer, parameter :: NMAX=500
      double precision, parameter :: TINY=1.0e-35
      INTEGER i,imax,j,k
      double precision aamax,dum,sum,vv(NMAX)
      d=1.
      do 12 i=1,n
        aamax=0.
        do 11 j=1,n
          if (abs(a(i,j)).gt.aamax) aamax=abs(a(i,j))
11      continue
        if (aamax.eq.0.) pause 'singular matrix in ludcmp'
        vv(i)=1./aamax
12    continue
      do 19 j=1,n
        do 14 i=1,j-1
          sum=a(i,j)
          do 13 k=1,i-1
            sum=sum-a(i,k)*a(k,j)
13        continue
          a(i,j)=sum
14      continue
        aamax=0.
        do 16 i=j,n
          sum=a(i,j)
          do 15 k=1,j-1
            sum=sum-a(i,k)*a(k,j)
15        continue
          a(i,j)=sum
          dum=vv(i)*abs(sum)
          if (dum.ge.aamax) then
            imax=i
            aamax=dum
          endif
16      continue
        if (j.ne.imax)then
          do 17 k=1,n
            dum=a(imax,k)
            a(imax,k)=a(j,k)
            a(j,k)=dum
17        continue
          d=-d
          vv(imax)=vv(j)
        endif
        indx(j)=imax
        if(a(j,j).eq.0.)a(j,j)=TINY
        if(j.ne.n)then
          dum=1./a(j,j)
          do 18 i=j+1,n
            a(i,j)=a(i,j)*dum
18        continue
        endif
19    continue
      return
      END
      SUBROUTINE lubksb(a,n,np,indx,b)
      implicit none
      INTEGER n,np,indx(n)
      double precision a(np,np),b(n)
      INTEGER i,ii,j,ll
      double precision summ
      ii=0
      do 12 i=1,n
        ll=indx(i)
        summ=b(ll)
        b(ll)=b(i)
        if (ii.ne.0)then
          do 11 j=ii,i-1
            summ=summ-a(i,j)*b(j)
11        continue
        else if (summ.ne.0.) then
          ii=i
        endif
        b(i)=summ
12    continue
      do 14 i=n,1,-1
        summ=b(i)
        do 13 j=i+1,n
          summ=summ-a(i,j)*b(j)
13      continue
        b(i)=summ/a(i,i)
14    continue
      return
      END
        end

