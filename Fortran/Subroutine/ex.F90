      implicit none
      integer :: a=1
      integer :: b=2
      integer,external :: addf
      call add(a,b)
      print *,addf(a,b)
      end
