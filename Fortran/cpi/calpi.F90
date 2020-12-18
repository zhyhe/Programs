Module PI_Calc_Mod
  Implicit None
  Integer , public , parameter :: PI_KIND = KIND(1)
  Private :: GenPI , Rot
  
  contains

  Subroutine CalcPI( nBit , myPI )
    Integer , Intent( IN ) :: nBit
    integer(Kind=PI_KIND) :: myPI(nBit),bitA(nBit),bitB(nBit),BitAs(nBit),BitBs(nBit),BitPIA(nBit),BitPIB(nBit)
    integer :: i
    myPI(:)=0
    bitA(:)=0
    bitB(:)=0
    BitAs(:)=0
    BitBs(:)=0
    BitPIA(:)=0
    BitPIB(:)=0
    BitAs(2)=3
    BitAs(3)=2
    BitBs(2)=4
    call Rot( nBit , BitBs , bitB , BitBs , 239 )
    call GenPI( nBit , myPI , BitBs , BitAs )
    Do i = 1 , nBit
      if (mod(i,2000)==0) print*,i
      call Rot( nBit , BitAs , bitA , BitAs , 25 )
      call Rot( nBit , BitAs , bitA , BitPIA , 2*i+1 )    
      call Rot( nBit , BitBs , bitB , BitBs , 57121 )
      call Rot( nBit , BitBs , bitB , BitPIB , 2*i+1 )    
      if( mod(i,2) == 1 ) then
        call GenPI( nBit , myPI , BitPIA , BitPIB )
      else
        call GenPI( nBit , myPI , BitPIB , BitPIA )
      end if
    End Do
  End Subroutine CalcPI  
  
  Subroutine Rot( nBit , nA , nB , nC , nM )
    Integer , Intent( IN ) :: nBit , nM
    Integer(Kind=PI_KIND) , Intent( INOUT ) :: nA(nBit) , nB(nBit) , nC(nBit)
    integer :: i , j
    Do i = 2 , nBit
      j = mod( nB(i-1) , nM ) * 10 + nA( i )
      nC(i) = j / nM
      nB(i) = mod( j , nM )
    End Do
  End Subroutine Rot

  Subroutine GenPI( nBit , nPI , npA , npB )
    Integer , Intent( IN ) :: nBit
    Integer(Kind=PI_KIND) , Intent( INOUT ) :: nPI(nBit) , npA(nBit) , npB(nBit)
    integer :: i
    nPI(2:nBit)=nPI(2:nBit)+npB(2:nBit)
    Do i = nBit , 2 , -1
      if( nPI(i) >= npA(i) ) then
        nPI(i) = nPI(i) - npA(i)
      else
        nPI(i) = nPI(i) + 10 - npA(i)
        nPI(i-1) = nPI(i-1) - 1
      end if
      nPI(i-1) = nPI(i-1) + nPI(i) / 10
      nPI(i) = mod( nPI(i) , 10 )
    End Do
  End Subroutine GenPI
  
End Module PI_Calc_Mod  
  
Program www_fcode_cn
  use PI_Calc_Mod
  Implicit None
  Integer , parameter :: N = 150000
  integer(Kind=PI_KIND) :: myPI(N)
  call CalcPI( N , myPI )
  open(10,file='pi.dat')
  write(10,'(50i1)') myPI
  close(10)
End Program www_fcode_cn
