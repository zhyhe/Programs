Program g
        implicit none
        integer(1) :: a
        integer(1),parameter :: nBit=20
        integer(1) :: myPI(nBit),bitA(nBit),bitB(nBit),BitAs(nBit),BitBs(nBit),BitPIA(nBit),BitPIB(nBit)
        integer    :: i,j,m,n


        !!caddll Rot( nBit , BitBs , bitB , BitBs , 239 )


        m=1
        n=239
        j=m
        do i=2,nBit
        bitB(i)=j/n
        j=mod(j,n)
        j=j*10
        enddo
        print'(50I1)',bitB
                
        do i=nBit,2,-1
        j=bitB(i)+bitB(i)
        bitA(i)=bitA(i)+mod(j,10)
        bitA(i-1)=bitA(i-1)+j/10
        enddo



        print'(50I1)',bitA
        call Rot(nBit,bitA,bitB,BitBs,n)
        print'(50I2)',BitBs
end

Subroutine Rot( nBit , nA , nB , nC , nM )
    Integer , Intent( IN ) :: nBit , nM
    Integer(1) , Intent( INOUT ) :: nA(nBit) , nB(nBit) , nC(nBit)
    integer :: i , j
    Do i = 2 , nBit
      j = mod( nB(i-1) , nM ) * 10 + nA( i )
      nC(i) = j / nM
      nB(i) = mod( j , nM )
    End Do
End Subroutine Rot

