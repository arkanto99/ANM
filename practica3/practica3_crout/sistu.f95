
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!SISTU MODIFICADO PARA EL METODO DE CROUT
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine sistu(a,b,u)

implicit none

real(8),dimension(:,:),intent(in)::a
real(8),dimension(:),intent(in)::b
real(8),dimension(:),intent(inout)::u
integer::n,i,j

n=size(b) !No nos vale size(a), ya que nos daria n*n elementos

u(n)=b(n)
do i=n-1,1,-1 !i variando de n-1 a 1, con paso DESCENDENTE de -1
	u(i)=b(i)
	do j=i+1,n
		u(i)=u(i)-a(i,j)*u(j) !Si podemos a(j,i) no haría falta transponer la matriz
	enddo
	!u(i)=u(i)/a(i,i)
enddo

end subroutine sistu
