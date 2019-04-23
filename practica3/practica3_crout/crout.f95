!Falta el calculo del determinante

!PARA MATRICES SIMETRICAS

subroutine crout(a,deter)

implicit none

real(8),dimension(:,:),intent(inout)::a
real(8),intent(out)::deter

integer:: n,i,j,k

n=size(a(1,:))

do i=2,n
	a(i,1)=a(i,1)/a(1,1)
end do

do j=2,n
	do k=1,j-1
		a(j,j)=a(j,j)-a(j,k)*a(j,k)*a(k,k)
	end do
	
	do i=j+1,n
		do k=1,j-1
			a(i,j)=a(1,k)*a(k,k)*a(j,k)
		end do
		a(i,j)=a(i,j)/a(j,j)
	end do
end do

end subroutine
