
!Version para matrices triangulares superiores
subroutine chol(a,deter)

implicit none

real(8),dimension(:,:),intent(inout):: a !matriz de S.E.L !matriz triangular inferior con 1 en la diagonal !matriz trianguar superior
real(8),intent(out):: deter

integer::n,i,j,k

n=size(a(1,:)) !O n=int(sqrt(real(size(a))))  

if(abs(a(1,1)) .le. 1.e-12)then
	print*,'La matriz puede no ser definida positiva'
	stop
end if

deter=a(1,1) !inicializacion do determinante

a(1,1)=sqrt(a(1,1)) !Elemento b(1,1), calculase a parte porque e necesario para calcular o resto da primeira columna

do i=2,n
	a(i,1)=a(i,1)/a(1,1) !Primeira columna da matriz B
end do

do k=2,n
	!Calculamos o elemento da diagonal b(k,k)
	do j=1,k-1
		a(k,k)=a(k,k)-a(k,j)*a(k,j)
	end do
	if(abs(a(k,k)) .le. 1.e-12)then
		print*,'La matriz puede no ser definida positiva'
		stop
	end if
	deter=deter*a(k,k)
	a(k,k)=sqrt(a(k,k))
	!Calculamos o resto da columna k (elemento so por debaixo de b(k,k), por ser B triangular inferior
	do i=k+1,n
		do j=1,k-1
			a(i,k)=a(i,k)-a(i,j)*a(k,j)
		end do
		a(i,k)=a(i,k)/a(k,k)
	end do
end do	

print*, ' '
print*,'A factorizacion de Cholesky da Matriz e:'
do i=1,n
	print '(100e12.4)',a(i,1:i) !FORMATO
end do



end subroutine
