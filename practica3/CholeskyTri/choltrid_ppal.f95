
!PROGRAMA SIN HACER; EL QUE APARECE ES CHOLESKY NORMAL!!!!

program chol_ppal

!Declaracion de modulos/interfaces


use residuoTri_interf

implicit none
!Declaracion de variables y parametros
character(len=10)::formato4='(100e12.4)',formato10='(50e18.10)' !FORMATO

real(8),allocatable::as(:),ad(:), x(:), y(:)!as: subdiagonal, ad: diagonal principal
real(8),allocatable::b(:),w(:),u(:),r(:) !r es para el residuo
real(8)::deter
integer:: n,i

print*,'Resolucion polo metodo de Cholesky do sistema Au=b'
print*,'Caso de Matriz Tridiagonal'
print*,'Introduza a orde do sistema'
read*,n

allocate(as(n-1), ad(n),x(n),y(n-1),b(n), u(n), w(n), r(n))

!Lectura de datos
print*,'Introduza os elementos da diagonal principal '
read*,ad(1:n)
print*,'Introduza os elementos da diagonal secundaria '
read*,as(1:n-1)
print*,'Introduza os elementos do termo independente '
read*,b(1:n)
print*,'Diagonal principal'
print formato4, ad
print*,'Diagonal secundaria'
print formato4, as
print*,'Termo independente'
print formato4,b

!!!!!!!!!!!!!!!!!!!!!!!!!!!Proceso de eliminacion!!!!!!!!!!!!!!!!!!1
x(1)=sqrt(ad(1))
do i=2,n
	y(i-1)=as(i-1)/x(i-1)
	x(i)=sqrt(ad(i)-y(i-1)**2)
end do

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!DESCENSO!!!!!!!!!!!!!!!!!!!!!!!!!
w(1)=b(1)/x(1)
do i=2,n
	w(i)=(b(i)-y(i-1)*w(i-1))/x(i)
end do
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!REMONTE!!!!!!!!!!!!!!!!!!!!!!!!!!
u(n)=w(n)/x(n)
do i=n-1,1,-1
	u(i)=(w(i)-y(i)*u(i+1))/x(i)
end do

call residuoTri(ad,as,as,u,b,r)

print*,' '
print*,'O resultado u ,empregando a factorizacion de Cholesky Tridiagonal, e:'
print formato10,u
print*,' '
print*,'O residuo r=Au-b e:'
print formato4,r
print*,' '
print*,'A norma do residuo e :'
print formato4,sqrt(dot_product(r,r))
print*,' '
print*,' '
print*, 'A diagonal principal da factorizacion de Cholesky e:'
print formato10,x
print*,' '
print*,'A subdiagonal inferior (A superior e 0) da factorizacion de Cholesky e:'
print formato10,y
print*,'Os outros elementos da matriz B son nulos, sendo B unha matriz triangular inferior'


deallocate(as,ad,u,b,x,y,r,w)

end program
