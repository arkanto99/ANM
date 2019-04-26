program luTridiagonal

implicit none

real(8),dimension(:),allocatable::a,b,c!Diagonales: principal, superior e inferior.
real(8),dimension(:),allocatable::r!Termo independente
integer::n,i !Dimension
character(len=10)::formato4='(100e12.4)',formato10='(50e18.10)' !FORMATO


print*,'Introduza a dimension do sistema'
read*,n

!Reserva de memomia para as diagonais e o termo independente
allocate(a(n),b(n-1),c(2:n),r(n))


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!LECTURA DE DATOS!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!Lectura da diagonal principal
print*,'Introduza os elementos da diagonal principal '
read*,a(1:n)

!Lectura da diagonal principal
print*,'Introduza os elementos da diagonal superior '
read*,b(1:n-1)

!Lectura da diagonal principal
print*,'Introduza os elementos da diagonal inferior '
read*,c(2:n)

!Lectura da diagonal principal
print*,'Introduza os elementos do termo independente '
read*,r(1:n)

!Impresion de datos lidos
print*,'A diagonal principal e: '
print formato10,a
print*,'A diagonal superior e: '
print formato10,b
print*,'A diagonal inferior e: '
print formato10,c
print*,'O termo independente e: '
print formato10,r
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!ALGORITMO DE FACTORIZACION!!!!!!!!!!!!!!!!!!!!!!
do i=2,n
	c(i)=c(i)/a(i-1)
	a(i)=a(i)-c(i)*b(i-1)
end do
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!DESCENSO!!!!!!!!!!!!!!!!!!!!!!!!!
do i=2,n
	r(i)=r(i)-c(i)*r(i-1)
end do
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!REMONTE!!!!!!!!!!!!!!!!!!!!!!!!!!
r(n)=r(n)/a(n)
do i=n-1,1,-1
	r(i)=(r(i)-b(i)*r(i+1))/a(i)
end do
!!!!!!!!!!!!!!!!!!!!!!!!IMPRESION DE RESULTADOS!!!!!!!!!!!!!!!
print*,'A solucion do sistema e: '
print formato10,r

end program
