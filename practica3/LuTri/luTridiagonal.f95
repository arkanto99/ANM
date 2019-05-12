
program luTridiagonal

use residuoTri_interf

implicit none

real(8),dimension(:),allocatable::a,b,c!Diagonales: principal, superior e inferior.
real(8),dimension(:),allocatable::u, r!u=Termo independente, r= residuo
real(8),dimension(:),allocatable::aa,cc,uu !Copias residuo
real(8)::deter
integer::n,i !Dimension
character(len=10)::formato4='(100e12.4)',formato10='(50e18.10)' !FORMATO


print*,'Introduza a dimension do sistema'
read*,n

!Reserva de memomia para as diagonais e o termo independente
allocate(a(n),aa(n),cc(n),b(n-1),c(2:n),u(n),r(n))

deter=1!Inicializacion do determinante

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
read*,u(1:n)

!Impresion de datos lidos
print*,'A diagonal principal e: '
print formato10,a
print*,'A diagonal superior e: '
print formato10,b
print*,'A diagonal inferior e: '
print formato10,c
print*,'O termo independente e: '
print formato10,u

!Datos residuo: no es necesario copiar b porque no se modifica en todo el programa
aa=a
cc=c
uu=u
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!ALGORITMO DE FACTORIZACION!!!!!!!!!!!!!!!!!!!!!!
!En el proceso de factorizacion no se modifica la diagonal superior b
do i=2,n
	c(i)=c(i)/a(i-1)
	a(i)=a(i)-c(i)*b(i-1)
end do
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!CALCULO DEL DETERMINANTE!!!!!!!!!!!!!!!!!!!
!El determinante es el determinante de la matriz U, que es el producto de su diagonal, que se corresponde con el vector 'a' en este caso
do i=1,n
	deter=deter*a(i)
end do
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!DESCENSO!!!!!!!!!!!!!!!!!!!!!!!!!
!En este tramo, u hace el papel habitual de w
do i=2,n
	u(i)=u(i)-c(i)*u(i-1)
end do
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!REMONTE!!!!!!!!!!!!!!!!!!!!!!!!!!
u(n)=u(n)/a(n)
do i=n-1,1,-1
	u(i)=(u(i)-b(i)*u(i+1))/a(i)
end do

!Calculo do residuo
call residuoTri(aa,b,cc,u,uu,r)

!!!!!!!!!!!!!!!!!!!!!!!!IMPRESION DE RESULTADOS!!!!!!!!!!!!!!!
print*,' '
print*,'A solucion do sistema e: '
print formato10,u
print*,' '
print*,'O determinante da matriz e: '
print*,deter
print*,' '
print*,'O residuo r=Au-b e:'
print formato4,r
print*,' '
print*,'A norma do residuo e :'
print formato4,sqrt(dot_product(r,r))
print*,' '
print*,' '
print*,'Matriz L (unicos valores non nulos na diagonal, que son 1s, e na subdiagonal inferior)'
print formato4,c
print*,' '
print*,'Matriz U (unicos valores non nulos na diagonal e na subdiagonal superior)'
print formato4,a
print formato4,b

!Liberacion de memoria
deallocate(a,b,c,u,aa,cc,uu,r)

end program
