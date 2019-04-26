
!INTRODUCIR MATRIZ POR FILAS EN CRECIENTE (1a fila: 1 elemento, 2a fila: 1 y 2 elemento...n-1a fila: 1 2 .. n-1 elemento)

program chol_ppal

!Declaracion de modulos/interfaces

use sistu_interf
use sistl_interf
use residuo_interf

implicit none
!Declaracion de variables y parametros
character(len=10)::formato4='(100e12.4)',formato10='(50e18.10)' !FORMATO

real(8),allocatable::as(:),ad(:) !as: subdiagonal, ad: diagonal principal
real(8),allocatable::b(:),w(:),u(:),r(:) !r es para el residuo
real(8)::deter
integer:: n,i

print*,'Resolucion polo metodo de Cholesky do sistema Au=b'
print*,'Caso de Matriz Tridiagonal'
print*,'Introduza a orde do sistema'
read*,n

allocate(a(n,n), aa(n,n),b(n), u(n), w(n), r(n))

call datsissim(a, b)
aa=a
call choltrid(a, deter)

call sistl(a,b,w)
!Transponemos a, ya que necesitamos la traspuesta en sistu
a=TRANSPOSE(a) 
call sistu(a,w,u)

call residuo(aa,b,u,r)

print*,' '
print*,'O resultado u ,empregando a factorizacion de Cholesky, e:'
print formato10,u
print*,' '
print*,'O determinante de A ten o seguinte valor: '
print*,deter
print*,' '
print*,'El residuo r=Au-b es:'
print formato4,r
print*,' '
print*,'La norma del residuo es :'
print formato4,sqrt(dot_product(r,r))

deallocate(a,aa,b,w,u,r)

end program
