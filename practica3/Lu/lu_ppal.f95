
!En la practica no se calcula el residuo, ya que la matriz que usariamos para comprobarlo no es la matriz original, si no la "transformada" por el metodo de LU. Por eso realizamos una copia (en la practica no se hace nunca), de ahi las matriz aa e bb

program lu_ppal

!Declaracion de modulos/interfaces
use datsis_interf
use lu_interf
use sistl_interf
use sistu_interf
use residuo_interf

implicit none
!Declaracion de variables y parametros
real(8), dimension(:,:), allocatable :: a, aa
real(8), dimension(:), allocatable :: b, u, w, r !r es para el residuo
real(8) ::deter
integer :: n, i

character(len=10)::formato4='(100e12.4)',formato10='(50e18.10)' !FORMATO

print*,'Resolucion usando a descomposicion LU do sistema Au=b'
print*,'Introduza a orde do sistema: '
read*, n

!Reservamos a memoria para as variables dinamicas
allocate(a(n,n), aa(n,n), b(n), u(n), w(n), r(n))

!Chamada as subrutinas + copia de A para a execucion de residuo

call datsis(a,b)
aa=a
call lu(a,deter)


!Realizamos os bucles para pasar as diagonais das matrices correctamente as funcións 

do i=1,n
	u(i)=a(i,i)
	a(i,i)=1
end do

call sistl(a,b,w)

do i=1,n
	a(i,i)=u(i)
end do

call sistu(a,w,u)
call residuo(aa,b,u,r) !Ver  nota inicio programa

!Impresion de resultados
print*,' '
print*,'O resultado u ,empregando a factorizacion LU polo metodo de Doolitle, e:'
print formato10,u
print*,' '
print*,'O determinante de A ten o seguinte valor: '
print*,deter
print*,' '
print*,'O residuo r=Au-b e:'
print formato4,r
print*,' '
print*,'A norma do residuo e :'
print formato4,sqrt(dot_product(r,r))

!Liberacion de memoria
deallocate(a,b,u,r,aa,w)

end program

!print*,"A matriz  A EN LU PPAL es: "
!do i=1,n
!	print formato4,a(i,1:n) !FORMATO
!end do
