
!Para probar el funcionamiento:
! -2 0 1  u1  -1
!  1 1 0  u2   2
! -1 0 0  u3  -1

!INCLUIR CALCULO DEL DETERMINANTE

program househ_ppal

use datsis_interf
use househ_interf
use sistu_interf
use residuo_interf

implicit none
!Declaracion de variables y parametros
character(len=10)::formato4='(100e12.4)',formato10='(50e18.10)' !FORMATO

real(8),allocatable::a(:,:),aa(:,:)
real(8),allocatable::b(:),bb(:),u(:),r(:)!r es para el residuo
real(8)::deter
integer::n,i

print*,'Resolucion polo metodo de HouseHolder do sistema Au=b'
print*,'Introduza a orde do sistema'
read*,n

!Reservamos a memoria para as variables dinamicas
allocate(a(n,n),aa(n,n))
allocate(b(n),bb(n),u(n),r(n))

call datsis(a,b)
aa=a
bb=b
call househ(a,b,deter)
call sistu(a,b,u)
call residuo(aa,bb,u,r)

!Impresion de resultados
print*,' '
print*,'O resultado u ,empregando o metodo de Householder, e:'
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
print*, ' '
print*,'A matriz de Householder e (solo son validos os alfa/diagonal_de_A , o resto dos numeros non se corresponden): '

do i=1,n
	print formato4,a(i,:) !FORMATO
end do
!Liberacion de memoria
deallocate(a,b,u,r,aa,bb)

end program

