

!!REPASAR, NO FUNCIONA
program gausspp_ppal

use datsissim_interf
use gausspp_interf
use sistupf_interf
use residuo_interf

implicit none
!Declaracion de variables y parametros
character(len=10)::formato4='(100e12.4)',formato10='(50e18.10)' !FORMATO

real(8),allocatable::a(:,:),aa(:,:)
real(8),allocatable::b(:),bb(:),u(:),r(:)!r es para el residuo
integer,allocatable::l(:) !l es para la matriz de permutaciones
real(8)::deter
integer::n

print*,'Resolucion polo metodo de gauss con pivote parcial do sistema Au=b'
print*,'Introduza a dimension do matriz b'
read*,n

!Reservamos a memoria para as variables dinamicas
allocate(a(n,n),aa(n,n))
allocate(b(n),bb(n),u(n),r(n))
allocate(l(n))

call datsissim(a,b)
aa=a
bb=b
call gausspp(a,b,deter,l)
call sistupf(a,b,u,l)
call residuo(aa,bb,u,r)

!Impresion de resultados
print*,' '
print*,'O resultado u ,empregando o metodo de Gauss con pivote parcial, e:'
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

!Liberacion de memoria
deallocate(a,b,u,r)
deallocate(l)

end program

