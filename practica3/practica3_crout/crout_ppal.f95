program crout_ppal

!Declaracion de modulos/interfaces
use datsis_interf
use crout_interf
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
call crout(a,deter)


!Realizamos os bucles para pasar as diagonais das matrices correctamente as funcións 


call sistl(a,b,w)

do i=1,n
	w(i)=w(i)/a(i,i) !En este caso, w toma el papel del z de los apuntes
end do
call sistu(transpose(a),w,u)
call residuo(aa,b,u,r) !Ver  nota inicio programa

!Impresion de resultados
print*,' '
print*,'O resultado u ,empregando o metodo de Crout (A=LDL^t), e:'
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
deallocate(a,b,u,r,aa,w)

end program
