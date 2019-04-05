
!En la practica no se calcula el residuo, ya que la matriz que usariamos para comprobarlo no es la matriz original, si no la "transformada" por el metodo de gauss. Por eso realizamos una copia (en la practica no se hace nunca), de ahi las matriz X e Y

program gauss_ppal

!Declaracion de modulos/interfaces
use datsissim_interf
use sistu_interf
use gauss_interf
use residuo_interf

implicit none
!Declaracion de variables y parametros
character(len=10)::formato4='(100e12.4)',formato10='(50e18.10)' !FORMATO

real(8),allocatable::a(:,:),x(:,:)
real(8),allocatable::b(:),y(:),u(:),r(:) !r es para el residuo
real(8)::deter
integer:: n,i

print*,'Resolucion polo metodo de gauss do sistema Au=b'
print*,'Introduza a dimension do matriz b'
read*,n

!Reservamos a memoria para as variables dinamicas
allocate(a(n,n),x(n,n))
allocate(b(n),y(n),u(n),r(n))

!Chamada as subrutinas+ copia das matrices para a execucion de residuo
call datsissim(a,b)
x=a
y=b
call gauss(a,b,deter)
call sistu(a,b,u)
call residuo(x,y,u,r) !Ver  nota inicio programa

!Impresion de resultados
print*,' '
print*,'O resultado u ,empregando o metodo de Gauss, e:'
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

end program
