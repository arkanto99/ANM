program sistu_ppal

!Declaracion de modulos/interfaces
use datsissim_interf
use sistu_interf
use sistub_interf

implicit none
!Declaracion de variables y parametros
integer,parameter:: clreal=selected_real_kind(p=15,r=307) !PRECISION, a veces puede no funcionar a la primera
character(len=10)::formato4='(100e12.4)',formato10='(50e18.10)' !FORMATO

real(kind=clreal),allocatable::a(:,:)
real(kind=clreal),allocatable::b(:),u(:)
integer:: n,i

print*,'Resolucion polo metodo de montante do sistema Au=b'
print*,'Introduza a dimension do matriz b'
read*,n

!Reservamos a memoria para as variables dinamicas
allocate(a(n,n))
allocate(b(n),u(n))

!Chamamos a subroutine de lectura de datos
call datsissim(a,b)

!Empregrando a subroutine Sistu
call sistu(a,b,u)
print*,
print*,'O resultado u ,empregando Sistu (calculo normal) es:'
print formato10,u

u=0 !Poñemos o resultado a 0 de novo

!Empregrando a subroutine Sistub
call sistub(a,b,u)
print*,
print*,'O resultado u ,empregando Sistub (calculo vectorial) es:'
print formato10,u

!Liberamos memoria
deallocate(a,b,u)

end program sistu_ppal




