
program matricesTridiagonales

implicit none

integer::n,i
real(8),allocatable,dimension(:)::ad,au,al,v,w

print*,'Introduza a dimension da matriz A (tamanho da diagonal principal)'
read*,n

allocate(ad(n),au(1:n-1),al(2:n),v(n),w(n))

!LECTURA
print*,'Introduza a diagonal superior'
read*,au(1:n-1)
print*,'Introduza a diagonal principal'
read*,ad(1:n)
print*,'Introduza a diagonal inferior'
read*,al(2:n)
print*,'Introduza o vector v'
read*,v

!CALCULO
!w(1)=ad(1)*v(1)+au(1)*v(2)
!do i=2,n-1
!	w(i)=al(i)*v(i-1)+ad(i)*v(i)+au(i)*v(i+1)
!end do
!w(n)=al(n)*v(n-1)+ad(n)*v(n)

!FORMA VECTORIAL DE CALCULO
w(1:n)=ad*v(1:n)
w(1:n-1)=w(1:n-1) +au*v(2:n)
w(2:n)=w(2:n)+ al*v(1:n-1)

print*,'O vector w resultado e: '
print*,w

deallocate(ad,au,al,v,w)

end program
