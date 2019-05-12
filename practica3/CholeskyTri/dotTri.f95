
subroutine dotTri(ad,au,al,v,w)

implicit none

integer::n,i
real(8),dimension(:),intent(in)::ad,au,al !ad= diagonal principal; au= diagonal superior; !al=diagonal inferior
real(8),dimension(:),intent(in)::v ! v= termino independiente
real(8),dimension(:),intent(out)::w !w= resultado

n=size(ad)7
!FORMA VECTORIAL DE CALCULO
w(1:n)=ad*v(1:n)
w(1:n-1)=w(1:n-1) +au*v(2:n)
w(2:n)=w(2:n)+ al*v(1:n-1)

!print*,'O vector w resultado e: '
!print*,w

end subroutine

!CALCULO
!w(1)=ad(1)*v(1)+au(1)*v(2)
!do i=2,n-1
!	w(i)=al(i)*v(i-1)+ad(i)*v(i)+au(i)*v(i+1)
!end do
!w(n)=al(n)*v(n-1)+ad(n)*v(n)
