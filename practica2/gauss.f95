!Eliminacion de Gauss e calculo del determinante

subroutine gauss(a,b,deter)

implicit none

real(8),dimension(:,:),intent(inout):: a !matriz de S.E.L
real(8),dimension(:),intent(inout)::b
real(8),intent(out):: deter

integer::n !orden del S.E.L
integer:: i,j,k
real(8)::piv,factor

!inicializacion do determinante
n=size(b)
deter=1.

!etapa k-esima da eliminacion //Cambio de etapa do proceso de eliminacion
do k=1,n-1
	piv=a(k,k)!comprobacion de que o k-esimo pivote non e nulo		
	if(abs(piv)<1.e-12) then
		print*,'pivote nulo na etapa: ',k
		stop
	end if	
	deter=deter*piv !actualizacion do determinante
	!eliminacion
	do i=k+1,n ! Cambio de fila
		factor=a(i,k)/piv
		!a(i,k)=0 !Esta linea solo facilita el visionado de la matriz MA, triangular inferior
		do j=k+1,n !Iteracion a lo largo de los elementos de la i-esima fila
			a(i,j)=a(i,j)-factor*a(k,j)
		end do
		b(i)=b(i)-factor*b(k) !Actualizacion del termino independiente
	end do
end do
!comprobacion de que o
!ultimo pivote non e nulo
if(abs(a(n,n))<1.e-12) then
	print*,'pivote nulo na etapa: ',n
	stop
end if
!remate do calculo do determinante
deter=deter*a(n,n)

end subroutine
