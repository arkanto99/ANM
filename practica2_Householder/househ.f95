
subroutine househ(a,b,deter)

implicit none

real(8), dimension(:,:),intent(inout):: a
real(8), dimension(:),intent(inout):: b
real(8),intent(out)::deter 

real(8):: alfa,beta,p,q,signo
integer:: n,k,i,j

n=size(b)
deter=1
do k=1,n-1 !Etapas do metodo: seleccion do k-esimo vector columna
	!Calculo de alfa
	alfa=0
	signo=1 !Necesario para el signo
	do i=k,n
		alfa=alfa+a(i,k)*a(i,k) !Norma do k-esimo vector columna da A
	end do
	alfa=sqrt(alfa)
	signo=-sign(signo,a(k,k))!sign devuelve el valor del primer argumento con el signo del segundo
	alfa=signo*alfa 

	beta=alfa*(alfa-a(k,k)) !a(k,k) fai referencia ao elemento k da k-esima columna

	a(k,k)=a(k,k)-alfa !Version para o computador de w= x-alfa*e1 (so e necesario actualizar a(k,k), o resto non influe nos calculos)
	do j=k+1,n !Actualizacion do resto de columna da matriz
		q=0
		do i=k,n
			q=q+a(i,k)*a(i,j)
		end do
		p=q/beta
		do i=k,n
			a(i,j)=a(i,j)-p*a(i,k) 
		end do
	end do
	!Actualizacion do vector de coeficientes
	q=0
	do i=k,n
		q=q+a(i,k)*b(i)
	end do
	p=q/beta
	do i=k,n !
		b(i)=b(i)-p*a(i,k)
	end do

	a(k,k)=alfa !ERROR EN LOS APUNTES; DONDE PONE a(k,k)=s
	deter=deter*a(k,k)
end do
deter=deter*a(n,n)
deter=(-1)**(n-1)*deter

end subroutine
