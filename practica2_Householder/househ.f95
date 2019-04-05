
subroutine househ(a,b,deter)

implicit none

real(8), dimension(:,:),intent(inout):: a
real(8), dimension(:),intent(inout):: b
real(8),intent(out)::deter 

real(8):: alfa,beta,p,q,signo
integer:: n,k,i,j

n=size(b)
deter=1
do k=1,n-1
	alfa=0
	signo=1 !Necesario para el signo
	do i=k,n
		alfa=alfa+a(i,k)*a(i,k)
	end do
	alfa=sqrt(alfa)
	signo=-sign(signo,a(k,k))!sign devuelve el valor del primer argumento con el signo del segundo
	alfa=signo*alfa 
	beta=alfa*(alfa-a(k,k))
	a(k,k)=a(k,k)-alfa
	do j=k+1,n
		q=0
		do i=k,n
			q=q+a(i,k)*a(i,j)
		end do
		p=q/beta
		do i=k,n
			a(i,j)=a(i,j)-p*a(i,k)
		end do
	end do
	q=0
	do i=k,n
		q=q+a(i,k)*b(i)
	end do
	p=q/beta
	do i=k,n
		b(i)=b(i)-p*a(i,k)
	end do
	a(k,k)=alfa !ERROR EN LOS APUNTES; DONDE PONE a(k,k)=s
	deter=deter*a(k,k)
end do
deter=deter*a(n,n)
deter=(-1)**(n-1)*deter

end subroutine
