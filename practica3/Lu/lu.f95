
!DESCOMPOSICION LU
!Tras esto, para resolver el sistema, debemos tener en cuenta que
	!u(i)=a(i,i)
	!a(i,i)=1, i=1,..,n
	!Lw=b <=sistl(a,b,w)
	!a(i,i)=u(i)
	!Uu=w <=sistu(a,w,u)

subroutine lu(a,deter)

implicit none

character(len=10)::formato4='(100e12.4)',formato10='(50e18.10)' !FORMATO
real(8),dimension(:,:),intent(inout):: a !matriz de S.E.L !matriz triangular inferior con 1 en la diagonal !matriz trianguar superior
real(8),intent(out):: deter

integer::n,i,j,k

n=size(a(1,:)) !O n=int(sqrt(real(size(a))))  

if(abs(a(1,1))<1.e-12)then
	print*,'pivote 1 nulo'
	stop
end if

deter=a(1,1) !inicializacion do determinante

do i=2,n
	a(i,1)=a(i,1)/a(1,1)
end do

do i=2,n
	do j=i,n
		do k=1,i-1,1
			a(i,j)=a(i,j)-a(i,k)*a(k,j)
		end do
	end do
	do j=i+1,n,1
		do k=1,i-1
			a(j,i)=a(j,i)-a(j,k)*a(k,i)
		end do
		a(j,i)=a(j,i)/a(i,i)
	end do
	deter=deter*a(i,i)
	if(abs(a(i,i))<1.e-12)then
		print*,'elemento diagonal nulo na etapa: ',k
		stop
	end if
end do


end subroutine
