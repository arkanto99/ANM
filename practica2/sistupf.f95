
subroutine sistupf(a,b,u,l)

implicit none

real(8),dimension(:,:),intent(in):: a !matriz de S.E.L
real(8),dimension(:),intent(in)::b
integer,dimension(:),intent(in)::l !permutacion de filas
real(8),dimension(:),intent(inout)::u!Solucion del S.E.L

integer::i,j,n

n=size(b)

u(n)=b(l(n))/a(l(n),n)
do i=n-1,1,-1
	u(i)=b(l(i))
	do j=i+1,n
		u(i)=u(i)-a(l(i),j)*u(j)
	enddo	
	u(i)=u(i)/a(l(i),i)
enddo

end subroutine
