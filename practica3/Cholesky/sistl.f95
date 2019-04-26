!El nombre de sistu procede se "system lower"

!Este programa resuelve sistemas triangulares superiores por el metodo de descenso
subroutine sistl(a,b,w)

implicit none

real(8),dimension(:,:),intent(in)::a
real(8),dimension(:),intent(in)::b
real(8),dimension(:),intent(out)::w
integer::n,i,j

n=size(b) !No nos vale size(a), ya que nos daria n*n elementos

w(1)=b(1)/a(1,1)
do i=2,n !i variando de 2 a n, con paso ASCENDENTE de 1
	w(i)=b(i)
	do j=1,i-1
		w(i)=w(i)-a(i,j)*w(j)
	enddo
	w(i)=w(i)/a(i,i)
enddo

end subroutine sistl
