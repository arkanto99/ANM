!El nombre de sistu procede se "system upper"

!Este programa resuelve sistemas triangulares superiores por el metodo de remomonte
subroutine sistu(a,b,u)

implicit none

integer,parameter:: clreal=selected_real_kind(p=15,r=307)
real(kind=clreal),dimension(:,:),intent(in)::a(:,:)
real(kind=clreal),dimension(:),intent(in)::b(:)
real(kind=clreal),allocatable,intent(out)::u(:)
integer::n,i,j

n=size(b) !No nos vale size(a), ya que nos daria n*n elementos


u(n)=b(n)/a(n,n)
do i=n-1,1,-1 !i variando de n-1 a 1, con paso DESCENDENTE de -1
	u(i)=b(i)
	do j=i+1,n
		u(i)=u(i)-a(i,j)*u(j)
	enddo
	u(i)=u(i)/a(i,i)
enddo

end subroutine sistu
