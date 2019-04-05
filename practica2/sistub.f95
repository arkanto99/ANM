
!METODO DE REMONTE OPTIMIZADO CON CALCULO VECTORIAL
subroutine sistub(a,b,u)

implicit none

integer,parameter:: clreal=selected_real_kind(p=15,r=307)
real(kind=clreal),dimension(:,:),intent(in)::a(:,:)
real(kind=clreal),dimension(:),intent(in)::b(:)
real(kind=clreal),allocatable,intent(out)::u(:)
integer::n,i,j

n=size(b) !No nos vale size(a), ya que nos daria n*n elementos

u=b
do i=n,1,-1
	u(i)=u(i)/a(i,i)
	u(1:i-1)=u(1:i-1)-a(1:i-1,i)*u(i)
end do

end subroutine sistub
