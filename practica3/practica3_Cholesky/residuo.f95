subroutine residuo(a,b,u,r)

implicit none

real(8),dimension(:,:),intent(in):: a !matriz de S.E.L
real(8),dimension(:),intent(in)::b,u
real(8),dimension(:),intent(out)::r

integer::m,n,j


r=matmul(a,u)-b

end subroutine
