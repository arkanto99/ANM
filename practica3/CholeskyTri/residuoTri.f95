subroutine residuo(ad,au,al,v,b,r)

use dotTri

implicit none

real(8),dimension(:),intent(in):: ad, au, al !!ad= diagonal principal; au= diagonal superior; !al=diagonal inferior
real(8),dimension(:),intent(in)::v, b !v= resultado, b=termo independente orixinal
real(8),dimension(:),intent(inout)::r

call dotTri(ad,au,al,v,r)

r= r-b

end subroutine
