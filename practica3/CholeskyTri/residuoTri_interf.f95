module residuoTri_interf

interface
	subroutine residuoTri(ad,au,al,v,b,r)
		real(8),dimension(:),intent(in):: ad, au, al !!ad= diagonal principal; au= 			diagonal superior; !al=diagonal inferior
		real(8),dimension(:),intent(in)::v, b !v= resultado, b=termo independente 			
		real(8),dimension(:),intent(inout)::r
	end subroutine
end interface

end module
