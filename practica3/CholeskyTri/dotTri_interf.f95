module dotTri_interf

interface
	subroutine dotTri(ad,au,al,v,w)
		real(8),dimension(:),intent(in):: ad, au, al !Diagonales
		real(8),dimension(:),intent(in)::v
		real(8),dimension(:),intent(out)::w !Resultado
	end subroutine
end interface

end module
