module gauss_interf

interface
	subroutine gauss(a,b,deter)
		real(8),dimension(:,:),intent(inout):: a !matriz de S.E.L
		real(8),dimension(:),intent(inout)::b
		real(8),intent(out):: deter
	end subroutine
end interface

end module
