	
module chol_interf

interface
	subroutine chol(a,deter)
		real(8),dimension(:,:),intent(inout)::a
		real(8),intent(out)::deter
	end subroutine
end interface

end module
