
module lu_interf

interface
	subroutine lu(a,deter)
		real(8),dimension(:,:),intent(inout)::a
		real(8),intent(out)::deter
	end subroutine
end interface

end module
