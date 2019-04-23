
module datsissim_interf

interface
	subroutine datsissim(a,b)
		real(8),dimension(:,:),intent(out)::a
		real(8),dimension(:),intent(out)::b
	end subroutine datsissim
end interface

end module
