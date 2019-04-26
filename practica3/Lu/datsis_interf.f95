
module datsis_interf

interface
	subroutine datsis(a,b)
		real(8),dimension(:,:),intent(out)::a
		real(8),dimension(:),intent(out)::b
	end subroutine datsis
end interface

end module
