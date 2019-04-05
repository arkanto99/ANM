
module datsissim_interf

interface
	subroutine datsissim(a,b)
		character(len=10)::formato4='(100e12.4)',formato10='(50e18.10)' !FORMATO
		real(8),dimension(:,:),intent(out)::a(:,:)
		real(8),dimension(:),intent(out)::b(:)
		integer::n,i,j
	end subroutine
end interface

end module
