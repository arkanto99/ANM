
module crout_interf

interface
	subroutine crout(a,deter)
		real(8),dimension(:,:),intent(inout)::a
		real(8),intent(out)::deter
	end subroutine crout
end interface

end module
