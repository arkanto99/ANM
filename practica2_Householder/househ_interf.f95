
module househ_interf

interface
	subroutine househ(a,b,deter)
		real(8),dimension(:,:),intent(inout)::a
		real(8),dimension(:),intent(inout)::b
		real(8),intent(out):: deter 
	end subroutine househ
end interface

end module
