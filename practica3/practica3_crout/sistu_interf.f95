module sistu_interf

interface
	subroutine sistu(a,b,u)
		real(8),dimension(:,:),intent(in)::a
		real(8),dimension(:),intent(in)::b
		real(8),dimension(:),intent(inout)::u
	end subroutine sistu	
end interface

end module
