module sistl_interf

interface
	subroutine sistl(a,b,u)
		real(8),dimension(:,:),intent(in)::a
		real(8),dimension(:),intent(in)::b
		real(8),dimension(:),intent(out)::u
	end subroutine sistl	
end interface

end module
