	
module jacobi_interf

interface
	subroutine jacobi(a,b,u,eps,nmaxit)
		real(8),dimension(:,:),intent(in)::a
		real(8),dimension(:),intent(in)::b
		real(8),dimension(:),intent(out)::u
		real(8),intent(in)::eps
		integer,intent(in)::nmaxit
	end subroutine
end interface

end module
