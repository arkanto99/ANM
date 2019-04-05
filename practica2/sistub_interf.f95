module sistub_interf

interface
	subroutine sistub(a,b,u)
		integer,parameter:: clreal=selected_real_kind(p=15,r=307)
		real(kind=clreal),dimension(:,:),intent(in)::a(:,:)
		real(kind=clreal),dimension(:),intent(in)::b(:)
		real(kind=clreal),dimension(:),intent(out)::u(:)
	end subroutine sistub
end interface

end module
