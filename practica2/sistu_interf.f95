module sistu_interf

interface
	subroutine sistu(a,b,u)
		integer,parameter:: clreal=selected_real_kind(p=15,r=307)
		real(kind=clreal),dimension(:,:),intent(in)::a(:,:)
		real(kind=clreal),dimension(:),intent(in)::b(:)
		real(kind=clreal),dimension(:),intent(out)::u(:)
	end subroutine sistu	
end interface

end module
