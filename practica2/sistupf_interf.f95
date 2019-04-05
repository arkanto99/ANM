module sistupf_interf

interface
	subroutine sistupf(a,b,u,l)
		real(8),dimension(:,:),intent(in):: a !matriz de S.E.L
		real(8),dimension(:),intent(in)::b		
		integer,dimension(:),intent(in)::l !permutacion de filas	
		real(8),dimension(:),intent(inout):: u
	end subroutine
end interface

end module
