module gausspp_interf

interface
	subroutine gausspp(a,b,deter,l)
		real(8),dimension(:,:),intent(inout):: a !matriz de S.E.L
		real(8),dimension(:),intent(inout)::b
		real(8),intent(out):: deter
		integer,dimension(:),intent(out)::l !permutacion de filas	
	end subroutine
end interface

end module
