module residuo_interf

interface
	subroutine residuo(a,b,u,r)
		real(8),dimension(:,:),intent(in):: a !matriz de S.E.L
		real(8),dimension(:),intent(in)::b,u
		real(8),dimension(:),intent(out)::r !Residuo
	end subroutine
end interface

end module
