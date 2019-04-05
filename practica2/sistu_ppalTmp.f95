program sistu_ppal

implicit none

interface 
	subroutine sistu(a,b,u)
		integer,parameter:: clreal=selected_real_kind(p=15,r=307)
		real(kind=clreal),dimension(:,:),intent(in)::a(:,:)
		real(kind=clreal),dimension(:),intent(in)::b(:)
		real(kind=clreal),dimension(:),intent(out)::u(:)
	end subroutine sistu	

	subroutine sistub(a,b,u)
		integer,parameter:: clreal=selected_real_kind(p=15,r=307)
		real(kind=clreal),dimension(:,:),intent(in)::a(:,:)
		real(kind=clreal),dimension(:),intent(in)::b(:)
		real(kind=clreal),dimension(:),intent(out)::u(:)
	end subroutine sistub
end interface

integer,parameter:: clreal=selected_real_kind(p=15,r=307) !PRECISION, a veces puede no funcionar a la primera
character(len=10)::formato4='(100e12.4)',formato10='(50e18.10)' !FORMATO

real(kind=clreal),allocatable::a(:,:)
real(kind=clreal),allocatable::b(:),u(:)
integer:: n,i,j
!Variables para medir tempos de execucion
real:: dtime,tcpu,ta(2) !Tiempo de cpu(tcpu)=tiempos de usuario+ tiempo de sistema


do n=1000, 50000, 10000 !Tomamos matrices de diferentes tamanhos, 
	print*,'A orde do sistema e'
	read*,n
	!Reservamos a memoria para as variables dinamicas
	allocate(a(n,n))
	allocate(b(n),u(n))

	a=0 !Ponemos a matriz A a ceros

	!Enchemos o sistema 
	do i=1,n
		do j=i,n
			a(i,j)=j-i+1
		end do
		b(i)=(n-i+1)*(n-i+2)/2
	end do

	!Empregrando a subroutine Sistu
	call sistu(a,b,u)
	print*,
	print*,'O resultado u ,empregando Sistu (calculo normal) es:'
	print formato10,u

	u=0 !Poñemos o resultado a 0 de novo

	!Empregrando a subroutine Sistub
	tcpu=dtime(ta)
	call sistub(a,b,u)
	tcpu=dtime(ta)
	print*,
	print*,'O resultado u ,empregando Sistub (calculo vectorial) es:'
	print formato10,u

	!Liberamos memoria
	deallocate(a,b,u)
end do 

end program sistu_ppal




