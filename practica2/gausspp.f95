
subroutine gausspp(a,b,deter,l)

implicit none

real(8),dimension(:,:),intent(inout):: a !matriz de S.E.L
real(8),dimension(:),intent(inout)::b
real(8),intent(out):: deter
integer,dimension(:),intent(out)::l !permutacion de filas

integer::n !orden del S.E.L
integer:: i,j,k,p,m,li,lk
real(8)::piv,z

!inicializacion do determinante
n=size(b)
deter=1.
!Inicializacion de l
l=(/(i,i=1,n)/) !Equivalente a do i=1,n ;  l(i)=i   ;  end do

!etapa k-esima da eliminacion
do k=1,n-1
	!busqueda do pivote e da fila na que se atopa
	piv=a(l(k),k)
	p=k
	do i=k+1,n
		if(abs(piv)<abs(a(l(i),k)))then
			piv=a(l(i),k) !Novo pivote
			p=i !Fila na que encontramos el pivote, MAXIMO
		end if
	end do
	!comprobacion de que o k-esimo pivote non e nulo
	if(abs(piv)<1.e-12) then
		print*,'pivote nulo na etapa: ',k
		print*,'A matriz e singular!'
		stop
	end if
	!posta ao dia da permutacion
	deter=deter*piv
	if (p /= k) then !Equivalente en C: if(p!=k)
		deter = -deter !Cambiamos de signo porque realizamos un PERMUTACION
		m=l(k) !Actualizacion lo vector de situacion dos pivotes l
		l(k)=l(p)
		l(p)=m
	endif
	piv=a(l(k),k)
	lk=l(k) !Facilita o acceso, xa que non e necesario buscar no vector sempre que se precise este valor

	!Eliminación de Gauss (so que en lugar de elixir a fila i-esima, eliximos a fila l(i)-esima
	do i=k+1,n
		li=l(i) !Facilita o acceso, xa que non e necesario buscar no vector sempre que se precise este valor
		z=a(li,k)/piv
		do j=k+1,n
			a(li,j)=a(li,j)-z*a(lk,j)
		end do
		b(li)=b(li)-z*b(lk)
	end do
end do
!Remate do calculo do determinante.
piv=a(l(n),n)
deter=deter*piv
if(abs(piv)<1.e-12) then
	print*,'pivote nulo na etapa: ',n
	print*,'A matriz e singular!'
	stop
end if


end subroutine
