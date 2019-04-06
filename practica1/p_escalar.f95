program p_escalar

implicit none

integer::n,i
real,allocatable::v(:),w(:)
real::pe1,pe2,pe3

print*,"Introduce el tamaño de los vectores"
read*,n
allocate(v(n),w(n))
print*,"Introduce el vector v"
read*,v
print*,"Introduce el vector w"
read*,w
print*,"Los vectores dados son"
print*,v
print*,w

pe1=dot_product(v,w)
pe2=0.
do i=1,n
	pe2=pe2+v(i)*w(i)
end do
pe3=sum(v*w)
print*,"Producto escalar 1:",pe1
print*,"Producto escalar 2:",pe2
print*,"Producto escalar 3:",pe3
deallocate(v,w)
end program
