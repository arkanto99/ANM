program cholpe

implicit none
!Declaracion de variables y parametros
character(len=10)::formato4='(100e12.4)',formato10='(50e18.10)' !FORMATO

real(8),allocatable::a(:),s(:),x(:),z(:) !Diagonal principal y subsubdiagonal de A y B, respectivamente
real(8),allocatable::b(:),w(:),u(:) !Termo independente e Vectores necesarios para a resolucion dos sistema
real(8)::deter
integer:: n,i

print*,'Resolucion polo metodo de Cholesky Pentadiagonal modificado do sistema Au=b'
n=10
print*,'Orde do sistema',n


allocate(a(1:n),s(2:n),x(1:n),z(2:n),b(n),w(n),u(n))

a=1
s=-0.45
b=20

print*,'Diagonal principal'
print formato10,a
print*,'Subdiagonal segunda'
print formato10,s
print*, 'Termo independente'
print formato10,b

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!Factorizacion!!!!!!!!!!!!!!!!!
deter=1

if((abs(a(1))) .le. 1.e-12)then
	print*,'La matriz puede no ser definida positiva'
	stop
end if

x(1)=sqrt(a(1))
deter=deter*x(1)

if((abs(a(2))) .le. 1.e-12)then
	print*,'La matriz puede no ser definida positiva'
	stop
end if

x(2)=sqrt(a(2))
deter=deter*x(2)
do i=3,n	
	z(i-2)=s(i-2)/x(i-2)
	x(i)=sqrt(a(i)-z(i-2)**2)
	if(x(i) .le. 1.e-12)then
		print*,'La matriz puede no ser definida positiva'
		stop
	end if
	deter=deter*x(i)
end do

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!DESCENSO!!!!!!!!!!!!!!!!!!!!!!

w(1)=b(1)*x(1)
w(2)=b(2)*x(2)
do i=3,n
	w(i)=b(i)-z(i-2)*w(i-2)
	w(i)=w(i)/x(i)
end do

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!REMONTE!!!!!!!!!!!!!!!!!!!!!!!

u(n)=w(n)/x(n)
u(n-1)=w(n-1)/x(n-1)
do i=n-2,1,-1
	u(i)= w(i) - z(i)*u(i+2)
	u(i)= u(i)/x(i)
end do

print*,' '
print*,'Solucion do sistema'
print formato10,u
print*,'Determinante',deter
print*,' '
print*,'Vector x'
print formato10,x
print*, ' '
print*,'Vector z'
print formato10,z
deallocate(a,s,x,z,b,w,u)


end program


