
program sumaMat

implicit none

integer::m,n,i,j
real,allocatable::A(:,:),B(:,:),S(:,:)

print*,"Introduce el tamaño de las matrices"
print*,"Numero de filas"
read*,m
print*,"Numero de columnas"
read*,n
allocate(A(m,n),B(m,n))

do i=1,m
	print*, 'Elementos de la fila',i,'de A?'
	read*,A(i,:)
end do
do i=1,m
	print*, 'Elementos de la fila',i,'de B?'
	read*,B(i,:)
end do

do i=1,m
	print'("Fila: ",i4,10f8.5)',i,a(i,1:n) !Para que funciones, debemos poner print si *, , ya que si no entra en conflicto con el formato
end do

do i=1,m
	print '("Fila: ",i4,10f8.5)',i,b(i,1:n)
end do

S=A+B
print*,'Matriz suma'
do i=1,m
	print'("Fila: ",i4,10f8.5)',i,s(i,1:n)
end do

deallocate(A,B,S)
end program
