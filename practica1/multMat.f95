
!Este programa imprie las matrices como una fila unica
program mulMat

implicit none

integer::m,n,i
real,allocatable::A(:,:),B(:,:),Q(:,:)

print*,"Introduce el tamaño de las matrices"
print*,"Numero de filas"
read*,m
print*,"Numero de columnas"
read*,n
allocate(A(m,n),B(m,n))

print*,"Introduce la matriz A por filas"
read*,A
print*,"Introduce la matriz B por filas"
read*,B
print*,"Las matrices dadas son"
print*,A
print*,B

Q=matmul(A,B)

print*,"Producto matricial de A y B: "
print*,Q
deallocate(A,B,Q)
end program
