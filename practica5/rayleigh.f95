
program rayleigh

implicit none

integer::n,i,k,nmaxit
real(8),allocatable,dimension(:,:)::a
real(8),allocatable,dimension(:)::u,v,r
real(8):.eps,error,xlam1,xlam2,xn !xlam1=sigma(k), xlam2=sigma(k+1), xn=

!LECTURA DE DATOS
print*,'Introduza a dimension da matriz'
read*,n
print*,'Introduza a matriz por filas'
do i=1,n
	print*,"Fila ",i
	read*,a(i;:)
end do
print*,'Introduza a tolerancia do metodo'
read*,eps
print*,'Introduza o numero maximo de iterantes'
read*,nmaxit

!RESERVA DE MEMORIA
allocate(a(n,n),v(n),u(n),r(n))

!ALGORITMO DO METODO DA POTENCIA DE RAYLEIGH

u=0 
u(1)=1 !Tomamos u0=e1
v=matmul(a,u) !v1=A*u0
xlam1=sum(u*v) ! xlam1=sigma(0)=productoEscalar(u0,v1)
xn=sqrt(sum(v*v)) ! ||v1||
u=v/xn !u1=v1/||v1||

do k=2,nmaxit !Cambio de iteracion
	v=matmul(a,u) !V(k+1)=A*u(k)
	xlam2=sum(u*v) !sigma(k)=productoEscalar(u(k),v(k+1))
	err=abs(xlam2-xlam1)/(abs(xlam1)+1)! Calculo del error
	print*,'Iteracion',k
	print*,'Aprox autovalor',xlam2
	print*,'Erro relativo',err
	print*, 'Iterante u_k'
	print*,u
	if(err<eps)then !Test de parada
		print*,'Converxencia '	
		print*'Numero de iteracions',k
		print*,u
		print*,xlam2
		r=matmul(a,u)-xlam1*u !Calculo do residuo: r=Ap-lambda*p
		err=sqrt(sum(r*r))
		print*,'Residuo-escalar',err
		print*,'Residuo-vector'
		print*,r
		stop
	end if	
	xn=sqrt(sum(v*v)) 
	u=v/xn
	xlam1=xlam2
end do
print*,'Alcanzado numero maximo de iteracions',nmaxit
print*,'Error relativo',err
print*,'Ultimo valor obtido'.xlam2


end program
