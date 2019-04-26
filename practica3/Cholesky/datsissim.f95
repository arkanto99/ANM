
!Subroutine de LECTURA DE DATOS PARA MATRICES TRIANGULARES
!ESPECIFICO PARA CHOLESKY
!NOTA: AHORA MISMO ESTA PARA MATRICES TRIANGULARES SUPERIORES;
!Para inferiores, cambiar  la lectura como aparece comentado:
subroutine datsissim(a,b)

implicit none 

character(len=10)::formato4='(100e12.4)',formato10='(50e18.10)' !FORMATO
real(8),dimension(:,:),intent(out)::a
real(8),dimension(:),intent(out)::b
integer::n,i,j

n=size(b) !No nos vale size(a), ya que nos daria n*n elementos

a=0 !Ponemos a matriz A a ceros

!Lectura da matriz A ESPECIFICA PARA CHOLESKY
print*,'Introduza os elementos non nulos da matriz A'
do i=1,n
	print*,"Fila ",i
	read*,a(i,i:n) !para inferiores: a(i,1:i)
	a(i:n,i)=a(i,i:n) !para inferiores: a(1:i,i)=a(i,1:i)
end do
print*,"La matriz triangular A es: "
do i=1,n
	print formato4,a(i,:) !FORMATO
end do

!Lectura da matriz b
print*,'Introduza os elementos de b, termo independente '
read*,b
print*,'O termo independiente es: '
print formato10,b

end subroutine
