



call lu(a,deter)

do i=1,n
	d(i)=a(i,i)
	
end do

do j=1,n
	e=b(:,j)
	do i=1,n
		a(i,i)=1
	end do
	call sistl(a,e,w)
	a(i,i)=d(i)
	do i=1,n
		a(i,i)=d(i)
	end do
	call sistu(a,w,u)
	b(:,j)=u
end do
