program prova_index
implicit none
real*4,parameter :: pig=4.D0*DATAN(1.D0)
integer :: i,j,dir
integer :: alpha(2,4),beta(2,4), ind_y1(2,4),ind_y2(2,4),ind_x1(2,4),ind_x2(2,4), segno
alpha(:,:)=0 ; beta(:,:)=0

do dir=1,4 ; do segno= -1,1,2

j=3-int((real(segno)/2 +1.5))
!print*, "dir=",dir,"segno=",segno, "coeff1=", (2- (dir+1)/2) , "coeff2=",((dir+1)/2  -1 )
print*, "dir=",dir,"segno=",segno , "coeff2=",cos((dir-1)*pig/2)/2 
!, "coeff1=", 0.5*real(segno)

alpha(j,dir)= NINT( 0.5*real(segno) + cos((dir-1)*pig/2)/2 ) * (2- (dir+1)/2)
ind_y1(j,dir)= alpha(j,dir) - 1*segno * (2- (dir+1)/2)
ind_y2(j,dir)= alpha(j,dir) - 2*segno * (2- (dir+1)/2)
beta (j,dir) = NINT( 0.5*real(segno) + cos((dir+1)*pig/2)/2 ) * ((dir+1)/2  -1 )
ind_x1(j,dir)= beta(j,dir) - 1*segno *  ((dir+1)/2  -1 )
ind_x2(j,dir)= beta(j,dir) - 2*segno *  ((dir+1)/2  -1 )

end do ; end do
print *, "alpha"
call print_matrix(2,4,alpha)
print *, "index y1,y2"
call print_matrix(2,4,ind_y1)
call print_matrix(2,4,ind_y2)
print *, "beta"
call print_matrix(2,4,beta)
print *, "index x1,x2"
call print_matrix(2,4,ind_x1)
call print_matrix(2,4,ind_x2)

end program prova_index
!________________________________________________
SUBROUTINE print_matrix (nr,nc,mat)
implicit none
integer,intent(in) :: nr,nc
integer, intent(in) :: mat(nr,nc)
integer :: i,j
do i=1,nr
    !print *, (mat(i,j), j=1,nc )
    write(*,*) (mat(i,j),j=1,nc )
    80 FORMAT ('',10F6.1)
end do
print *, '-------------------------------------------'
END SUBROUTINE print_matrix
!________________________________________________