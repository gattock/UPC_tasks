program heat
implicit none

!constants (check if assign value of rho later allowed)
real*4, parameter :: tmax=10000, lx=1.10 , ly=0.80, dx=0.02 , dy=0.02, dt=1, Qtop=60.00, Tg=33.00, htc_left=9.00, tol=1e-3
integer, parameter :: nx=NINT(lx/dx) , ny=NINT(ly/dy), steps= NINT(tmax/dt)
real*4 ::  rho(ny,nx), cp(ny,nx), kappa(ny,nx), P(3,2)
! ny is the n of rows=n of items in 1 col(moving along y)
! nx is the n of cols=n of items in 1 row(moving along x)
!variables&indexes                                            !which   row     column
real*4 :: T(ny,nx), T_vec(ny*nx), eps(ny,nx), dQ, Qleft, time,x,y, AA(nx*ny , nx*ny+1), BB(nx*ny , nx*ny+1)
integer :: h,i,j,k, iplot(3,2), t_scale=10, count,num
character(len=4) :: file_number !'heat_plate1000.csv'
character(len=8) :: fmt !format descriptor for data0001.csv
character(len=18) :: file_name
call azzera_matrix(nx*ny,nx*ny+1,AA)
call azzera_matrix(nx*ny,nx*ny+1,BB)

print *, 'nx=',nx,'ny=', ny

!assign spatial dimensions
P(1,:)=(0.50, 0.40)
P(2,:)=(0.50, 0.70)
P(3,:)=(lx, ly)

!iplot(1,:)=(NINT(0.65/lx*nx) , NINT(0.56/ly*ny))   !WHY NOT ALLOWED WITH INTEGERS?
!iplot(2,:)=(NINT(0.74/lx*nx) , NINT(0.72/ly*ny))
!iplot(3,:)=(NINT(0.50/lx*nx) , NINT(0.50/ly*ny))
iplot(1,1)=NINT(0.65/lx*nx)
iplot(1,2)=NINT(0.56/ly*ny)
iplot(2,1)=NINT(0.74/lx*nx) 
iplot(2,2)=NINT(0.72/ly*ny)
iplot(3,1)=NINT(0.50/lx*nx)
iplot(3,2)=NINT(0.50/ly*ny)

!assign parameters to material
do j=1,nx                   !which column? j moves along the row of nx items
    T(1,j)=23.00 !BC_bottom
do i=1,ny                   !which row? i moves along the column of ny items
        if (i /= 1) then
    T(i,j)=8.00 !IC
        end if

    x=j*dx
    y=i*dy

    if (x<=P(1,1) .and. y<=P(1,2)) then   !remove /1e3 !!
        rho(i,j)= 1500.00
        cp(i,j)= 750.00
        kappa(i,j)= 170.00
    else if (x<=P(1,1) .and. y>P(1,2)) then
        rho(i,j)= 1900.00 
        cp(i,j)= 810.00 
        kappa(i,j)= 200.00
    else if (x>P(1,1) .and. y<=P(2,2)) then
        rho(i,j)= 1600.00 
        cp(i,j)= 770.00 
        kappa(i,j)= 140.00
    else if (x>P(1,1) .and. y>P(2,2)) then
        rho(i,j)=  2500.00 
        cp(i,j)= 930.00 
        kappa(i,j)= 140.00
    end if
eps(i,j)=rho(i,j)*cp(i,j)*dx*dy/(kappa(i,j)*dt)
end do 
end do

            !call gutenberg(ny,nx,rho,  'rho         ')
            !call gutenberg(ny,nx,cp,   'cp          ')
            !call gutenberg(ny,nx,kappa,'kappa       ')
            !call gutenberg(ny,nx,eps,  'eps         ')
            !call gutenberg(ny,nx,T,    'T           ')

            call execute_command_line ("rm data.txt data_array.txt", exitstat=i)
h=1
time=h*dt
!output data into a file-----------------------------------------------------------------------
open(1, file = 'data.txt', status='new')        
        write(1,*) time ,T(iplot(1,1),iplot(1,2)), T(iplot(2,1),iplot(2,2))  
close(1) 

open(2, file= 'data_array.txt', status='new')
        !write(2,*) T(:,:)                       !DA ESPLICITARE METODO DI STAMPA: chiama Gutemberg
   do i=1,ny
        !write(2,101) (T(i,j),j=1,nx )
        write(2,*) (T(i,j),j=1,nx )
	            !200 FORMAT ('',13ES10.1E2) !scieeentifico
                !201 FORMAT ('',F5.3)    !std::umano
   end do
close(2)
!------------------------------------------------------------------------------------------num=1
                do h=2,steps            !TIME CYCLE
time=h*dt
!------------------------------------------------------------------------------------------
!CSV FOR PARAVIEW
! fmt='(I4.4)'
! if (mod(h,t_scale)==2) then 
! num=num+1
! write(file_number,fmt) num
! file_name='heat_plate'//trim(file_number)//'.csv'

! open(3, file=file_name , status='new')
!     write(3,*) 'x,y,z,T'
! do i=1,ny
! do j=1,nx
!     x=j*dx
!     y=i*dy
!     write(3,*) x,',',y,',',0,',',T(i,j)
! end do 
! end do
! close(3)
! !call execute_command_line("mv "//file_name//" /PV", exitstat=i)
! end if
!------------------------------------------------------------------------------------------
 
    do j=1,nx  
    do i=1,ny  
                                            !call gutenberg(nx*ny,nx*ny+1,AA,'AA          ')
        if (i>1 .and. i<ny .and. j>1 .and. j<nx) then  !INNER domain:only full 4-directions Laplacian

AA(j+(i-1)*nx , j+(i-1)*nx )= eps(i,j)+ 2.00/(dx**2) + 2.00/(dy**2)

AA(j+(i-1)*nx , j+(i-1)*nx +1 )= -1.00/(dx**2)
AA(j+(i-1)*nx , j+(i-1)*nx -1 )= -1.00/(dx**2)
AA(j+(i-1)*nx , j+(i-2)*nx )= -1.00/(dy**2)
AA(j+(i-1)*nx , j+(i-0)*nx )= -1.00/(dy**2)

AA(j+(i-1)*nx , ny*nx +1 )= eps(i,j)*T(i,j)

        else if (i==1) then !BOTTOM Dirichlet BC=23°

AA(j+(i-1)*nx , j+(i-1)*nx )=1.00
AA(j+(i-1)*nx , ny*nx+1 )=23.00

        else if (i==ny) then !TOP: 3-directions Laplacian + Neumann Qtop(i.e. heat flux)

AA(j+(i-1)*nx , j+(i-1)*nx )= eps(i,j)+ 2.00/(dx**2) + 1.00/(dy**2)

AA(j+(i-1)*nx , j+(i-1)*nx +1 )= -1.00/(dx**2)
AA(j+(i-1)*nx , j+(i-1)*nx -1 )= -1.00/(dx**2)

AA(j+(i-1)*nx , j+(i-2)*nx )= -1.00/(dy**2)
AA(j+(i-1)*nx , j+(i-0)*nx )= 0.00 !the upper one is zero

AA(j+(i-1)*nx , ny*nx+1 )= eps(i,j)*T(i,j) + Qtop/kappa(i,j) !check formula!


        else if (j==nx) then !RIGHT: Dirichlet(t)
!T(i,j,h)= 8.00 + 0.005*time !WHO HAVE CORNERS WITH 9POINTS SCHEME? SUPPOSE IT'S 5PS... HP

AA(j+(i-1)*nx , j+(i-1)*nx )=1.00
AA(j+(i-1)*nx , ny*nx+1 )= 8.00+5.00*time/1000.00


        else if (j==1) then !LEFT: 3-dir Laplacian + Neumann Temperature dependent flux ____! could be done just with full Laplacian, but mind the conductivity
Qleft=dy*htc_left*(Tg-T(i,j))*dt

AA(j+(i-1)*nx , j+(i-1)*nx )= eps(i,j)+ 1.00/(dx**2) + 2.00/(dy**2)

AA(j+(i-1)*nx , j+(i-1)*nx +1 )= -1.00/(dx**2)
AA(j+(i-1)*nx , j+(i-1)*nx -1 )= 0.00 !the left one is zero

AA(j+(i-1)*nx , j+(i-2)*nx )= -1.00/(dy**2)
AA(j+(i-1)*nx , j+(i-0)*nx )= -1.00/(dy**2) 

AA(j+(i-1)*nx , ny*nx+1 )= eps(i,j)*T(i,j) + Qleft/kappa(i,j) !check formula!

        end if

    end do
    end do
            call array_to_vec(ny,nx,T,T_vec)
            !call gutenberg(ny*nx , ny*nx+1, AA,'AA          ')             !!!EXTRAPRINT
            !call gutenberg(ny*nx, 1 , T_vec,   'T_vec(guess)')
            call gauss_seidel(ny*nx,AA, T_vec,tol,count,T_vec)
            !call gutenberg(nx*ny, 1,     T_vec,'T_solved    ') 
            !print *, T_vec
            call solved_LS_to_array ( ny*nx , T_vec,    ny,nx,T )
            !call gutenberg(ny,nx, T,           'T           ')                !!!EXTRAPRINT
!------------------------------------------------------------------------------------------
! output data into a file (so T1 not necessary)
if (mod(h,t_scale)==2) then 
   open(1, file = 'data.txt', Access='append', status='old')  
       write(1,*) time ,T(iplot(1,1),iplot(1,2)), T(iplot(2,1),iplot(2,2))  
   close(1) 

   open(2, file= 'data_array.txt', Access='append', status='old')

        do i=1,ny
            write(2,*) (T(i,j),j=1,nx )
        end do
	        !100 FORMAT ('',13ES10.1E2) !scieeentifico
            !101 FORMAT ('',13F10.2)    !std::umano
    close(2)
 end if 
!------------------------------------------------------------------------------------------
if (mod(h,t_scale)==2) then 
    print *, time/tmax*100 , '%'
    print *,'n_cycles=' ,count
    print *, 'T(middle point)=', T(ny/2, nx/2)
end if   
            call azzera_matrix(ny*nx , ny*nx+1, AA)
            call azzera_matrix(ny*nx , ny*nx+1, BB)
            
        end do !end time cycle (BRING INSIDE TIME LOOP THE OUTPUT TO TXT)
END program
!____________________________________________________________________
!____________________________________________________________________
!____________________________________________________________________
!____________________________________________________________________
!____________________________________________________________________
!____________________________________________________________________
!____________________________________________________________________
!____________________________________________________________________
!call solved_LS_to_array ( ny*nx , T_vec(nx*ny),    ny,nx,T )
SUBROUTINE solved_LS_to_array ( a , LS, ny, nx, T )
implicit none
integer,intent(in) :: a,ny,nx
real*4, intent(in) :: LS(a)
real*4, intent(out) :: T(ny,nx)
integer :: i,j
do j=1,nx
do i=1,ny
            T(i,j)=LS(j+(i-1)*nx)              !!! (i+(j-1)*ny) OR j+(i-1)*nx ? CHECK!!!
end do
end do
END SUBROUTINE solved_LS_to_array
!________________________________________________
SUBROUTINE gutenberg (ny,nx,mat,name)
implicit none
character(len = 12),intent(in) :: name
integer,intent(in) :: nx,ny
real, intent(in) :: mat(ny,nx)
integer :: i,j
print *, '---------------------------------------------------------'
print *, '-         ',name,     '                                 -'
print *, '---------------------------------------------------------'
do i=1,ny
	!print *, (mat(i,j), j=1,nc )
	write(*,101) (mat(i,j),j=1,nx )
	100 FORMAT ('',13ES10.1E2)
    101 FORMAT ('',13F10.2)
end do
END SUBROUTINE gutenberg

!________________________________________________
SUBROUTINE print_vector (n,vector )
implicit none
integer,intent(in) :: n
real, intent(in) :: vector(n)
integer :: j

	write(*,80) (vector(j),j=1,n )
	80 FORMAT ('',10F6.1)
print *, '-------------------------------------------'
END SUBROUTINE print_vector
!_______________________________________________
!________________________________________________
SUBROUTINE azzera_matrix (ny,nx,B)
implicit none
integer,intent(in) :: ny,nx
!real, intent(in) :: A(ny,nx)
real, intent(out) :: B(ny,nx)
integer :: i,j
do i=1,ny
do j=1,nx
B(i,j)=0.00
end do
end do
END SUBROUTINE azzera_matrix
!_______________________________________________
!call array_to_vec(ny,nx,T,T_vec)
SUBROUTINE array_to_vec(ny,nx,arr,vec)
implicit none
integer,intent(in) :: ny,nx
real, intent(in) :: arr(ny,nx)
real, intent(out) :: vec(ny*nx)
integer :: i,j
do i=1,ny
do j=1,nx
vec(j+(i-1)*nx)=arr(i,j)
end do
end do
END SUBROUTINE array_to_vec
!________________________________________________
SUBROUTINE gauss_seidel(n,LS,X0,tol,count,X1)
implicit none
integer,intent(in) :: n 
real, intent(in) :: LS(n,n+1),tol, X0(n)
real :: L(n,n), U(n,n), X(n), X1(n), sum1,sum2, err, err_vec(n)
integer,intent(out) :: count
integer :: i,j

do i=1,n
U(1:i-1,i)=LS(1:i-1,i)
L(i:n  ,i)=LS(i:n  ,i)
end do


        !call gutenberg(n,n+1,LS,'LS          ')
        !call gutenberg(n,n,U,   'Upper U     ')
        !call gutenberg(n,n,L,   'Lower L     ')

err=10
count=0
X(:)=X0(:)

                    do while (err>tol) ! whileeeeee
count=count+1
X1(:)=0
do i=1,n
sum1=0
sum2=0
    do j=1,i-1
        sum1=sum1+LS(i,j)*X1(j)
    end do
    do j=i+1,n
        sum2=sum2+LS(i,j)*X(j)
    end do
X1(i)=(LS(i,n+1)-sum1 -sum2)/LS(i,i)
end do

err_vec(:)=0
err_vec(:)=X1(:)-X(:)
err=0
    do i=1,n
err=err+err_vec(i)**2
    end do
err=(err**0.5)/n

                !call gutenberg(n,1,X,'X           ')
    !print *, 'err=', err
X(:)=X1(:)
                    end do !while cycle

END SUBROUTINE gauss_seidel
!________________________________________________