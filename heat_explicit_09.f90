program heat
implicit none
                       
real*4, parameter :: tmax=6000, lx=1.10 , ly=0.80, dx=0.02 , dy=0.02, dt=0.01, Qtop=60.00, Tg=33.00, htc_left=9.00
integer, parameter :: nx=NINT(lx/dx) , ny=NINT(ly/dy), steps= NINT(tmax/dt)
real*4 ::  rho(ny,nx), cp(ny,nx), kappa(ny,nx),kappa_h(ny,nx,4), P(3,2)
real*4 :: T(ny,nx), T1(ny,nx), eps1(ny,nx,4),eps2(ny,nx,2), dQ, Qleft, time,x,y
integer :: h,i,j,k, iplot(3,2), t_scale=50, num
character(len=4) :: file_number !'heat_plate1000.csv'
character(len=8) :: fmt !format descriptor for data0001.csv, that is '(I4.4)'
character(len=18) :: file_name
! ny is the n of rows=n of items in 1 col(moving along y)
! nx is the n of cols=n of items in 1 row(moving along x)  
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
end do 
end do

do j=1,nx
do i=1,ny 
    if (i/=ny) then
kappa_h(i,j,1)=2/(1/kappa(i,j)+1/kappa(i+1,j)) !North1
    end if
    if (i/=1) then
kappa_h(i,j,2)=2/(1/kappa(i,j)+1/kappa(i-1,j)) !South2
    end if
    if (j/=nx) then
kappa_h(i,j,3)=2/(1/kappa(i,j)+1/kappa(i,j+1)) !East3
    end if
    if (j/=1) then
kappa_h(i,j,4)=2/(1/kappa(i,j)+1/kappa(i,j-1)) !West4
    end if
do k=1,2
    eps1(i,j,k)=rho(i,j)*cp(i,j)*dy**2/(kappa_h(i,j,k)*dt)     !*dy**(2) from Laplacian      
end do
do k=3,4
    eps1(i,j,k)=rho(i,j)*cp(i,j)*dx**2/(kappa_h(i,j,k)*dt)      !(dx**(2-1))*dx 
end do
eps2(i,j,1)=rho(i,j)*cp(i,j)*dy/dt  !*dx*dy/(dx*dt) !N-S-1
eps2(i,j,2)=rho(i,j)*cp(i,j)*dx/dt  !*dx*dy/(dy*dt) !E-W-2
end do
end do

            !call gutenberg(ny,nx,rho,  'rho         ')
            !call gutenberg(ny,nx,cp,   'cp          ')
            ! call gutenberg(ny,nx,kappa,'kappa       ')
            ! call gutenberg(ny,nx,kappa_h(:,:,1),  'kappa_h1N   ')
            ! call gutenberg(ny,nx,kappa_h(:,:,2),  'kappa_h2S   ')
            ! call gutenberg(ny,nx,kappa_h(:,:,3),  'kappa_h3E   ')
            ! call gutenberg(ny,nx,kappa_h(:,:,4),  'kappa_h4W   ')
            ! call gutenberg(ny,nx,eps1(:,:,1),  'eps1N       ')
            ! call gutenberg(ny,nx,eps1(:,:,2),  'eps2S       ')
            ! call gutenberg(ny,nx,eps1(:,:,3),  'eps3E       ')
            ! call gutenberg(ny,nx,eps1(:,:,4),  'eps4W       ')
            ! call gutenberg(ny,nx,eps2,         'eps2        ')

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
        write(2,201) (T(i,j),j=1,nx )
	            !200 FORMAT ('',13ES10.1E2) !scieeentifico
               201 FORMAT ('',F5.3)    !std::umano
   end do
close(2)
!------------------------------------------------------------------------------------------
num=1
                do h=2,steps           !TIME CYCLE
time=h*dt
!------------------------------------------------------------------------------------------
!CSV FOR PARAVIEW
! fmt='(I4.4)'
! if (mod(h,t_scale)==1) then 
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
 
    do i=1,nx
    do j=1,ny  
        if (i>1 .and. i<nx .and. j>1 .and. j<ny) then  !INNER domain:only full 4-directions Laplacian

dQ= (T(j+1,i)-T(j,i))/eps1(j,i,1) + (T(j-1,i)-T(j,i))/eps1(j,i,2) + &
    (T(j,i+1)-T(j,i))/eps1(j,i,3) + (T(j,i-1)-T(j,i))/eps1(j,i,4)

T1(j,i)=T(j,i)+dQ
dQ=0.00

        else if (j==ny .and. i/=1 .and. i/=nx) then !TOP: without corners, 3-directions Laplacian + Neumann Qtop(i.e. heat flux)

    !(T(j+1,i)-T(j,i))/eps1(j,i,1)
dQ=                                 (T(j-1,i)-T(j,i))/eps1(j,i,2) + &
    (T(j,i+1)-T(j,i))/eps1(j,i,3) + (T(j,i-1)-T(j,i))/eps1(j,i,4)

T1(j,i)=T(j,i)+dQ+ Qtop/eps2(j,i,1)
dQ=0.00

        else if (i==nx .and. j/=1) then !RIGHT: Dirichlet(t) with 1 corner

T1(j,i)= 8.00 + 0.005*time !WHO HAVE CORNERS WITH 9POINTS SCHEME? SUPPOSE IT'S 5PS... HP

        else if (i==1 .and. j/=1 .and. j/=ny) then !LEFT: without corners  3-dir Laplacian + Neumann Temperature dependent flux

Qleft= ( Tg-T(j,i) )*htc_left*dy*dt  

dQ= (T(j+1,i)-T(j,i))/eps1(j,i,1)  + (T(j-1,i)-T(j,i))/eps1(j,i,2) + &
    (T(j,i+1)-T(j,i))/eps1(j,i,3) !+ (T(j,i-1)-T(j,i))/eps1(j,i,4)

T1(j,i)=T(j,i)+dQ + Qleft/eps2(j,i,2)
dQ=0.00

        else if (j==1) then !BOTTOM with 2 corners
            T1(j,i)=23.00

        else if (i==1 .and. j==ny) then !TOP+LEFT corner remaining
   !(T(j+1,i)-T(j,i))/eps1(j,i,1)+
dQ=                               (T(j-1,i)-T(j,i))/eps1(j,i,2) + &
    (T(j,i+1)-T(j,i))/eps1(j,i,3) !+ (T(j,i-1)-T(j,i))/eps1(j,i,4)
Qleft= ( Tg-T(j,i) )*htc_left*dy
T1(j,i)=T(j,i)+dQ + Qleft/eps2(j,i,2) + Qtop/eps2(j,i,1)
dQ=0.00
        end if

    end do
    end do

!------------------------------------------------------------------------------------------
! output data into a file (so T1 not necessary)
if (mod(h,t_scale)==2) then 
   open(1, file = 'data.txt', Access='append', status='old')  
       write(1,*) time ,T(iplot(1,1),iplot(1,2)), T(iplot(2,1),iplot(2,2))  
   close(1) 

   open(2, file= 'data_array.txt', Access='append', status='old')

        do i=1,ny
            write(2,201) (T(i,j),j=1,nx )
        end do
	        !100 FORMAT ('',13ES10.1E2) !scieeentifico
            !101 FORMAT ('',13F10.2)    !std::umano
    close(2)
 end if 
!------------------------------------------------------------------------------------------
if (mod(h,t_scale*10)==2) then 
    print *, time/tmax*100 , '%'
    !print *, 'T(middle point)=', T(ny/2, nx/2)
    call gutenberg(ny,nx,T,    'T           ')

end if    
T(:,:)=0.0
T(:,:)=T1(:,:)
T1(:,:)=0.0
       
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
