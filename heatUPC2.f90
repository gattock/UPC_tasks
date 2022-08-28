program heat
implicit none

!constants (check if assign value of rho later allowed)
real*4, parameter :: tmax=10, lx=1.10 , ly=0.80, dx=0.1, dy=0.1, dt=0.1, Qtop=60.00, Tg=33.00, htc_left=9.00
integer, parameter :: nx=NINT(lx/dx) , ny=NINT(ly/dy), steps= NINT(tmax/dt)
real*4 ::  rho(nx,ny), cp(nx,ny), kappa(nx,ny), P(3,2)

!variables&indexes
real*4 :: T(nx,ny,steps), dQ, Qleft, time,x,y 
integer :: h,i,j,k

!assign spatial dimensions
P(1,:)=(0.50, 0.40) !valid syntax? CHECK!
P(2,:)=(0.50, 0.70)
P(3,:)=(lx, ly)

!assign parameters to material
do i=1,nx
    T(i,1,1)=23.00 !BC_bottom
do j=1,ny
    T(i,j,1)=8.00 !IC
    x=i*dx
    y=j*dy

    if (x<=P(1,1) .and. y<=P(1,2)) then
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


!TIME cycle
do h=2,steps
time=h*dt
    do i=1,nx
    do j=1,ny

        if (i>=2 .and. i<=nx-1 .and. j>=2 .and. j<=ny-1) then  !INNER domain:only full 4-directions Laplacian

dQ= ( T(i+1,j,h-1)+T(i-1,j,h-1) - 2*T(i,j,h-1) ) / (dx**2) +  ( T(i,j+1,h-1)+T(i,j-1,h-1) - 2*T(i,j,h-1) ) / (dy**2)
            T(i,j,h)=T(i,j,h-1)+dQ*kappa(i,j)/(rho(i,j)*cp(i,j)*dx*dy) !CHECK!
dQ=0.00

        else if (i==nx) then !RIGHT: Dirichlet(t)
            T(i,j,h)= 8.00 + 0.005*time !WHO HAVE CORNERS WITH 9POINTS SCHEME? SUPPOSE IT'S 5PS... HP

        else if (j==ny) then !TOP: 3-directions Laplacian + Qtop(i.e. heat flux)
dQ= ( T(i+1,j,h-1)+T(i-1,j,h-1) - 2*T(i,j,h-1) ) / (dx**2) +  (T(i,j-1,h-1) - T(i,j,h-1) ) / (dy**2) + Qtop
            T(i,j,h)=T(i,j,h-1)+dQ*kappa(i,j)/(rho(i,j)*cp(i,j)*dx*dy) !CHECK! !what if kappa not constant? must be averaged on each couple of elements?
dQ=0.00

        else if (i==1) then !LEFT: Robin: 3-dir Laplacian + Temperature dependent flux ____! could be done just with full Laplacian, but mind the conductivity
Qleft= ( Tg-T(i,j,h-1) )*htc_left !check heat direction (+- Qleft there->                            __   )
dQ= ( T(i+1,j,h-1) - T(i,j,h-1) ) / (dx**2) +  ( T(i,j+1,h-1)+T(i,j-1,h-1) - 2*T(i,j,h-1) ) / (dy**2)+ Qleft
            T(i,j,h)=T(i,j,h-1)+dQ*kappa(i,j)/(rho(i,j)*cp(i,j)*dx*dy) !CHECK!
dQ=0.00
        !else
        !dQ=0.00 !do nothing
        end if
        !bottom is already with Dirichlet BC=23°

    end do
    end do
end do



END program
!____________________________________________________________________