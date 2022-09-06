program convection_diffusion
implicit none

real*4, parameter :: pig=4.D0*DATAN(1.D0), dt=0.1, dx=0.1, dy=0.1, lx=2.0, ly=1.0, rho=1.0, gamma=0.5 ,tmax=10,Twest=10
integer, parameter :: nx=NINT(lx/dx) , ny=NINT(ly/dy), steps=NINT(tmax/dt)
integer :: h,i,j,k
real*4 :: T(ny,nx),T1(ny,nx), Ts(ny,nx,4), u(ny,nx,2),us(ny,nx,4), source,conv,diff,      &
out1(nx*ny,4),auxT, auxU ! Ts(n+1) because Staggered? 
T(:,:)=0.0  ; Ts(:,:,:)=0.0 ; u(:,:,:)=0.0; us(:,:,:)=0.0 ; source=0.0 ; out1(:,:)=0.0 ; auxT=0.0 ; auxU=0.0

call velocity_field(ny,nx,dy,dx,u)

    do i=1,ny ; do j=1,nx
T(i,j)= 1+ sin(real(i)/real(ny)*4*pig)+ sin(real(j)/real(nx)*4*pig)

out1(j+(i-1)*nx , 1)=j*dx  ;  out1(j+(i-1)*nx , 3)=u(i,j,1)
out1(j+(i-1)*nx , 2)=i*dy  ;  out1(j+(i-1)*nx , 4)=u(i,j,2)
    end do ; end do
call init_export_matrix (ny*nx, 4, out1, "1veloc_field")
!delete/clear out1?

do i=2,ny-1 ; do j=2,nx-1	!2:ny-1? 3:ny-2?	!u=(ux,uy)=(u,v)

do k=1,4 
!call UDS_pol(ny,nx,dy,dx, dt,u,T, i,j,k   ,auxT,auxU) !to update with outvarU!
call UDS_std (ny,nx, u,T, i,j, k ,auxT, auxU)
Ts(i,j,k)=auxT ; us(i,j,k)=auxU  ; end do

end do ; end do
call init_export_matrix (ny,nx, T(:,:), "2Temp_locale")
call init_export_matrix (ny+1,nx+1, Ts(:,:,1), "3T_upwind_DS")


        do h=1,steps !TIME CYCLE-----------


    do i=2,ny-1 ; do j=2,nx-1 !inner domain
conv= rho*(us(i,j,1)*Ts(i,j,1)*dx - us(i,j,2)*Ts(i,j,2)*dx + us(i,j,3)*Ts(i,j,3)*dy - us(i,j,4)*Ts(i,j,4)*dy )
diff= gamma*( (T(i+1,j)+T(i-1,j)-2*T(i,j))*dx/dy + (T(i,j+1)+T(i,j-1)-2*T(i,j))*dy/dx  )
call compute_source(ny,nx,h,steps,i,j,source)
T1(i,j)=T(i,j) +dt /(rho*dx*dy)*(diff-conv)+source*dt/rho
    end do ; end do 

do j=2,nx-1 ; i=ny !1-NORD-NEUMANN
conv= rho*(us(i,j,1)*T(i,j)*dx - us(i,j,2)*Ts(i,j,2)*dx + us(i,j,3)*Ts(i,j,3)*dy - us(i,j,4)*Ts(i,j,4)*dy )
diff= gamma*( (T(i-1,j)-T(i,j))*dx/dy + (T(i,j+1)+T(i,j-1)-2*T(i,j))*dy/dx  )
call compute_source(ny,nx,h,steps,i,j,source)   !T(i+1,j)+
T1(i,j)=T(i,j)+dt/(rho*dx*dy)*(diff-conv)+source*dt/rho
end do

do j=2,nx-1 ; i=1 !2-SUD-NO B.C.  !- us(i,j,2)*Ts(i,j,2)*dx)
conv= rho*(us(i,j,1)*Ts(i,j,1)*dx                           + us(i,j,3)*Ts(i,j,3)*dy - us(i,j,4)*Ts(i,j,4)*dy )
diff= gamma*( (T(i+1,j)               -T(i,j))*dx/dy + (T(i,j+1)+T(i,j-1)-2*T(i,j))*dy/dx  )
call compute_source(ny,nx,h,steps,i,j,source) !+T(i-1,j)-2*
T1(i,j)=T(i,j)+dt/(rho*dx*dy)*(diff-conv)+source*dt/rho
end do

do i=2,ny-1 ; j=nx !3-EST-NO B.C.                          !+ us(i,j,3)*Ts(i,j,3)*dy
conv= rho*(us(i,j,1)*Ts(i,j,1)*dx - us(i,j,2)*Ts(i,j,2)*dx                           - us(i,j,4)*Twest*dy )
diff= gamma*( (T(i+1,j)+T(i-1,j)-2*T(i,j))*dx/dy + (         T(i,j-1)-T(i,j))*dy/dx  )
call compute_source(ny,nx,h,steps,i,j,source)      ! T(i,j+1)+ ... -2*
T1(i,j)=T(i,j)+dt/(rho*dx*dy)*(diff-conv)+source*dt/rho
end do

do i=2,ny-1 ; j=1 !4-WEST-DIRICHLET
conv= rho*(us(i,j,1)*Ts(i,j,1)*dx - us(i,j,2)*Ts(i,j,2)*dx + us(i,j,3)*Ts(i,j,3)*dy - us(i,j,4)*Twest*dy )
diff= gamma*( (T(i+1,j)+T(i-1,j)-2*T(i,j))*dx/dy + (T(i,j+1)      -T(i,j))*dy/dx  )
call compute_source(ny,nx,h,steps,i,j,source) ! ! +T(i,j-1)-2*
T1(i,j)=T(i,j)+dt/(rho*dx*dy)*(diff-conv)+source*dt/rho
end do

i=1 ; j=1 ! SOUTH-WEST           !- us(i,j,2)*Ts(i,j,2)*dx)
conv= rho*(us(i,j,1)*Ts(i,j,1)*dx                           + us(i,j,3)*Ts(i,j,3)*dy - us(i,j,4)*Twest*dy )
diff= gamma*( (T(i+1,j)-T(i,j))*dx/dy + (T(i,j+1)-T(i,j))*dy/dx  )
call compute_source(ny,nx,h,steps,i,j,source)
T1(i,j)=T(i,j)+dt/(rho*dx*dy)*(diff-conv)+source*dt/rho

i=1 ; j=nx ! SOUTH-EST           !- us(i,j,2)*Ts(i,j,2)*dx) + us(i,j,3)*Ts(i,j,3)*dy
conv= rho*(us(i,j,1)*Ts(i,j,1)*dx                                                 - us(i,j,4)*Ts(i,j,4)*dy )
diff= gamma*( (T(i+1,j)-T(i,j))*dx/dy + (T(i,j-1)-T(i,j))*dy/dx  )
call compute_source(ny,nx,h,steps,i,j,source)
T1(i,j)=T(i,j)+dt/(rho*dx*dy)*(diff-conv)+source*dt/rho

i=ny ; j=1 !NORD-OVEST
conv= rho*(us(i,j,1)*T(i,j)*dx - us(i,j,2)*Ts(i,j,2)*dx + us(i,j,3)*Ts(i,j,3)*dy - us(i,j,4)*Twest*dy )
diff= gamma*( (T(i-1,j)-T(i,j))*dx/dy + (T(i,j+1)-T(i,j))*dy/dx  )
call compute_source(ny,nx,h,steps,i,j,source)
T1(i,j)=T(i,j)+dt/(rho*dx*dy)*(diff-conv)+source*dt/rho

i=ny ; j=nx !NORD-EST                                   !+ us(i,j,3)*Ts(i,j,3)*dy
conv= rho*(us(i,j,1)*T(i,j)*dx - us(i,j,2)*Ts(i,j,2)*dx                          - us(i,j,4)*Ts(i,j,4)*dy )
diff= gamma*( (T(i+1,j)+T(i-1,j)-2*T(i,j))*dx/dy + (T(i,j+1)+T(i,j-1)-2*T(i,j))*dy/dx  )
call compute_source(ny,nx,h,steps,i,j,source)
T1(i,j)=T(i,j)+dt/(rho*dx*dy)*(diff-conv)+source*dt/rho


T(:,:)=T1(:,:)
T1(:,:)=0.0
        end do !TIME CYCLE-------------

call init_export_matrix (ny,nx, T(:,:), "5Temp_result")


end program convection_diffusion
!______________________________________________________________________
!________________________________________________
SUBROUTINE velocity_field (ny,nx,dy,dx,u)
implicit none
integer,intent(in) :: ny,nx
real*4,intent(in) :: dy,dx
real*4,intent(out) :: u(ny,nx,2)
integer :: i,j
real*4 :: x,y
do i=1,ny
do j=1,nx
x=j*dx-1  ! x-shift
y=i*dy
u(i,j,1)=2.0*y*(1.0-(x)**2)
u(i,j,2)=-2.0*(x)*(1-y**2)
end do
end do

END SUBROUTINE velocity_field
!________________________________________________
!________________________________________________
SUBROUTINE compute_source (ny,nx,h,steps,i,j,source)
implicit none
integer,intent(in) :: ny,nx, h, steps,i,j
real*4,intent(out) :: source

source=0.0 !func(i/ny and j/nx and h/steps)

END SUBROUTINE compute_source
!________________________________________________
!________________________________________________
SUBROUTINE UDS_std (ny,nx, u,T, i,j,dir ,outvarT,outvarU)
implicit none
integer,intent(in) :: ny,nx,i,j,dir
real*4,intent(in) :: u(ny,nx,2),T(ny,nx)
real*4,intent(out) :: outvarT,outvarU

if      (u(i,j,2) >0 .and. dir==1) then ;     outvarT=T(i,j) ; outvarU=u(i,j,1)
else if (u(i,j,2)==0 .and. dir==1) then ;     outvarT=(T(i+1,j)+T(i,j))/2 ; outvarU=(u(i+1,j,1)+u(i,j,1))/2 
else if (u(i,j,2) <0 .and. dir==1) then ;     outvarT=T(i+1,j) ; outvarU=u(i+1,j,1)

else if (u(i,j,2) >0 .and. dir==2) then ;     outvarT=T(i-1,j) ; outvarU=u(i-1,j,1)
else if (u(i,j,2)==0 .and. dir==2) then ;     outvarT=(T(i-1,j)+T(i,j))/2 ; outvarU=(u(i-1,j,1)+u(i,j,1))/2
else if (u(i,j,2) <0 .and. dir==2) then ;     outvarT=T(i,j) ; outvarU=u(i,j,1)

else if (u(i,j,1) >0 .and. dir==3) then ;     outvarT=T(i,j) ; outvarU=u(i,j,2)
else if (u(i,j,1)==0 .and. dir==3) then ;     outvarT=(T(i,j)+T(i,j+1))/2 ; outvarU=(u(i,j,2)+u(i,j+1,2))/2
else if (u(i,j,1) <0 .and. dir==3) then ;     outvarT=T(i,j+1) ; outvarU=u(i,j+1,2)

else if (u(i,j,1) >0 .and. dir==4) then ;     outvarT=T(i,j-1) ; outvarU=u(i,j-1,2)
else if (u(i,j,1)==0 .and. dir==4) then ;     outvarT=(T(i,j-1)+T(i,j))/2 ; outvarU=(u(i,j-1,2)+u(i,j,2))/2
else if (u(i,j,1) <0 .and. dir==4) then ;     outvarT=T(i,j) ; outvarU=u(i,j,2)
end if
END SUBROUTINE UDS_std
!________________________________________________
SUBROUTINE UDS_pol (ny,nx,dy,dx, dt,u,T, i,j,dir ,outvarT,vel)
implicit none
real*4,parameter :: pig=4.D0*DATAN(1.D0)
integer,intent(in) :: ny,nx,i,j,dir
real*4,intent(in) :: dy,dx,dt, u(ny,nx,2),T(ny,nx)
real*4,intent(out) :: outvarT,vel
integer ::   h,k,n,alpha,beta, ind_y1,ind_y2,ind_x1,ind_x2, segno,scale1
real*4 :: au,bu,cu,u0,u1,u2, aT,bT,cT,T0,T1,T2, x1,x2, d_eps,eps, tol, err1,err0
tol=1e-4 ; scale1=20 !is the dissipation a function of scale1?
alpha=0 ; beta=0

if (dir<3) then ; d_eps=dy/scale1 ; segno= sign(1.0,u(i,j,1))
else if (dir >2 .and. dir<5) then ; d_eps=dx/scale1 ; segno= sign(1.0,u(i,j,2)) ; end if

x1=d_eps*0.5
x2=d_eps*1.5

alpha= NINT( 0.5*real(segno) + cos((dir-1)*pig/2)/2 ) * (2- (dir+1)/2)
ind_y1= alpha - 1*segno * (2- (dir+1)/2)
ind_y2= alpha - 2*segno * (2- (dir+1)/2)
beta  = NINT( 0.5*real(segno) + cos((dir+1)*pig/2)/2 ) * ((dir+1)/2  -1 )
ind_x1= beta - 1*segno *  ((dir+1)/2  -1 )
ind_x2= beta - 2*segno *  ((dir+1)/2  -1 )

u0=u(i,j,(dir+1)/2 )
u1=u(i+ ind_y1  , j+ ind_x1   ,(dir+1)/2 )
u2=u(i+ ind_y2  , j+ ind_x2   ,(dir+1)/2 )

au=( (u1-u0)*x2 - (u2-u0)*x1 ) / (x1**2 * x2 - x2**2 * x1)
bu=( (u1-u0)*x2**2 - (u2-u0)*x1**2 ) / ( x1*x2*(x2-x1) )
cu=u0

        n=0 ; vel=u0 ; eps=0; err1=1 ;err0=2;
do while ( abs(err1) > tol)
    n=n+1      
    eps= n* d_eps

    vel=au*eps**2 + bu*eps+ cu
    print *,"vel=", vel
    err0=err1
    err1=abs(vel*dt - eps)
        print *,"err=", err1

if (err1-err0>0) then ; EXIT ;end if
end do
print *,"n=",n
print *,"eps=", eps

!now eps is the distance to keep the value from
T0=T(i,j)
T1=T(i+ ind_y1 , j+ ind_x1  )
T2=T(i+ ind_y2  , j+ ind_x2  )
print *, "T0,T1,T2= ", T0 , T1 , T2

aT=( (T1-T0)*x2 - (T2-T0)*x1 ) / (x1**2 * x2 - x2**2 * x1)
bT=( (T1-T0)*x2**2 - (T2-T0)*x1**2 ) / ( x1*x2*(x2-x1) )
cT=T0
print *, "aT,bT,cT= ", aT , bT , cT
outvarT= aT * eps**2 + bT * eps+ cT !T(i,j,dir) 
print *, "T_UDS=f(T0,1,2)=",outvarT
END SUBROUTINE UDS_pol
!________________________________________________
!________________________________________________
SUBROUTINE print_matrix (nr,nc,mat)
implicit none
integer,intent(in) :: nr,nc
real, intent(in) :: mat(nr,nc)
integer :: i,j
do i=1,nr
    !print *, (mat(i,j), j=1,nc )
    write(*,80) (mat(i,j),j=1,nc )
    80 FORMAT ('',10F6.1)
end do
print *, '-------------------------------------------'
END SUBROUTINE print_matrix
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
!_______________________________________________
SUBROUTINE init_export_vector (n, v1)
implicit none
integer,intent(in) :: n
real, intent(in) :: v1(n)
integer :: j
call execute_command_line("rm data_vector.txt")
open(1, file = 'data_vector.txt', status='new')         
    write(1,80) (v1(j),j=1,n )
    80 FORMAT ('',10F6.1)
close(1)
END SUBROUTINE init_export_vector
!------------------------------------------------
SUBROUTINE do_export_vector (n, v1) !to call when if (mod(h,t_scale)==2) then 
implicit none
integer,intent(in) :: n
real, intent(in) :: v1(n)
integer :: j
open(1, file = 'data_vector.txt', Access='append', status='old')       
    write(1,80) (v1(j),j=1,n )
    80 FORMAT ('',10F6.1)
close(1)
END SUBROUTINE do_export_vector
!_______________________________________________
!_______________________________________________
!_______________________________________________
SUBROUTINE init_export_matrix (ny,nx, mat,name)
implicit none
character(len = 12),intent(in) :: name
integer,intent(in) :: ny,nx
real, intent(in) :: mat(ny,nx)
integer :: i,j
call execute_command_line("rm "//name//".txt")
open(2, file = name//'.txt', status='new')    
do i=1,ny     
    write(2,*) (mat(i,j), j=1,nx )
end do      !80
    80 FORMAT ('',10F6.1)
close(2)
END SUBROUTINE init_export_matrix
!------------------------------------------------
SUBROUTINE do_export_matrix (ny,nx, mat) !to call when if (mod(h,t_scale)==2) then 
implicit none
integer,intent(in) :: ny,nx
real, intent(in) :: mat(ny,nx)
integer :: i,j
open(2, file = 'data_matrix.txt', Access='append', status='old')       
do i=1,ny     
    write(2,*) (mat(i,j), j=1,nx )
end do
    80 FORMAT ('',10F6.1)
close(2)
END SUBROUTINE do_export_matrix
!_______________________________________________
!_______________________________________________