#include <iostream>
#include <fstream>
#include <math.h>
#include <string>
#include <cstring>
#include <vector>
#include <cmath>
//#define type1 "ny, std::vector<double> (nx, 0.0)"

//using namespace std;
//______________________________________________________________________________________________
//______________________________SUBROUTINES BEGIN_______________________________________________
//______________________________________________________________________________________________
void gutenberg (int ny, int nx, std::vector<std::vector<double>> mat, const char *name){
int i,j;

std::cout << "---------------------------------------------------------------\n-----------------------------";
for (size_t i = 0; i < strlen(name); i++) {
    // Access each char in the string
std::cout << name[i] ;
}
std::cout << "-----------------------------" << std::endl <<"---------------------------------------------------------------"<< std::endl ;
std::cout << mat.size() << " x "<< mat[0].size() << std::endl ;

for (i=0 ; i<ny ; i++) {
for (j=0 ; j<nx ; j++) {
  std::cout << mat[i][j] << " ";
} std::cout << std::endl ;
}}//______________________________________________________________________________________________

//______________________________________________________________________________________________


//______________________________________________________________________________________________
//______________________________SUBROUTINES END_________________________________________________
//______________________________________________________________________________________________
int main() {
//DATA EXPORT stuff
std::system("rm data.txt");
std::system("touch data.txt");
std::ofstream myfile;
char nombre[200];
sprintf(nombre,"data.txt");
//DECLARATIONS
int h,i,j,k, num  ;
double dQ, Qleft, time,x,y;
const double /*pi= 2*acos(0.0),*/ Lx=1.1, dx=0.01, Ly=0.8, dy=0.01 , dt=0.01, Qtop=60.00, Tg=33.00, htc_left=9.00;
const int tmax=5100 , nx=round(Lx/dx) , ny=round(Ly/dy) ,steps=round(tmax/dt), t_scale=1E4;
double P[3][2]={{0.5 , 0.4 },{0.5 , 0.7 },{Lx  , Ly  }};

int iplot[3][2]={   {round(0.65/Lx*(double)nx) , round(0.56/Ly*(double)ny) },
                    {round(0.74/Lx*(double)nx) , round(0.72/Ly*(double)ny) },
                    {round(0.50/Lx*(double)nx) , round(0.50/Ly*(double)ny) } };

//type1="ny, std::vector<double> (nx, 0.0)" -> rho(type1)  ??

std::vector<std::vector<double>>    rho(ny, std::vector<double> (nx, 0.0)),
                                    cp(ny, std::vector<double> (nx, 0.0)),
                                    kappa(ny, std::vector<double> (nx, 0.0)),
                                    T(ny, std::vector<double> (nx, 0.0)),
                                    T1(ny, std::vector<double> (nx, 0.0)) ;

std::vector<std::vector<std::vector<double>>>kappa_h(ny, std::vector<std::vector<double>>(nx, std::vector<double>(4,0.0))),
                                                eps1(ny, std::vector<std::vector<double>>(nx, std::vector<double>(4,0.0))),
                                                eps2(ny, std::vector<std::vector<double>>(nx, std::vector<double>(4,0.0)));

// declare strings:: file_number, fmt,filename ;
//print nx,ny, other param.

  std::cout << "nx="<< nx << "      ny="<< ny << std::endl ;


    //assign material parameters        // C++ is ROW major based
for (j=0 ; j<nx ; j++) { 

  T[0][j]=23.0 ; //BC bottom

for (i=0 ; i<ny ; i++) {   //n-1??
    if (i != 0) {
        T[i][j]=8.0 ;  //IC
}   x=j*dx ; y=i*dy ;

    if (x<=P[0][0] && y<=P[0][1]) {
        rho[i][j]=1500.0 ;
        cp[i][j]=750.0   ;
        kappa[i][j]=170.0;
}   else if (x<=P[0][0] && y>P[0][1]) {
        rho[i][j]=1900.0 ;
        cp[i][j]=810.0   ;
        kappa[i][j]=200.0;
}   else if (x>P[0][0] && y<=P[1][1]) {
        rho[i][j]=1600.0 ;
        cp[i][j]=770.0   ;
        kappa[i][j]=140.0;
}   else if (x>P[0][0] && y>P[1][1]) {
        rho[i][j]=2500.0 ;
        cp[i][j]=930.0   ;
        kappa[i][j]=140.0;
}}} //end "if,i,j"
gutenberg(ny,nx,T,"Temperature");
//gutenberg(ny,nx,rho, "rho"); gutenberg(ny,nx,cp, "cp"); gutenberg(ny,nx,kappa, "kappa");


for (j=0 ; j<nx ; j++) {
for (i=0 ; i<ny ; i++) {
    if ( i != ny-1) {
        kappa_h[i][j][0]=2/(1/kappa[i][j]+1/kappa[i+1][j]); //0=Nord
}   if ( i != 0) {
        kappa_h[i][j][1]=2/(1/kappa[i][j]+1/kappa[i-1][j]); //1=Sud
}   if ( j != nx-1) {
        kappa_h[i][j][2]=2/(1/kappa[i][j]+1/kappa[i][j+1]); //2=Est
}   if ( j != 0) {
        kappa_h[i][j][3]=2/(1/kappa[i][j]+1/kappa[i][j-1]); //3=Ovest
}   for ( k=0 ; k<2 ; k++) {
        eps1[i][j][k]  =rho[i][j]*cp[i][j]*pow(dy,2)/(kappa_h[i][j][k]*dt)   ;
        eps1[i][j][k+2]=rho[i][j]*cp[i][j]*pow(dx,2)/(kappa_h[i][j][k+2]*dt) ;
}   eps2[i][j][0]=rho[i][j]*cp[i][j]*dy/dt  ;  //!*dx*dy/(dx*dt) !N-S-1
    eps2[i][j][1]=rho[i][j]*cp[i][j]*dx/dt  ;  //!*dx*dy/(dy*dt) !E-W-2
}}

//gutenberg(ny,nx,kappa_h[][][0], "rho");
        //-----------------------------------------------------------------
        //missing lines 66->89 for printing and exporting data to txt
        //-----------------------------------------------------------------
num=1;
for (h=1 ; h<steps-1 ; h++) { //TIME CYCLE BEGIN
    time=h*dt;
        //-----------------------------------------------------------------
        //missing lines 93->113 to export .csv files
        //-----------------------------------------------------------------
  for (i=0 ; i<ny ; i++) {
  for (j=0 ; j<nx ; j++) {

    if (i>0 && i<ny-1 && j>0 && j<nx-1) {
        dQ= (T[i+1][j]-T[i][j])/eps1[i][j][0] + (T[i-1][j]-T[i][j])/eps1[i][j][1] +
            (T[i][j+1]-T[i][j])/eps1[i][j][2] + (T[i][j-1]-T[i][j])/eps1[i][j][3]  ;
        T1[i][j]=T[i][j]+dQ ;
        dQ=0.0 ;

}   else if (i==ny-1 && j!=nx-1 && j!=0 ) { //TOP without corners
        //  (T[i+1][j]-T[i][j])/eps1[i][j][0]
        dQ=                                   + (T[i-1][j]-T[i][j])/eps1[i][j][1] +
            (T[i][j+1]-T[i][j])/eps1[i][j][2] + (T[i][j-1]-T[i][j])/eps1[i][j][3]  ;
        T1[i][j]=T[i][j]+dQ + Qtop/eps2[i][j][0] ;
        dQ=0.0 ;

}   else if (j==nx-1 && i!=0) {  //RIGHT with upper corner
        T1[i][j]= 8.0 + 0.005*time      ;

}   else if (j==0 && i!=0 && i!=ny-1 ) {  //LEFT without corners
        Qleft= (Tg-T[i][j])*htc_left*dy*dt ;

        dQ= (T[i+1][j]-T[i][j])/eps1[i][j][0] + (T[i-1][j]-T[i][j])/eps1[i][j][1] +
            (T[i][j+1]-T[i][j])/eps1[i][j][2] ;//+ (T[i][j-1]-T[i][j])/eps1[i][j][3]
        T1[i][j]=T[i][j]+dQ + Qleft/eps2[i][j][1] ;
        dQ=0.0;

}   else if (i==0 /*&& j!=0 && j!=nx-1*/) { //BOTTOM with both corners
        T1[i][j]=23.0;
}   else if (j==0 && i==ny-1) { //TOP+LEFT corner only
        Qleft= (Tg-T[i][j])*htc_left*dy ;
        dQ= /*(T[i+1][j]-T[i][j])/eps1[i][j][0]*/ + (T[i-1][j]-T[i][j])/eps1[i][j][1] +
              (T[i][j+1]-T[i][j])/eps1[i][j][2];//+ (T[i][j-1]-T[i][j])/eps1[i][j][3]
        T1[i][j]=T[i][j]+dQ + Qleft/eps2[i][j][1] + Qtop/eps2[i][j][0] ;
        dQ=0.0;

}}} //end "if,i,j"

//OUTPUT DATA TO FILES
if (h%(t_scale)==1) {
        //std::cout << time/tmax*100 << "%" << std::endl ;
        //gutenberg(ny,nx,T, "T")  ;
        std::cout << time<< "[s]" << std::endl ;
        
        myfile.open(nombre, std::ios::app);
        myfile << time << " " << T[iplot[0][0]][iplot[0][1]]<< " "<< T[iplot[1][0]][iplot[1][1]]<< std::endl ;
        myfile.close();
}

for (i=0 ; i<ny ; i++) {
for (j=0 ; j<nx ; j++) {
        T[i][j]=T1[i][j]   ;  // CAN BE SUBSTITUTED WITH?    T.clear(); T1.clear()
        T1[i][j]=0.0       ;
}}

} //TIME CYCLE END
                                                std::cout << "A--" << std::endl ;
return 0        ;       } //_______________________________________________