#include <iostream>
#include <string>
using namespace std;

//_________________________________________________
float copy(float obj[], int Io, int Fo, float target[], int It, int Ft ) {
int i;
        if (Fo-Io == Ft-It) {
for (i=0 ; i<Ft-It ; i++) {
    target[It+i]=obj[Io+i];
            } } else 
{cout << "Sizes don't match";     }

return target ; }
//_________________________________________________
//________________________________________________
float InitCond(int n , float pi ) {
    int i ;
    float u0[n];

    for (i=0 ; i<=n ; i++) {   //n-1??
        //u0[i]= 0.001*pow(i,2) - 0.2*i + 4
        u0[i] = sin(2*pi*i/n);
                            }
return u0   ;    }
//______________________________________________________
//with PBC
float compute_ux_uxx( int n , float u[n], float h) {
float ux[n];
//------------------------1st order
for (i=0 ; i<=n ; i++ ) {
    ux[i] = (u[round(fmod(i+1,n))]-u[round(fmod(i-1,n)])/(2*h);
}
//-------------------------2nd order
for (i=0 ; i<=n ; i++ ) {
    uxx[i] = (u[round(fmod(i+1,n))]+u[round(fmod(i-1,n)]-2*u[i])/pow(h,2) ;
}

return ux , uxx  ;  }
//______________________________________________________
//forcing
float compute_f(int t, int n , float h , float pi) {
float f[n];

for (i=0 ; i<=n ; i++) {
    f[i]= sin(2*pi*i/n)*sin(0.1*t);
}

return f ; }
//____________________MAIN______________________________
int main() {
const float pi = 2*acos(0.0);
const int tmax = 50, n=50 ;
const float dt = 0.01 ;// mind Courant condition!
const float L=1.0 , h= L/n , Re=40.0;
const int steps = round(tmax/dt) , size_u={n,steps};
float ui[n], u[n*steps], ut[n], ux[n], uxx[n];
int i,j,k ;

                                // no -1 bc no <=
u = copy(InitCond, 0, n, u, 0, size_u[0] ) ;

for (i=0 ; i<= steps ; i++) { //time cycle

//ui = u[:][i]; //instant u (current line of u array)
ui = copy(u,0,size_u[0], ui, 0, size_u[0]);

ux, uxx = compute_ux_uxx(ui) ;
f = compute_f(i,n,h,pi) ;

    for (j=0 ; j<=n ; j++) {
    ut[j] = uxx[j]/Re + f[j] - ui[j]*ux[j] ;
    u[j+(i+1)*n] = ui[j] + ut[j]*dt                 //i+1?
    return ut, u               }

    return 0 } //time cycle end

    return 0;}  //END MAIN

