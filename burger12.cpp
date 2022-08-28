#include <iostream>
#include <string>
using namespace std;

//_________________________________________________
int slice2to1(int n, int m , float array[n][m], int dir, int index ) {
int i;

 switch(dir) {
    case 1:
float slice[m[0]]
for (i=0 ; i<=m[0] ; i++){
    slice[i]=array[i][index]
}
    case 2:
float slice[m[1]]
for (i=0 ; i<=m[1] ; i++){
    slice[i]=array[index][i]
}
            }
return slice  }
//_________________________________________________
int slice1to2(float slice[] ,int m[2], float array[m[0]][m[1]], int dir, int index ) {
int i;                        

 switch(dir) {
    case 1:
float slice [m[0]]
for (i=0 ; i<=m[0] ; i++){
    array[i][index]=slice[i]
}
    case 2:
float slice[m[1]]
for (i=0 ; i<=m[1] ; i++){
    array[index][i]=slice[i]
}
            }
return array  }
//________________________________________________
int InitCond(int n , float pi ) {
    int i ;
    float u0[n];

    for (i=0 ; i<=n ; i++) {   //n-1??
        //u0[i]= 0.001*pow(i,2) - 0.2*i + 4
        u0[i] = sin(2*pi*i/n);
                            }
return u0       }
//______________________________________________________
//with PBC
int compute_ux_uxx( int n , float u[n], float h) {
float ux[n];
//------------------------1st order
for (i=0 ; i<=n ; i++ ) {
    ux[i] = (u[round(fmod(i+1,n))]-u[round(fmod(i-1,n)])/(2*h);
}
//-------------------------2nd order
for (i=0 ; i<=n ; i++ ) {
    uxx[i] = (u[round(fmod(i+1,n))]+u[round(fmod(i-1,n)]-2*u[i])/pow(h,2) ;
}

return ux , uxx    }
//______________________________________________________
//forcing
int compute_f(int t, int n , float h , float pi) {
float f[n];

for (i=0 ; i<=n ; i++) {
    f[i]= sin(2*pi*i/n)*sin(0.1*t);
}

return f  }
//____________________MAIN______________________________
int main() {
const float pi = 2*acos(0.0);
const int tmax = 50, n=50 ;
const float dt = 0.01 ;// mind Courant condition!
const float L=1.0 , h= L/n , Re=40.0;
const int steps = round(tmax/dt) , size_u={n,steps};
const int* ptr =&size_u;
float ui[n], u[n][steps], ut[n], ux[n], uxx[n];
int i,j,k ;

//u[:][0]=InitCond(n); 
//u[dir=1st][index=0]
u = slice1to2(InitCond, size_u, u, 1, 0) ;

for (i=0 ; i<= steps ; i++) { //time cycle

//ui = u[:][i]; //instant u (current line of u array)
ui = slice2to1(n,steps ,u, 1, i);

ux, uxx = compute_ux_uxx(ui) ;
f = compute_f(i,n,h,pi) ;

    for (j=0 ; j<=n ; j++) {
    ut[j] = uxx[j]/Re + f[j] - ui[j]*ux[j] ;
    u[j][i+1] = ui[j] + ut[j]*dt
    return ut, u               }

    return 0 } //time cycle end

    return 0;}  //END MAIN

