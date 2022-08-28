#include <iostream>
#include <string>
#include <vector>
# include <cmath>
//using namespace std;

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


int main() {
const float pi = 2*acos(0.0);
const int tmax = 50, n=50 ;
const float dt = 0.01 ;// mind Courant condition!
const float L=1.0 , h= L/n , Re=40.0;
const int steps = round(tmax/dt) , size_u={n,steps};
int i,j,k ;

std::vector<std::float> ui(n); 
std::vector<std::vector<float>> u(n,std::vector<float>(steps));
std::vector<std::float> ut(n);
std::vector<std::float> ux(n);
std::vector<std::float> uxx(n);

ui = InitCond( n , pi) ;

for (auto i = ui.begin(); i != ui.end(); ++i)
    cout << *i << " ";

}