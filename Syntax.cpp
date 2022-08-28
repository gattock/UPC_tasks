//DECLARATIONS, STRINGS AND STUFF


#include <iostream> // Input-Output header
using namespace std; // standard library for objects and variables names

// Include the string library (subset of ...?)
#include <string>

# include <cmath>

/* SOME PRINTING and STRINGS
FUNCTIONS: */
int main() {
    cout << "Hello World"; //see-out
    cout << "Ciao Mondo"; // << is insertion operator
    return 0;     // every statement finishes with ;
}   
// ^will print in the same line. to go new line use \n   
int main() {
  cout << "Hello World! \n"; // \n\n skip 1 line
  cout << "I am learning C++";
  return 0;
}
// endl manipulator (endline)
int main() {
  cout << "Hello World!" << endl;
  cout << "I am learning C++";
  return 0;
}

//STRING CONCATENATION:
string first = "Ciao";
string second= "Erri";
string x= first + second;
cout << x //OUT --> CiaoErri
//append function
string x= first.append(second);

//concatenating numbers
string x = "10";
string y = "20";
string z = x + y; //z is 1020 (a string)

//string length:
string text= "abcdef"
cout << text.length();
cout << text.size(); //the same

//access strings:
string parola= "Hello";
cout << parola[0]; //OUT --> H
//modify
myString[0] = 'J';
cout << parola; // OUT--> Jello

//string user input:
string firstName;
cout << "Type your first name: ";
cin >> firstName;
//SPACES ARE TERMINATING CHARACTERS

/* ESCAPE SEQUENCES: 
\t	Creates a horizontal tab	
\\	Inserts a backslash character (\)	
\"	Inserts a double quote character
*/

//___________________________________________
// namespace omission by std::
#include <iostream>

int main() {
  std::cout << "Hello World!";
  return 0;
}
////___________________________________________
/*VARIABLES
int= 
double= floating point
char = character as 'a' or 'B'
string = "Hello World"
bool = boolean variable

e.g. */
int alpha = 15;
cout << alpha;
//or
int alpha; //2 or 4 bytes
alpha=16;
cout << alpha;
//others
float a_number = 3.1415; //4 bytes->  6 digits
double myFloatNum = 5.99;//8 bytes-> 15 digits 
float f1 = 35e3; //scientific notation
double d1 = 12E4;
char myLetter = 'D';            
bool myBoolean = true; // true = 1 and false = 0
//print some sentence:
int myAge = 35;
cout << "I am " << myAge << " years old.";
//multiple declaration A:
int x = 5, y = 6, z = 50;
cout << x + y + z;
//multiple declaration B:
int x, y, z;
x = y = z = 50;
cout << x + y + z;

/*SOME RULES:
Names can contain letters, digits and underscores
Names must begin with a letter or an underscore (_)
Names are case sensitive
No reserved words or spaces or special characters allowed
*/

//Declare read-only variable (constants)
const int myNum = 15;

//_________________________________________
//USER INPUT
int x;
cout << "Type a number_ "; 
cin >> x;
cout << "Your number is: " << x;
//_________________________________________
//USING ASCII CHARACTERS:
using namespace std;
int main () {
  char a = 125, b = 100, c = 76;
  cout << a;
  cout << b;
  cout << c;
  return 0;
}
// OUTPUT }dL

//__________________________________________
//STRUCTURES (struct)
/*are a way to group several related variables into one place. Each variable in the structure is known as a member of the structure.
Unlike an array, a structure can contain many different data types (int, string, bool, etc.). */

//e.g.
struct {             // Structure declaration
  int myNum;         // Member (int variable)
  string myString;   // Member (string variable)
} myStructure, secondaStruttura ;       // Structure variable
// Assign values to members of myStructure
myStructure.myNum = 1;
myStructure.myString = "Hello World!";
secondaStruttura.myNum=3;
// Print members of myStructure
cout << myStructure.myNum << "\n";
cout << myStructure.myString << "\n";
//----------------
//another e.g.
struct {
  string brand;
  string model;
  int year;
} myCar1, myCar2; // We can add variables by separating them with a comma here

// Put data into the first structure
myCar1.brand = "BMW";
myCar1.model = "X5";
myCar1.year = 1999;

// Put data into the second structure
myCar2.brand = "Ford";
myCar2.model = "Mustang";
myCar2.year = 1969;
//----------------------

//NAMED STRUCTURES: you can treat it as a data type. This means that you can create variables with this structure anywhere in the program at any time.

// ESEMPIO VELICO
struct boat {
    string shipyard;
    string model;
    int year;
};

int main() {
    boat Talita;
    Talita.shipyard= "Comar";
    Talita.model= "Comet1050";
    Talita.year= 1991;

    boat Miniraf;
    Miniraf.shipyard= "Del Pardo";
    Miniraf.model= "Grand Soleil";
    Miniraf.year= 1984;

    cout << Talita.shipyard << " " Talita.model << " " << Talita.year << "\n";

    return 0;
}
//___________________________________________
//REFERENCES (use??) is a "reference" to an existing variable, and it is created with the & operator:
string food = "Pizza";
string &meal = food;

cout << food << "\n";  // Outputs Pizza
cout << meal << "\n";  // Outputs Pizza

// Memory Address
string food= "Pizza";
cout << &food; //OUT --> 0x6fed4 --> location of where the variable is stored on the computer.

/* N.B. References and Pointers (which you will learn about in the next chapter) 
are important in C++, because they give you the ability to manipulate the data in the computer's memory
which can reduce the code and improve the performance. */

/* POINTERS: 
-are variables that store the memory address as their value.
-A pointer variable points to a data type (like int or string) of the same type!
-It is created with the * operator            */

//e.g.
string food = "Pizza";  // A food variable of type string
string* ptr = &food;    // A pointer variable, with the name ptr, that stores the address of food
cout << food << "\n";   // Output the value of food (Pizza)
cout << &food << "\n"; // Output the memory address of food (0x6dfed4)
cout << ptr << "\n";   // Output the memory address of food with the pointer (0x6dfed4)
// Change the value of the pointer:
*ptr = "Hamburger"; 

// ALL VALID POINTERS
string* mystring; // Preferred
string *mystring;
string * mystring;

//IMPORTANT!!
// you can also use the pointer to get the value of the variable, by using the * operator (the dereference operator):
string food = "Pizza";  // Variable declaration
string* ptr = &food;    // Pointer declaration
cout << ptr << "\n"; // Reference: Output the memory address of food with the pointer (0x6dfed4)
cout << *ptr << "\n"; // Dereference: Output the value of food with the pointer (Pizza)



































//MAAAAAAAAAAATH



//ARRAYSSS
string cars[4];
cars[0]= "Volvo"
/*or*/ string cars[4] = {"Volvo", "BMW", "Ford", "Mazda"};
cout << cars[0];

int myNum[3] = {10, 20, 30};

//print array e.g.
string cars[4] = {"Volvo", "BMW", "Ford", "Mazda"};
for (int i = 0; i < 4; i++) {
  cout << cars[i] << "\n";
}

//OMIT SIZE:
string cars[] = {"Vol", "BMW", "Ford"}; // size of array is always 3
//in this case, to add, must:
string cars[] = {"Vol", "BMW", "Ford", "Mazda", "Tesla"};

//or you can reserve space:
string cars[5] = {"Vol", "BMW", "Ford"};
cars[3] = "Mazda";
cars[4] = "Tesla";

//ARRAY SIZE: REMEMBER!!!!!!!!!
int mynumbers[3] = {1,2,3};
cout << sizeof(mynumbers) // [bytes] !!!!
int getArrayLength = sizeof(myNumbers) / sizeof(int);
cout << getArrayLength; 

//sizes of 2D-Tensors
const int n = sizeof(array) / sizeof(array[0]); // rows  
const int m = sizeof(array[0]) / sizeof(int); // cols


//TENSORSS (multi dim arrays)
string letters[2][4];
//---
string letters[2][4] = {
  { "A", "B", "C", "D" },
  { "E", "F", "G", "H" } 
};

for(int i = 0; i < 2; i++) {
  for(int j = 0; j < 4; j++) {
    cout << letters[i][j] << "\n";
  }
}


//_________________________________________
//SOME OPERATORS: standard + - * /
int main() {
  int x = 5;
  int y = 2;
  cout << x % y; //mod
  ++x; //increment of one
  --x; //decrement of one
  x +=5; //addition assignment
  cout << x;
  return 0;
}
/*others: 
Example	    Same As	
x = 5	    x = 5	
x += 3	    x = x + 3	
x -= 3	    x = x - 3	
x *= 3	    x = x * 3	
x /= 3	    x = x / 3	
x %= 3	    x = x % 3	
*/		
//COMPARISON OPERATORS
int x = 5;
int y = 3;
cout << (x > y);
/*
Equal to	x == y	
Not equal	x != y	
Greater than	x > y	
Less than	x < y	
Greater than or equal to	x >= y	
Less than or equal to	x <= y
*/
//LOGICAL OPERATORS:
/*
AND &&
OR  ||
NOT  !

e.g.*/
cout << (x > 3 && x < 10);
cout << (x > 3 || x < 4);
cout << (!(x > 3 && x < 10));

//BASIC MATH
cout << max(5,10); 
/* also min, sqrt,round,log

Function	Description
abs(x)	Returns the absolute value of x
acos(x)	Returns the arccosine of x
asin(x)	Returns the arcsine of x
atan(x)	Returns the arctangent of x
cbrt(x)	Returns the cube root of x
ceil(x)	Returns the value of x rounded up to its nearest integer
cos(x)	Returns the cosine of x
cosh(x)	Returns the hyperbolic cosine of x
exp(x)	Returns the value of Ex
expm1(x)	Returns ex -1
fabs(x)	Returns the absolute value of a floating x
fdim(x, y)	Returns the positive difference between x and y
floor(x)	Returns the value of x rounded down to its nearest integer
hypot(x, y)	Returns sqrt(x2 +y2) without intermediate overflow or underflow
fma(x, y, z)	Returns x*y+z without losing precision
fmax(x, y)	Returns the highest value of a floating x and y
fmin(x, y)	Returns the lowest value of a floating x and y
fmod(x, y)	Returns the floating point remainder of x/y
pow(x, y)	Returns the value of x to the power of y
sin(x)	Returns the sine of x (x is in radians)
sinh(x)	Returns the hyperbolic sine of a double value
tan(x)	Returns the tangent of an angle
tanh(x)	Returns the hyperbolic tangent of a double value
*/





















//CYCLEEEEES AND CONDITIONSSSS


// IF STATEMENT:
int x = 20 , y = 18;
if (x > y) {
  cout << "x is greater than y";
}
//ELSE
int time = 20;
if (time < 18) {
  cout << "Good day.";
} else {
  cout << "Good evening.";
}
//ELSE IF
int time = 22;
if (time < 10) {
  cout << "Good morning.";
} else if (time < 20) {
  cout << "Good day.";
} else {
  cout << "Good evening.";
}
//SHORT ELSE :
int time = 20;
string resultado = (time < 18) ? "Good day." : "Good evening.";
cout << resultado;

//-------------------
//SWITCH STATEMENT: select which of many code blocks to be executed!

//Syntax:
switch(expression) {
  case x:
    // code block
    break;
  case y:
    // code block
    break;
  default:
    // code block
}

// e.g.
int day = 2;
switch (day) {
  case 1:
    cout << "Monday";
    break;
  case 2:
    cout << "Tuesday";
    break;
//all other days
  case 7:
    cout << "Sunday";
    break;
}

//default keyword specifies some code to run if there is no case match:
//e.g.
int day = 4;
switch (day) {
  case 7:
    cout << "Today is Sunday";
    break;
  default:
    cout << "Looking forward to the Weekend";
}

//____________________________________________
//WHILE LOOP
while (condition) {
  // code block to be executed
}
//e.g.
int i = 0;
while (i < 5) {
  cout << i << "\n";
  i++;
}

//FOR LOOP
for (statement 1; statement 2; statement 3) {
  // code block to be executed
}

//e.g.
for (int i = 0; i < 5; i++) {
  cout << i << "\n";
}

//BREAK STATEMENT: jump out of a loop
for (int i = 0; i < 10; i++) {
  if (i == 4) {
    break;
  }
  cout << i << "\n";
}
//CONTINUE STATEMENT: breaks one iteration of the loop
int i = 0;
while (i < 10) {
  if (i == 4) {
    i++;
    continue;
  }
  cout << i << "\n";
  i++;
}

































// C++ FUNCTIONSSSSSSSS
//C++ provides some pre-defined functions, such as main()



//e.g.
void function1() {
    //code to be executed
}

/* void means that the function 
does not have a return value!!!!!!      */


//call a function:
void myFunction() {
  cout << "I just got executed!\n";
}
// MUST BE DECLARED BEFORE MAIN!!!!
int main() {
  myFunction();
  myFunction();
  return 0;
}

// ANYWAY CAN BE DECLARED BEFORE BUT DEFINED AFTER:
//important for code optimiz. and readability
void myFunction();   // Function declaration

int main() { 
  myFunction();  
  return 0;
}

void myFunction() {     // Function definition
  cout << "I just got executed!";
}

//FUNCTION WITH PARAMETERS + default
void myFunction(string firstname="Anja", int age=5) {
  cout << firstname << " Smith." << age << "years old. \n";
}
int main() {
  myFunction("Liam", 3);
  myFunction("Jenny", 14);
  myFunction(); //DEFAULT PARAMETERS
  return 0;
}



























