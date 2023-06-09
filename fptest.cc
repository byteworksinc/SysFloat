#pragma keep "fptest"
#pragma lint -1

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

/* This program checks to insure that all subroutines needed by	*/
/* ORCA/C that are not also used by ORCA/Pascal are in the      */
/* library, can be linked, and can be called.  It does _not_    */
/* check the quality of the output.	                        */

float a, b, c;
double d1, d2, d3;
extended e1, e2, e3;
comp c1, c2, c3;
int i;
char s[100];

float ff(void) {return 1.0;}

double df(void) {return 1.0;}

extended ef(void) {return 1.0;}

comp cf(void) {return 1;}


#pragma optimize -1

void f(void)

{
a = ff();
d1 = df();
e1 = ef();
c1 = cf();
}

#pragma optimize 0


void main (void)

{
long l; unsigned u; unsigned long ul;

/* various assignment forms */
a = ff();
b = c = a;
d1 = df();
d2 = d3 = d1;
e1 = ef();
e2 = e3 = e1;
c1 = cf();
c2 = c3 = c1;
f();

/* type casts */

a = 1;
a = 1L;
a = 1u;
a = 1UL;
i = a;
l = a;
u = a;
ul = a;

/* math.h calls */
b = 1.0;
c = 2.0;

a = acos(b);
a = asin(b);
a = atan(b);
a = cos(b);
a = cosh(b);
a = exp(b);
a = log(b);
a = log10(b);
a = sin(b);
a = sinh(b);
a = sqrt(b);
a = tan(b);
a = tanh(b);
a = atan2(b, c);
a = ceil(b);
a = fabs(b);
a = floor(b);
a = fmod(b, c);
a = pow(b, c);
a = frexp(b, &i);
a = ldexp(b, i);
a = modf(b, &i);

/* stdio.h floating point converters */
sscanf("1.2", "%f", &a);
sprintf(s, "%f", a);
sprintf(s, "%g", a);

/* stdlib.h floating-point routines */
a = strtod("1.2", NULL);

/* time.h floating-point routines */
a = difftime(20000L, 10000L);
}
