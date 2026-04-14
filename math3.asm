         keep  obj/math3
         mcopy math3.macros
         case  on

****************************************************************
*
*  Math3 - additional math routines from C23
*
*  This code implements <math.h> functions added in C23.
*
****************************************************************

math3    private                        dummy segment
         copy  equates.asm
         end

INVALID    gequ $0001                   exceptions
UNDERFLOW  gequ $0002
OVERFLOW   gequ $0004
DIVBYZERO  gequ $0008
INEXACT    gequ $0010

TONEAREST  gequ 0                       rounding directions
UPWARD     gequ 1
DOWNWARD   gequ 2 
TOWARDZERO gequ 3

EXTENDED_PRECISION gequ 0               rounding precisions
DOUBLE_PRECISION   gequ 1
SINGLE_PRECISION   gequ 2

FADDX_OPWORD gequ $0000                 opwords for certain SANE operations
FSUBX_OPWORD gequ $0002
FMULX_OPWORD gequ $0004
FDIVX_OPWORD gequ $0006

****************************************************************
*
*  MathCommon3 - common work areas for the math library
*
****************************************************************
*
MathCommon3 privdata
;
;  temporary work space/return value
;
t1       ds    10
         end

****************************************************************
*
*  double compoundn(double x, long long int n);
*
*  Returns (1+x)^n (defined for x >= -1).
*
****************************************************************
*
compoundn start
compoundnf entry
compoundnl entry
         using MathCommon3

         csubroutine (10:x,8:n),0

         phb
         phk
         plb

         lda   n+6                      if n == LLONG_MIN
         bpl   use_fc2x
         asl   a
         ora   n
         ora   n+2
         ora   n+4
         bne   use_fc2x

         lda   llmin                      t1 = LLONG_MIN
         sta   t1
         lda   llmin+2
         sta   t1+2
         lda   llmin+4
         sta   t1+4
         lda   llmin+6
         sta   t1+6
         lda   llmin+8
         sta   t1+8
         bra   do_cmpnd                 else

use_fc2x tdc                              t1 = n (treated as comp)
         clc
         adc   #n
         pea   0
         pha
         ph4   #t1
         FC2X

do_cmpnd tdc                            t1 = (1 + x) ^ n
         clc
         adc   #x
         pea   0
         pha
         ph4   #t1
         ph4   #t1
         FCOMPOUND

         plb
         creturn 10:t1                  return t1

llmin    dc    e'-9223372036854775808'  LLONG_MIN in extended format
         end

****************************************************************
*
*  double exp10(double x);
*
*  Returns 10^x.
*
****************************************************************
*
exp10    start
exp10f   entry
exp10l   entry
         using MathCommon3

         csubroutine (10:x),0

         phb
         phk
         plb

         lda   ten                      t1 = 10
         sta   t1
         lda   ten+2
         sta   t1+2
         lda   ten+4
         sta   t1+4
         lda   ten+6
         sta   t1+6
         lda   ten+8
         sta   t1+8

         tdc                            t1 = 10 ^ x
         clc
         adc   #x
         pea   0
         pha
         ph4   #t1
         FXPWRY

         plb
         creturn 10:t1                  return t1

ten      dc    e'10'
         end

****************************************************************
*
*  double exp10m1(double x);
*
*  Returns 10^x - 1.
*
****************************************************************
*
exp10m1  start
exp10m1f entry
exp10m1l entry
         using MathCommon3

         csubroutine (10:x),0

         phb
         phk
         plb

         lda   x                        copy x to t1
         sta   t1
         lda   x+2
         sta   t1+2
         lda   x+4
         sta   t1+4
         lda   x+6
         sta   t1+6
         lda   x+8
         sta   t1+8

         ph4   #log2_10                 t1 = x * log2(10)
         ph4   #t1
         FMULX

         ph4   #t1                      t1 = 2^(x * log2(10)) - 1
         FEXP21X

         plb
         creturn 10:t1                  return t1

log2_10  dc    e'3.32192809488736234787031942948939017586483139302458'
         end

****************************************************************
*
*  double exp2m1(double x);
*
*  Returns 2^x - 1.
*
****************************************************************
*
exp2m1   start
exp2m1f  entry
exp2m1l  entry
         using MathCommon3

         csubroutine (10:x),0

         phb
         phk
         plb

         lda   x                        copy x to t1
         sta   t1
         lda   x+2
         sta   t1+2
         lda   x+4
         sta   t1+4
         lda   x+6
         sta   t1+6
         lda   x+8
         sta   t1+8

         ph4   #t1                      t1 = 2^x - 1
         FEXP21X

         plb
         creturn 10:t1                  return t1
         end

****************************************************************
*
*  float fadd(double x, double y);
*
*  Returns x+y, rounded to float precision.
*
****************************************************************
*
fadd     start
faddl    entry
         lda   #FADDX_OPWORD
         ldx   #SINGLE_PRECISION*$40
         jmp   ~NarrowingBinop
         end

****************************************************************
*
*  double daddl(long double x, long double y);
*
*  Returns x+y, rounded to double precision.
*
****************************************************************
*
daddl    start
         lda   #FADDX_OPWORD
         ldx   #DOUBLE_PRECISION*$40
         jmp   ~NarrowingBinop
         end

****************************************************************
*
*  float fsub(double x, double y);
*
*  Returns x-y, rounded to float precision.
*
****************************************************************
*
fsub     start
fsubl    entry
         lda   #FSUBX_OPWORD
         ldx   #SINGLE_PRECISION*$40
         jmp   ~NarrowingBinop
         end

****************************************************************
*
*  double dsubl(long double x, long double y);
*
*  Returns x-y, rounded to double precision.
*
****************************************************************
*
dsubl    start
         lda   #FSUBX_OPWORD
         ldx   #DOUBLE_PRECISION*$40
         jmp   ~NarrowingBinop
         end

****************************************************************
*
*  float fmul(double x, double y);
*
*  Returns x*y, rounded to float precision.
*
****************************************************************
*
fmul     start
fmull    entry
         lda   #FMULX_OPWORD
         ldx   #SINGLE_PRECISION*$40
         jmp   ~NarrowingBinop
         end

****************************************************************
*
*  double dmull(long double x, long double y);
*
*  Returns x*y, rounded to double precision.
*
****************************************************************
*
dmull    start
         lda   #FMULX_OPWORD
         ldx   #DOUBLE_PRECISION*$40
         jmp   ~NarrowingBinop
         end

****************************************************************
*
*  float fdiv(double x, double y);
*
*  Returns x/y, rounded to float precision.
*
****************************************************************
*
fdiv     start
fdivl    entry
         lda   #FDIVX_OPWORD
         ldx   #SINGLE_PRECISION*$40
         jmp   ~NarrowingBinop
         end

****************************************************************
*
*  double ddivl(long double x, long double y);
*
*  Returns x/y, rounded to double precision.
*
****************************************************************
*
ddivl    start
         lda   #FDIVX_OPWORD
         ldx   #DOUBLE_PRECISION*$40
         jmp   ~NarrowingBinop
         end

****************************************************************
*
*  ~NarrrowingBinop
*
*  Do a binary operation, rounding to a narrower result type.
*
*  Inputs:
*       Two extended operands on stack
*       A - Opword for the binary operation (+, -, *, /)
*       X - Rounding precision, to be or'd into environment word
*
****************************************************************
*
~NarrowingBinop private
         using MathCommon3

         sta   >opword
         txa
         sta   >precision

         csubroutine (10:x,10:y),0

         FGETENV
         phx                            save original environment
         txa
         and   #$E020                   clear halts, exceptions, precision
         ora   >precision               set rounding precision
         pha
         FSETENV

         lda   x                        copy x to t1
         sta   >t1
         lda   x+2
         sta   >t1+2
         lda   x+4
         sta   >t1+4
         lda   x+6
         sta   >t1+6
         lda   x+8
         sta   >t1+8

         tdc                            t1 = t1 OP y
         clc
         adc   #y
         pea   0
         pha
         ph4   #t1
         lda   >opword
         pha
         _SANEFP816

         FPROCEXIT                      restore env & raise any new exceptions

         creturn 10:t1                  return t1

opword   ds    2
precision ds   2
         end

****************************************************************
*
*  double fsqrt(double x);
*
*  Returns sqrt(x), rounded to single precision.
*
*  double dsqrtl(long double x);
*
*  Returns sqrt(x), rounded to double precision.
*
****************************************************************
*
fsqrt    start
fsqrtl   entry
         using MathCommon3

         lda   #SINGLE_PRECISION*$40
         bra   lb1
         
dsqrtl   entry
         lda   #DOUBLE_PRECISION*$40

lb1      sta   >precision

         csubroutine (10:x),0

         FGETENV
         phx                            save original environment
         txa
         and   #$E020                   clear halts, exceptions, precision
         ora   >precision               set rounding precision
         pha
         FSETENV

         lda   x                        copy x to t1
         sta   >t1
         lda   x+2
         sta   >t1+2
         lda   x+4
         sta   >t1+4
         lda   x+6
         sta   >t1+6
         lda   x+8
         sta   >t1+8

         ph4   #t1                      t1 = sqrt(t1)
         FSQRTX

         FPROCEXIT                      restore env & raise any new exceptions

         creturn 10:t1                  return t1

opword   ds    2
precision ds   2
         end

****************************************************************
*
*  long llogb(double x);
*
*  Returns the binary exponent of x (a signed integer value),
*  treating denormalized numbers as if they were normalized.
*  Handles inf/nan/0 cases specially.
*
****************************************************************
*
llogb    start
llogbf   entry
llogbl   entry

         csubroutine (10:x),0

         tdc                            check for special cases
         clc
         adc   #x
         pea   0
         pha
         FCLASSX
         txa
         ldy   #$7FFF
         ldx   #$FFFF
         and   #$FF
         cmp   #$FE                     if x is INF
         beq   special                    return LONG_MAX
         lsr   a
         beq   do_logb                  if x is 0 or NAN
         inx                              return LONG_MIN
         iny
special  stx   x
         sty   x+2
         bra   ret

do_logb  tdc                            compute logb(x)
         clc
         adc   #x
         pea   0
         pha
         FLOGBX
         
         tdc                            convert to long integer
         clc
         adc   #x
         pea   0
         pha
         pea   0
         pha
         FX2L
         
ret      creturn 4:x                    return it
         end

****************************************************************
*
*  double log10p1(double x);
*
*  Returns log10(1+x).
*
****************************************************************
*
log10p1  start
log10p1f entry
log10p1l entry
         using MathCommon3

         phb                            place the number in a work area
         plx
         ply
         phk
         plb
         pla
         sta   t1
         pla
         sta   t1+2
         pla
         sta   t1+4
         pla
         sta   t1+6
         pla
         sta   t1+8
         phy
         phx
         plb

         ph4   #t1                      t1 = log2(1+x)
         FLOG21X

         ph4   #log2_10                 t1 = log2(1+x) / log2(10)
         ph4   #t1
         FDIVX
         
         ldx   #^t1                     return a pointer to the result
         lda   #t1
         rtl

log2_10  dc    e'3.32192809488736234787031942948939017586483139302458'
         end

****************************************************************
*
*  double log2p1(double x);
*
*  Returns log2(1+x).
*
****************************************************************
*
log2p1   start
log2p1f  entry
log2p1l  entry
         using MathCommon3

         phb                            place the number in a work area
         plx
         ply
         phk
         plb
         pla
         sta   t1
         pla
         sta   t1+2
         pla
         sta   t1+4
         pla
         sta   t1+6
         pla
         sta   t1+8
         phy
         phx
         plb

         ph4   #t1                      compute the value
         FLOG21X
         
         ldx   #^t1                     return a pointer to the result
         lda   #t1
         rtl
         end

****************************************************************
*
*  double logp1(double x);
*
*  Returns ln(1+x).
*
****************************************************************
*
logp1    start
logp1f   entry
logp1l   entry

         jml   log1p
         end

****************************************************************
*
*  double pown(double x, long long int n);
*
*  Returns x^n.
*
****************************************************************
*
pown     start
pownf    entry
pownl    entry
         using MathCommon3

         csubroutine (10:x,8:n),0

         phb
         phk
         plb

         lda   n+6                      if n == LLONG_MIN
         bpl   use_fc2x
         asl   a
         ora   n
         ora   n+2
         ora   n+4
         bne   use_fc2x

         lda   llmin                      t1 = LLONG_MIN
         sta   t1
         lda   llmin+2
         sta   t1+2
         lda   llmin+4
         sta   t1+4
         lda   llmin+6
         sta   t1+6
         lda   llmin+8
         sta   t1+8
         bra   do_pwr                   else

use_fc2x tdc                              t1 = n (treated as comp)
         clc
         adc   #n
         pea   0
         pha
         ph4   #t1
         FC2X

do_pwr   ph4   #t1                      x = x ^ t1
         tdc
         clc
         adc   #x
         pea   0
         pha
         FXPWRY

         lda   x                        copy x to t1
         sta   t1
         lda   x+2
         sta   t1+2
         lda   x+4
         sta   t1+4
         lda   x+6
         sta   t1+6
         lda   x+8
         sta   t1+8

         plb
         creturn 10:t1                  return t1

llmin    dc    e'-9223372036854775808'  LLONG_MIN in extended format
         end
