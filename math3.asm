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
