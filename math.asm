         keep  obj/math
         mcopy math.macros
         case  on

****************************************************************
*
*  Math - Math libraries for C
*
*  This code implements the tables and subroutines needed to
*  support the standard C library MATH.
*
*  January 1989
*  Mike Westerfield
*
*  Copyright 1989
*  Byte Works, Inc.
*
****************************************************************
*
Math     start                          dummy segment
         copy  equates.asm
         end

****************************************************************
*
*  MathCommon - common work areas for the math library
*
****************************************************************
*
MathCommon privdata
;
;  constants
;
pi       dc    h'35C2 6821 A2DA 0FC9 0040'
piover2  dc    h'35C2 6821 A2DA 0FC9 FF3F'
quarter  dc    h'0000 0000 0000 0080 FD3F'
half     dc    h'0000 0000 0000 0080 FE3F'
one      dc    h'0000 0000 0000 0080 FF3F'
two      dc    h'0000 0000 0000 0080 0040'
;
;  temporary work space
;
t1       ds    10
t2       ds    10
t3       ds    10
sign     ds    2
orig     ds    10                       saved copy of input value
         end

****************************************************************
*
*  asin - return arcsin(x)
*
*  Inputs:
*        stack - extended number
*
*  Outputs:
*        X-A - addr of the result
*
****************************************************************
*
asin     start
         using MathCommon
;
;  Intialization
;
         phb                            place the number in a work area
         plx
         ply
         phk
         plb
         pla
         sta   t1
         sta   orig
         pla
         sta   t1+2
         sta   orig+2
         pla
         sta   t1+4
         sta   orig+4
         pla
         sta   t1+6
         sta   orig+6
         pla
         sta   t1+8
         sta   orig+8
         phy
         phx
;
;  do general initialization
;
         move  one,t3,#10               t3 := 1.0
         lda   t1                       t2 := t1
         sta   t2
         lda   t1+2
         sta   t2+2
         lda   t1+4
         sta   t2+4
         lda   t1+6
         sta   t2+6
         lda   t1+8
         sta   t2+8
;
;  if t2 >= 2^-33 then arcsine(t1) := atan(t1/sqrt(1-sqr(t2))
;  else arssine(t1) := t1
;
         and   #$7FFF                   branch if the value is less than 2^-33
         cmp   #$3FDE
         blt   lb3
         lda   #t2                      t2 := sqr(t2)
         ldx   #^t2
         phx
         pha
         phx
         pha
         fmulx
         ph4   #t2                      t3 := t3-t2
         ph4   #t3
         fsubx
lb2      lda   #t3                      t3 := sqrt(t3)
         ldx   #^t3
         phx
         pha
         phx
         pha
         fsqrtx
         ph4   #t1                      t1 := t1/t3
         fdivx
         ph4   #t1                      t1 := atan(t1)
         fatanx

lb3      jsr   ~SetErrno                check for a domain error
         ldx   #^t1                     return a pointer to the result
         lda   #t1
         plb
         rtl
         end

****************************************************************
*
*  acos - return arccosin(x)
*
*  Inputs:
*        stack - extended number
*
*  Outputs:
*        X-A - addr of the result
*
****************************************************************
*
acos     start
         using MathCommon
;
;  Intialization
;
         phb                            place the number in a work area
         plx
         ply
         phk
         plb
         pla
         sta   t1
         sta   orig
         pla
         sta   t1+2
         sta   orig+2
         pla
         sta   t1+4
         sta   orig+4
         pla
         sta   t1+6
         sta   orig+6
         pla
         sta   t1+8
         sta   orig+8
         phy
         phx
;
;  acos(t1) := 2*atan(sqrt((1-t1)/(1+t1)))
;
         move  one,t2,#10               t2 := 1
         ph4   #t1                      t2 := t2+t1
         ph4   #t2
         faddx
         ph4   #one                     t1 := t1-1
         ph4   #t1
         fsubx
         lda   t1+8                     t1 := -t1
         eor   #$8000
         sta   t1+8
         ph4   #t2                      t1 := t1/t2
         ph4   #t1
         fdivx
         lda   #t1                      t1 := sqrt(t1)
         ldx   #^t1
         phx
         pha
         phx
         pha
         fsqrtx
         fatanx                         t1 := atan(t1)
         ph4   #two                     t1 := t1*2
         ph4   #t1
         fmulx

         jsr   ~SetErrno                check for a domain error
         ldx   #^t1                     return a pointer to the result
         lda   #t1
         plb
         rtl
         end

****************************************************************
*
*  atan - return arctan(x)
*
*  Inputs:
*        stack - extended number
*
*  Outputs:
*        X-A - addr of the result
*
****************************************************************
*
atan     start
         using MathCommon

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
         ph4   #t1                      compute the arc tangent
         fatanx
         ldx   #^t1                     return a pointer to the result
         lda   #t1
         plb
         rtl
         end

****************************************************************
*
*  atan2 - return arctangent(y,x) scaled to -pi..pi
*
*  Inputs:
*        stack - extended numbers
*
*  Outputs:
*        X-A - addr of the result
*
****************************************************************
*
atan2    start
         using MathCommon

         phb                            place the numbers in the work areas
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
         pla
         sta   t2
         pla
         sta   t2+2
         pla
         sta   t2+4
         pla
         sta   t2+6
         pla
         sta   t2+8
         phy
         phx
;
;  do special processing for x=0
;
         lda   t2+8                     branch if x <> 0
         and   #$7FFF
         ora   t2+6
         ora   t2+4
         ora   t2+2
         ora   t2
         bne   lb2
         lda   t1+8                     if y = 0, report a range error
         and   #$7FFF
         ora   t1+6
         ora   t1+4
         ora   t1+2
         ora   t1
         bne   lb1
         lda   #ERANGE
         sta   >errno
         brl   lb5

lb1      lda   t1+8                     return pi/2 with the sign of t1
         and   #$8000
         ora   piover2+8
         sta   t1+8
         move  piover2,t1,#8
         bra   lb5
;
;  handle cases where x <> 0
;
lb2      lda   t2+8                     save the sign of t2 in sign
         and   #$8000
         sta   sign
         lda   t2+8                     t2 := abs(t2)
         and   #$7FFF
         sta   t2+8
         ph4   #t2                      t1 := t1/t2
         ph4   #t1
         fdivx
         ph4   #t1                      t1 := arctan(t1)
         fatanx
         lda   sign                     if t2 was less than zero then
         beq   lb5
         ph4   #pi
         ph4   #t1
         lda   t1+8                       if t1 < 0 then
         bpl   lb3
         faddx                              t1 := t1+pi
         bra   lb4
lb3      fsubx                            else t1 := t1-pi
lb4      lda   t1+8                       t1 := -t1
         eor   #$8000
         sta   t1+8

lb5      ldx   #^t1                     return a pointer to the result
         lda   #t1
         plb
         rtl
         end

****************************************************************
*
*  ceil - round up
*
*  Inputs:
*        stack - extended number
*
*  Outputs:
*        X-A - addr of the result
*
****************************************************************
*
ceil     start
         using MathCommon

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
         fgetenv                        set the rounding direction to round up
         phx
         txa
         and   #$3FFF
         ora   #$4000
         pha
         fsetenv
         ph4   #t1
         frintx                         round the number
         fsetenv                        restore the environment
         ldx   #^t1                     return a pointer to the result
         lda   #t1
         plb
         rtl
         end

****************************************************************
*
*  cos - return cos(x)
*
*  Inputs:
*        stack - extended number
*
*  Outputs:
*        X-A - addr of the result
*
****************************************************************
*
cos      start
         using MathCommon

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
         ph4   #t1                      compute the cosine
         fcosx
         ldx   #^t1                     return a pointer to the result
         lda   #t1
         plb
         rtl
         end

****************************************************************
*
*  cosh - return hyperbolic cosine(x)
*
*  Inputs:
*        stack - extended number
*
*  Outputs:
*        X-A - addr of the result
*
****************************************************************
*
cosh     start
         using MathCommon
;
;  Intialization
;
         phb                            place the number in a work area
         plx
         ply
         phk
         plb
         pla
         sta   t1
         sta   orig
         pla
         sta   t1+2
         sta   orig+2
         pla
         sta   t1+4
         sta   orig+4
         pla
         sta   t1+6
         sta   orig+6
         pla
         sta   orig+8
         and   #$7FFF                   (take the absolute value now)
         sta   t1+8
         phy
         phx
;
;  cosh(t1) := (0.5*exp(abs(t1)) + 0.25/(0.5*exp(abs(t1)))
;
         ph4   #t1                      t1 := exp(abs(t1))
         fexpx
         ph4   #half                    t1 := t1*0.5
         ph4   #t1
         fmulx
         move  quarter,t2,#10           t2 := 0.25
         ph4   #t1                      t2 := t2/t1
         ph4   #t2
         fdivx
         ph4   #t2                      t1 := t1+t2
         ph4   #t1
         faddx

         jsr   ~SetErrno                check for a range error 
         ldx   #^t1                     return a pointer to the result
         lda   #t1
         plb
         rtl
         end

****************************************************************
*
*  exp - return exp(x)
*
*  Inputs:
*        stack - extended number
*
*  Outputs:
*        X-A - addr of the result
*
****************************************************************
*
exp      start
         using MathCommon

         phb                            place the number in a work area
         plx
         ply
         phk
         plb
         pla
         sta   t1
         sta   orig
         pla
         sta   t1+2
         sta   orig+2
         pla
         sta   t1+4
         sta   orig+4
         pla
         sta   t1+6
         sta   orig+6
         pla
         sta   t1+8
         sta   orig+8
         phy
         phx
         ph4   #t1                      compute the exponent
         fexpx
         jsr   ~SetErrno                check for a range error 
         ldx   #^t1                     return a pointer to the result
         lda   #t1
         plb
         rtl
         end

****************************************************************
*
*  fabs - absolute value of an extended number
*
*  Inputs:
*        stack - extended number
*
*  Outputs:
*        X-A - addr of the result
*
****************************************************************
*
fabs     start
         using MathCommon

         phb
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
         and   #$7FFF
         sta   t1+8
         phy
         phx
         plb
         ldx   #^t1
         lda   #t1
         rtl
         end

****************************************************************
*
*  floor - round down
*
*  Inputs:
*        stack - extended number
*
*  Outputs:
*        X-A - addr of the result
*
****************************************************************
*
floor    start
         using MathCommon

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
         fgetenv                        set the rounding direction to round down
         phx
         txa
         and   #$3FFF
         ora   #$8000
         pha
         fsetenv
         ph4   #t1
         frintx                         round the number
         fsetenv                        restore the environment
         ldx   #^t1                     return a pointer to the result
         lda   #t1
         rtl
         end

****************************************************************
*
*  fmod - return the floating point remainder
*
*  Inputs:
*        stack - extended number
*
*  Outputs:
*        X-A - addr of the result
*
****************************************************************
*
fmod     start
         using MathCommon

         phb                            place the numbers in the work areas
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
         pla
         sta   t2
         pla
         sta   t2+2
         pla
         sta   t2+4
         pla
         sta   t2+6
         pla
         sta   t2+8
         phy
         phx

         move  t1,t3,#10                t3 := t1/t2
         ph4   #t2
         ph4   #t3
         fdivx
         ph4   #t3                      t3 := trunc(t3)
         ftintx
         ph4   #t2                      t3 = t3*t2
         ph4   #t3
         fmulx
         ph4   #t3                      t1 = t1-t3
         ph4   #t1
         fsubx
lb1      ldx   #^t1                     return a pointer to the result
         lda   #t1
         plb
         rtl
         end

****************************************************************
*
*  frexp - split a number into a fracrion and exponent
*
*  Inputs:
*        x - number
*        nptr - pointer to location to save integer
*
*  Outputs:
*        returns the address of the fraction
*
****************************************************************
*
frexp    start
         using MathCommon

         csubroutine (10:x,4:nptr),0

         phb
         phk
         plb

         lda   x+8                      get the exponent
         and   #$7FFF
         bne   lb1                      handle zero
         sta   [nptr]
         bra   lb2
lb1      sec
         sbc   #$3FFE
         sta   [nptr]
         lda   x+8                      set the fraction range
         and   #$8000
         ora   #$3FFE
         sta   x+8
lb2      ldx   #8                       set up to return the result
lb3      lda   x,X
         sta   t1,X
         dex
         dex
         bpl   lb3
         lla   nptr,t1

         plb
         creturn 4:nptr
         end

****************************************************************
*
*  ldexp - raise a number to an integer power of 2
*
*  Inputs:
*        x - number
*        n - integer power of 2
*
*  Outputs:
*        returns the address of the result
*
****************************************************************
*
ldexp    start
         using MathCommon

         csubroutine (10:x,2:n),0

         phb
         phk
         plb

         lda   x+8                      separate the sign from the exponent
         and   #$8000
         sta   sign
         clc                            add the value to the exponent
         lda   x+8
         and   #$7FFF
         adc   n
         bvs   err
         cmp   #$7FFF                   check for a range error
         blt   lb1
err      lda   #ERANGE
         sta   >errno
         bra   lb2
lb1      ora   sign                     replace the sign
         sta   x+8                      save the exponent

lb2      ldx   #8                       set up to return the result
lb3      lda   x,X
         sta   t1,X
         dex
         dex
         bpl   lb3
         lla   x,t1

         plb
         creturn 4:x
         end

****************************************************************
*
*  log - return ln(x)
*
*  Inputs:
*        stack - extended number
*
*  Outputs:
*        X-A - addr of the result
*
****************************************************************
*
log      start
         using MathCommon

         phb                            place the number in a work area
         plx
         ply
         phk
         plb
         pla
         sta   t1
         sta   orig
         pla
         sta   t1+2
         sta   orig+2
         pla
         sta   t1+4
         sta   orig+4
         pla
         sta   t1+6
         sta   orig+6
         pla
         sta   t1+8
         sta   orig+8
         phy
         phx
         ph4   #t1                      compute ln(x)
         flnx
         jsr   ~SetErrno                check for a domain or pole error 
lb1      ldx   #^t1                     return a pointer to the result
         lda   #t1
         plb
         rtl
         end

****************************************************************
*
*  log10 - return log(x)
*
*  Inputs:
*        stack - extended number
*
*  Outputs:
*        X-A - addr of the result
*
****************************************************************
*
log10    start
         using MathCommon

         phb                            place the number in a work area
         plx
         ply
         phk
         plb
         pla
         sta   t1
         sta   orig
         pla
         sta   t1+2
         sta   orig+2
         pla
         sta   t1+4
         sta   orig+4
         pla
         sta   t1+6
         sta   orig+6
         pla
         sta   t1+8
         sta   orig+8
         phy
         phx
         ph4   #t1                      compute ln(x)
         flnx
         ph4   #ln10e                   convert from ln base e to log base 10
         ph4   #t1
         fmulx
         jsr   ~SetErrno                check for a domain or pole error 
         ldx   #^t1                     return a pointer to the result
         lda   #t1
         plb
         rtl

ln10e    dc    e'0.43429448190325182765'
         end

****************************************************************
*
*  modf - split a number into whole and fraction parts
*
*  Inputs:
*        x - number
*        nptr - ptr to double to store integer part
*
*  Outputs:
*        returns the address of the result
*
****************************************************************
*
modf     start
         using MathCommon

         csubroutine (10:x,4:nptr),0

         phb
         phk
         plb

         ldx   #8                       t1 := x
lb1      lda   x,X                      t2 := x
         sta   t1,X
         sta   t2,X
         dex
         dex
         bpl   lb1
         ph4   #t2                      t2 := trunc(t2)
         ftintx
         ph4   #t2                      t1 := t1-t2
         ph4   #t1
         fsubx
         ph4   #t2                      convert t2 to a double
         ph4   #t3
         fx2d
lb2      ldy   #6
lb3      lda   t3,Y                     return the integer part
         sta   [nptr],Y
         dey
         dey
         bpl   lb3
         lla   nptr,t1                  set up to return the result

         plb
         creturn 4:nptr
         end

****************************************************************
*
*  pow - return x ** y
*
*  Inputs:
*        stack - extended numbers
*
*  Outputs:
*        X-A - addr of the result
*
****************************************************************
*
pow      start
         using MathCommon

         phb                            place the numbers in the work areas
         plx
         ply
         phk
         plb
         pla
         sta   t1
         sta   orig
         pla
         sta   t1+2
         sta   orig+2
         pla
         sta   t1+4
         sta   orig+4
         pla
         sta   t1+6
         sta   orig+6
         pla
         sta   t1+8
         sta   orig+8
         pla
         sta   t2
         pla
         sta   t2+2
         pla
         sta   t2+4
         pla
         sta   t2+6
         pla
         sta   t2+8
         phy
         phx

         ph4   #t2                      t1 := t1 ** t2
         ph4   #t1
         fxpwry
         jsr   ~SetErrno2               check for a range error
         ldx   #^t1                     return a pointer to the result
         lda   #t1
         plb
         rtl
         end

****************************************************************
*
*  sin - return sin(x)
*
*  Inputs:
*        stack - extended number
*
*  Outputs:
*        X-A - addr of the result
*
****************************************************************
*
sin      start
         using MathCommon

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
         ph4   #t1                      compute the sine
         fsinx
         ldx   #^t1                     return a pointer to the result
         lda   #t1
         plb
         rtl
         end

****************************************************************
*
*  sinh - return hyperbolic sine(x)
*
*  Inputs:
*        stack - extended number
*
*  Outputs:
*        X-A - addr of the result
*
****************************************************************
*
sinh     start
         using MathCommon
;
;  Intialization
;
         phb                            place the number in a work area
         plx
         ply
         phk
         plb
         pla
         sta   t1
         sta   orig
         pla
         sta   t1+2
         sta   orig+2
         pla
         sta   t1+4
         sta   orig+4
         pla
         sta   t1+6
         sta   orig+6
         pla
         sta   t1+8
         sta   orig+8
         phy
         phx
;
;  if abs(t1) >= 2^-33 then t1 :=  0.5*(exp1(t1)+exp1(t1)/(1+exp1(t1)))
;
         lda   t1+8                     branch if the value is less than 2^-33
         and   #$7FFF
         cmp   #$3FDE
         jlt   lb2
         tay                            sign := sign of t1
         lda   t1+8                     t1 := abs(t1)
         and   #$8000
         sta   sign
         sty   t1+8
         ph4   #t1                      t1 := exp1(t1)
         fexp1x
         lda   t1                       t2 := t1
         sta   t2                       t3 := t1
         sta   t3
         lda   t1+2
         sta   t2+2
         sta   t3+2
         lda   t1+4
         sta   t2+4
         sta   t3+4
         lda   t1+6
         sta   t2+6
         sta   t3+6
         lda   t1+8
         sta   t2+8
         sta   t3+8
         cmp   #32767                   if t1 is +inf or +nan then
         beq   lb1                        skip to setting sign
         ph4   #one                     t3 := t3+1
         ph4   #t3
         faddx
         ph4   #t3                      t2 := t2/t3
         ph4   #t2
         fdivx
         ph4   #t2                      t1 := t1+t2
         ph4   #t1
         faddx
         ph4   #half                    t1 := t1*0.5
         ph4   #t1
         fmulx
lb1      lda   sign                     t1 := t1*sign
         ora   t1+8
         sta   t1+8

lb2      jsr   ~SetErrno                check for a range error
         ldx   #^t1                     return a pointer to the result
         lda   #t1
         plb
         rtl
         end

****************************************************************
*
*  sqrt - return sqrt(x)
*
*  Inputs:
*        stack - extended number
*
*  Outputs:
*        X-A - addr of the result
*
****************************************************************
*
sqrt     start
         using MathCommon

         phb                            place the number in a work area
         plx
         ply
         phk
         plb
         pla
         sta   t1
         sta   orig
         pla
         sta   t1+2
         sta   orig+2
         pla
         sta   t1+4
         sta   orig+4
         pla
         sta   t1+6
         sta   orig+6
         pla
         sta   t1+8
         sta   orig+8
         phy
         phx
         ph4   #t1                      compute the square root
         fsqrtx
         jsr   ~SetErrno                check for a domain error
         ldx   #^t1                     return a pointer to the result
         lda   #t1
         plb
         rtl
         end

****************************************************************
*
*  tan - return tan(x)
*
*  Inputs:
*        stack - extended number
*
*  Outputs:
*        X-A - addr of the result
*
****************************************************************
*
tan      start
         using MathCommon

         phb                            place the number in a work area
         plx
         ply
         phk
         plb
         pla
         sta   t1
         sta   orig
         pla
         sta   t1+2
         sta   orig+2
         pla
         sta   t1+4
         sta   orig+4
         pla
         sta   t1+6
         sta   orig+6
         pla
         sta   t1+8
         sta   orig+8
         phy
         phx
         ph4   #t1                      compute the tangent
         ftanx
         jsr   ~SetErrno                check for a range error
lb1      ldx   #^t1                     return a pointer to the result
         lda   #t1
         plb
         rtl
         end

****************************************************************
*
*  tanh - return hyperbolic tangent(x)
*
*  Inputs:
*        stack - extended number
*
*  Outputs:
*        X-A - addr of the result
*
****************************************************************
*
tanh     start
         using MathCommon
;
;  Intialization
;
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
;
;  if abs(t1) >= 2^-33 then t1 :=  -exp1(-2*t1)/(2+exp1(-2*t1))
;
         lda   t1+8                     branch if the value is less than 2^-33
         and   #$7FFF
         cmp   #$3FDE
         jlt   lb2
         tay                            sign := sign of t1
         lda   t1+8                     t1 := abs(t1)
         and   #$8000
         sta   sign
         sty   t1+8
         ph4   #two                     t1 := t1*2
         ph4   #t1
         fmulx
         lda   t1+8                     t1 := -t1
         eor   #$8000
         sta   t1+8
         ph4   #t1                      t1 := exp1(t1)
         fexp1x
lb1      move  two,t2,#10               t2 := 2
         ph4   #t1                      t2 := t2+t1
         ph4   #t2
         faddx
         ph4   #t2                      t1 := t1/t2
         ph4   #t1
         fdivx
         lda   t1+8                     apply sign to t1
         and   #$7FFF
         ora   sign
         sta   t1+8

lb2      ldx   #^t1                     return a pointer to the result
         lda   #t1
         plb
         rtl
         end

****************************************************************
*
*  ~SetErrno - set errno if needed based on input and output values
*
*  Inputs:
*        orig - input value
*        t1 - output value
*
****************************************************************
*
~SetErrno private
         using MathCommon

         lda   t1+8                     if result is inf or nan then
         asl   a
         cmp   #32767*2
         bne   ret
         lda   t1+6                       if result is nan then
         asl   a
         ora   t1+4
         ora   t1+2
         ora   t1
         beq   inf
         lda   orig+8                       if input value was not nan then
         asl   a
         cmp   #32767*2
         bne   lb1
         lda   orig+6
         asl   a
         ora   orig+4
         ora   orig+2
         ora   orig
         bne   ret
lb1      lda   #EDOM                          errno = EDOM
         bra   err                        else if result is inf then

inf      lda   orig+8                       if input value was not inf/nan then
         asl   a
         cmp   #32767*2
         beq   ret
         lda   #ERANGE                        errno = ERANGE

err      sta   >errno
ret      rts
         end

****************************************************************
*
*  ~SetErrno2 - set errno if needed based on two input values
*               and an output value
*
*  Inputs:
*        orig - first input value
*        t2 - second input value
*        t1 - output value
*
****************************************************************
*
~SetErrno2 private
         using MathCommon

         lda   t1+8                     if result is inf or nan then
         asl   a
         cmp   #32767*2
         bne   ret
         lda   t1+6                       if result is nan then
         asl   a
         ora   t1+4
         ora   t1+2
         ora   t1
         beq   inf
         lda   orig+8                       if neither input value was nan then
         asl   a
         cmp   #32767*2
         bne   lb1
         lda   orig+6
         asl   a
         ora   orig+4
         ora   orig+2
         ora   orig
         bne   ret
lb1      lda   t2+8
         asl   a
         cmp   #32767*2
         bne   lb2
         lda   t2+6
         asl   a
         ora   t2+4
         ora   t2+2
         ora   t2
         bne   ret
lb2      lda   #EDOM                          errno = EDOM
         bra   err                        else if result is inf then

inf      lda   orig+8                       if neither input value was inf/nan
         asl   a                              then
         cmp   #32767*2
         beq   ret
         lda   t2+8
         asl   a
         cmp   #32767*2
         beq   ret
         lda   #ERANGE                        errno = ERANGE

err      sta   >errno
ret      rts
         end

