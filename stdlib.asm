         keep  obj/stdlib
         mcopy stdlib.macros
         case  on

****************************************************************
*
*  StdDef - Standard Definitions
*
*  This code implements the tables and subroutines needed to
*  support the standard C library stdlib.
*
*  This porition of the library contains subroutines that are
*  dependent on the way floating-point calculations are
*  performed.  The main portion of the library is in ORCACLib.
*
*  December 1988
*  Mike Westerfield
*
*  Copyright 1988
*  Byte Works, Inc.
*
****************************************************************
*
StdDef   start                          dummy segment
         copy  equates.asm

         end

****************************************************************
*
*  strtod - convert string to extended sane
*
*  Inputs:
*        str - pointer to the string
*        ptr - pointer to a pointer; a pointer to the first
*              char past the number is placed here.  If ptr is
*              nil, no pointer is returned
*
*  Outputs:
*        X-A - pointer to result
*
****************************************************************
*
strtod   start
         using ~stdglobals
ptr      equ   8                        *return pointer
str      equ   4                        string pointer
rtl      equ   1                        return address

         tsc                            set up direct page addressing
         phd
         tcd
         phb                            use our data bank
         phk
         plb

	lda	str	if str = NULL then
	ora	str+2
	beq	cn2	  flag an error
         ph4   str                      lvp^.rval := cnvsr(digit);
         ph4   #index                   {convert from ascii to decform}
         ph4   #decrec
         ph4   #valid
         stz   index
         fcstr2dec
         lda   index                    {flag an error if SANE said to}
         beq   cn2
	ph4   #decrec                  {convert decform to real}
         ph4   #t1
         fdec2x
         ldx   #^t1                     set the return value
         ldy   #t1
         bcc   cn4                      branch if there was no error

         fgetenv                        if overflow then
         txa
         bit   #$0400
         beq   cn2
         ldx   #^huge_val                 return *huge_val
         ldy   #huge_val
         bra   cn3

cn2      ldx   #^zero                   return *0.0
         ldy   #zero
cn3      lda   #ERANGE                  errno = ERANGE
         sta   >errno
         stz   index                    index = 0

cn4      lda   ptr                      if ptr != NULL then
         ora   ptr+2
         beq   cn5
         phy                              *ptr = str+index
         ldy   #2
         clc
         lda   index
         adc   str
         sta   [ptr]
         lda   str+2
         adc   #0
         sta   [ptr],Y
         ply

cn5      lda   rtl+1                    fix the stack & registers
         sta   ptr+2
         lda   rtl
         sta   ptr+1
         plb
         pld
         tsc
         clc
         adc   #8
         tcs
         tya                            set the return value

         rtl                            return
         end

****************************************************************
*
*  ~stdglobals - standard globals
*
****************************************************************
*
~stdglobals privdata
;
;  globals used to convert strings to real numbers
;
decrec   anop                           decimal record
sgn      ds    2                        sign
exp      ds    2                        exponent
sig      ds    29                       significant digits

index    ds    2                        index for number conversions
valid    ds    2                        valid flag for SANE scanner
;
;  floating point constants
;
zero     dc    e'0.0'
huge_val dc    e'3.4e38'
;
;  temp work space
;
t1       ds    10                       real number
         end
