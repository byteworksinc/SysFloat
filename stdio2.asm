         keep  obj/stdio2
         mcopy stdio2.macros
         case  on

****************************************************************
*
*  StdIO - Standard I/O Library
*
*  This code implements the tables and subroutines needed to
*  support the standard C library STDIO.
*
*  This porition of the library contains subroutines that are
*  dependent on the way floating-point calculations are
*  performed.  The main portion of the library is in ORCACLib.
*
*  November 1988
*  Mike Westerfield
*
*  Copyright 1988
*  Byte Works, Inc.
*
****************************************************************
*
StdIO2   start                          dummy segment
         copy  equates.asm

         end

****************************************************************
*
*  ~Format_e - format a double precision number
*  ~Format_E - format a double precision number
*  ~Format_f - format a double precision number
*
*  Inputs:
*        ~fieldWidth - output field width
*        ~paddChar - padd character
*        ~leftJustify - left justify the output?
*        ~precision - precision of output
*        ~precisionSpecified - was the precision specified?
*        ~sign - char to use for positive sign
*
****************************************************************
*
~Format_e start
         using ~printfCommon
argp     equ   7                        argument
;
;  Set/clear the case flag and style
;
         stz   case
         bra   fl1
~Format_E entry
         lda   #1
         sta   case
fl1      stz   ~style                   exponential format
         inc   ~precision
         bra   sn1
~Format_f entry
         lda   #1
         sta   ~style
         stz   case
;
;  If the value is negative, use the sign flag
;
sn1      ldy   #8                       if the value is negative then
         lda   [argp],Y
         bpl   cn0
         eor   #$8000                     reverse the sign
         sta   [argp],Y
sn2      lda   #'-'                       set the sign character
         sta   ~sign
;
;  Convert the number to an ASCII string
;
cn0      lda   ~precisionSpecified      if the precision was not specified then
         bne   cn1
         lda   #7                         use a precision of 7-~style
         sec
         sbc   ~style
         bra   cn2
cn1      lda   ~precision
cn2      sta   ~digits                  set the precision

         ph4   #~decForm                convert do a decimal record
         ph4   argp
         ph4   #~decRec
         fx2dec
         ph4   #~decForm                convert to a string
         ph4   #~decRec
         ph4   #~str
         fdec2str

         lda   ~style                   if the format is exponential then
         bne   cn2a
         short I,M                        if the exponent has only one digit then
         ldx   ~str
         lda   ~str-2,X
         cmp   #'e'
         bne   cn2a
         inc   ~str                         extend it to 2 digits
         lda   ~str,X
         sta   ~str+1,X
         lda   #'0'
         sta   ~str,X
cn2a     long  I,M

         lda   ~removeZeros             if we are to remove insignificant zeros
         beq   cn2c                       then
cn2b     jsr   Strip                      while there are any, remove them
         bcs   cn2b

cn2c     lda   case                     if the E should be uppercase then
         beq   cn5
         short I,M                        find the 'e'
         ldx   ~str
cn3      lda   ~str,X
         cmp   #'e'
         bne   cn3a
         lda   #'E'
         sta   ~str,X
         bra   cn4
cn3a     dex
         bne   cn3
cn4      long  I,M

cn5      lda   ~str+1                   if the first char is a space then
         and   #$00FF
         cmp   #' '
         bne   cn5a
         dec   ~str                       remove it
         move  ~str+2,~str+1,#l:~str-2

cn5a     lda   ~digits                  if ~digits = 0 then
         bne   dp0
         lda   ~altForm                   if ~altForm then
         beq   dp0
         short I,M                          insert a '.' into the string
         inc   ~str
         ldx   #1
cn6      lda   ~str,X
         cmp   #'0'
         blt   cn7
         cmp   #'9'+1
         bge   cn7
         inx
         cpx   ~str
         bne   cn6
cn7      ldy   ~str,X
         lda   #'.'
         sta   ~str,X
cn8      cpx   ~str
         beq   cn9
         inx
         tya
         ldy   ~str,X
         sta   ~str,X
         bra   cn8
cn9      long  I,M
;
;  Determine the padding and do left padding
;
dp0      lda   ~sign                    if the sign is non-zero, allow for it
         beq   dp1
         dec   ~fieldWidth
dp1      lda   ~str                     determine the length of the buffer
         and   #$00FF
         sec                            subtract it from ~fieldWidth
         sbc   ~fieldWidth
         eor   #$FFFF
         inc   a
         sta   ~fieldWidth
         lda   ~paddChar                skip justification if we are padding
         cmp   #'0'
         beq   pn0
         jsr   ~RightJustify            handle right justification
;
;  Print the number
;
pn0      lda   ~sign                    if there is a sign character then
         beq   pn1
         pha                              print it
         jsl   ~putchar
pn1      lda   ~paddChar                if padding with 0s then
         cmp   #'0'
         bne   pn1b
         lda   ~fieldWidth                padd the field
         beq   pn1b
         bmi   pn1b
pn1a     ph2   #'0'
         jsl   ~putchar
         dec   ~fieldWidth
         bne   pn1a
pn1b     lda   ~str                     print the string
         and   #$00FF
         tay
         beq   rn1
         ldx   #1
pn2      lda   ~str,X
         phx
         phy
         and   #$00FF
         pha
         jsl   ~putchar
         ply
         plx
         inx
         dey
         bne   pn2
;
;  remove the number from the argument list
;
rn1      add2  argp,#10
;
;  Handle left justification
;
         brl   ~LeftJustify             handle left justification
;
;  Strip - strip an insignificant zero or '.', returning C=1 if there may be more
;
Strip    short I,M                      quit of there is no '.'
         ldx   #0
st1      lda   ~str+1,X
         cmp   #'.'
         beq   st2
         inx
         cpx   ~str
         bne   st1
         bra   st5
st2      cpx   ~str                     scan to the 'e' or end of string
         beq   st3
         lda   ~str+1,X
         cmp   #'e'
         beq   st3
         inx
         bra   st2
st3      lda   ~str,X                   if the previous char is a '0' then
         cmp   #'0'
         bne   st4
         jsr   Remove                     remove it
         sec                              there may be more
         bra   st6
st4      cmp   #'.'                     else if it is a '.' then
         bne   st5
         jsr   Remove                     remove it
st5      clc                            there are no more
st6      long  I,M
         rts
;
;  Remove - remove a character at X
;
         longi off
         longa off
Remove   cpx   ~str
         beq   rm1
         lda   ~str+1,X
         sta   ~str,X
         inx
         bra   Remove
rm1      dec   ~str
         rts
         longi on
         longa on
;
;  Local Data
;
case     ds    2
         end

****************************************************************
*
*  ~Format_g - format a double precision number
*  ~Format_G - format a double precision number
*
*  Inputs:
*        ~fieldWidth - output field width
*        ~paddChar - padd character
*        ~leftJustify - left justify the output?
*        ~precision - precision of output
*        ~precisionSpecified - was the precision specified?
*        ~sign - char to use for positive sign
*
*  Notes:
*        This subroutine works by manipulating flags and calling
*        one of the other floating point foramt routines.
*
****************************************************************
*
~Format_g start
         using ~printfCommon
argp     equ   7                        argument
;
;  Set/clear the case flag
;
         stz   case
         bra   fl1
~Format_G entry
         lda   #1
         sta   case
fl1      anop
;
;  Find the exponent and precision
;
         lda   ~precisionSpecified      if the precision was not specified then
         bne   cn1
         lda   #6                         use a precision of 6
         sta   ~precision
         inc   ~precisionSpecified        note that it is givin
cn1      anop

         lda   ~precision               set the precision
         sta   ~digits
         ph4   #~decForm                convert do a decimal record
         ph4   argp
         ph4   #~decRec
         fx2dec
;
;  Modify flags and format the number
;
         lda   ~altForm                 strip zeros?
         eor   #1
         sta   ~removeZeros
         lda   #1                       force '.' unless removed by stripping
         sta   ~altForm
         lda   ~sig                     if (~exp < -3) or (~exp > ~precision) then
         and   #$00FF
         clc
         adc   ~exp
         dec   A
         sta   aexp
         bpl   mf1
         cmp   #-3
         bge   mf3
mf1      cmp   ~precision
         ble   mf3
         dec   ~precision                 ~precision -= 1
         bpl   mf1a
         stz   ~precision
mf1a     lda   case                       if case then
         bne   mf2
         brl   ~Format_e                    use e format specifier
mf2      brl   ~Format_E                  else use E format specifier
mf3      sec                            else
         lda   ~precision                 ~precision -= ~exp
         sbc   aexp
         brl   ~Format_f                  use f format specifier

case     ds    2
aexp     ds    2
         end

****************************************************************
*
*  ~Scan_f - read a real number
*
*  Inputs:
*        ~scanError - has a scan error occurred?
*        ~scanWidth - max input length
*        ~suppress - suppress save?
*        ~size - size specifier
*
****************************************************************
*
~Scan_f  start
         using ~scanfCommon
         using ~printfCommon
arg      equ   11                       argument

FX2X_op  equ   $0010                    SANE opwords for conversions
FX2D_op  equ   $0110
FX2S_op  equ   $0210

;
;  Read the ASCII version of the number
;
         stz   read                     no characters read yet
         stz   disp                     no characters in the buffer yet
         lda   ~scanWidth               make sure we have a scan width
         bne   lb1
         dec   ~scanWidth
lb1      jsl   ~getchar                 skip leading whitespace...
         cmp   #EOF                     if at EOF then
         bne   lb1a
         dec   ~assignments                no assignment made
         sta   ~eofFound                   eofFound = EOF
         brl   lb12                        bail out
lb1a     tax                            ...back to skipping whitespace
         lda   __ctype+1,X
         and   #_space
         bne   lb1
         txa
         dec   ~scanWidth

lb2      cmp   #'+'                     allow a leading sign
         beq   lb3
         cmp   #'-'
         bne   lb5
lb3      jsr   PutChar
lb4      jsr   GetChar
         bcs   lb10
lb5      cmp   #'0'                     skip leading 0's
         beq   lb4

         pha                            make sure there is at least one digit
         lda   #'0'
         jsr   PutChar
         pla
         ldx   #20                      set the max digit count
         stx   maxDig
         jsr   GetDigits                get the digits before the '.'
         bcs   lb10
         cmp   #'.'                     get the optional '.'
         bne   lb6
         jsr   PutChar
         jsr   GetChar
         bcs   lb10
         jsr   GetDigits                get the fraction digits
         bcs   lb10
lb6      cmp   #'e'                     allow for an exponent
         beq   lb7
         cmp   #'E'
         bne   lb9a
lb7      jsr   PutChar
         jsr   GetChar                  allow for an exponent sign
         bcs   lb10
         cmp   #'+'
         beq   lb8
         cmp   #'-'
         bne   lb9
lb8      jsr   PutChar
         jsr   GetChar
         bcs   lb10
lb9      ldx   #20                      allow for an exponent
         stx   maxDig
         jsr   GetDigits
         bcs   lb10
lb9a     jsl   ~putback                 return the last char to the input stream

lb10     lda   read                     if no chars read then
         bne   lb10a
         inc   ~scanError                 ~scanError = true
         dec   ~assignments               no assignment made
         bra   lb12                       skip the save
lb10a    lda   ~suppress                quit if output is suppressed
         bne   lb13
         ldy   disp                     if the last char is not a digit then
         lda   ~str-1,Y
         and   #$00FF
         cmp   #'0'
         blt   dg1
         cmp   #'9'+1
         blt   dg2
dg1      lda   #'0'                       place a zero in the number
         jsr   PutChar
dg2      ph4   #0                       convert to an extended number
         ph4   #~str
         jsl   strtod
         phx                            convert and save
         pha
         ph4   arg
         dec   ~size
         beq   lb11
         bmi   lb11a
         ph2   #FX2X_op
         bra   lb12
lb11     ph2   #FX2D_op
         bra   lb12
lb11a    ph2   #FX2S_op
lb12     _SANEFP816
         lda   ~suppress                quit if output is suppressed
         bne   lb13
         ldy   #2                       remove the parameter from the stack
         jsr   ~RemoveWord
lb13     rts
;
;  GetChar - get a character; return C set of no more can be read
;
GetChar  lda   ~scanWidth
         beq   gc1
         jsl   ~getchar
         inc   read
         dec   ~scanWidth
         clc
         rts

gc1      sec
         rts
;
;  PutChar - out a character in the string
;
PutChar  ldy   disp
         sta   ~str,Y
         inc   disp
         rts
;
;  GetDigits - read a stream of digits
;
GetDigits cmp  #'0'
         blt   gd1
         cmp   #'9'+1
         bge   gd1
         jsr   PutChar
         jsr   GetChar
         bcs   gd2
         dec   maxDig
         bne   GetDigits
gd1      clc
gd2      rts
;
;  Local data
;
disp     ds    2                        disp into ~str
maxDig   ds    2                        max # of digits GetDigits can return
read     ds    2                        # chars read
         end
