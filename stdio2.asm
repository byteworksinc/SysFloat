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
         bra   sn1
~Format_F entry
         lda   #1
         sta   ~style
         sta   case
;
;  If the value is negative, use the sign flag
;
sn1      ldy   #8                       if the value is negative then
         lda   [argp],Y
         bpl   cn0
         asl   a                          reverse the sign
         lsr   a
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
         bra   cn1a
cn1      lda   ~precision
cn1a     sta   ~digits                  set the precision

         ph4   #~decForm                convert do a decimal record
         ph4   <argp
         ph4   #~decRec
         fx2dec
         ph4   #~decForm                convert to a string
         ph4   #~decRec
         ph4   #~str
         fdec2str
         
         lda   ~sig+1                   if number is Inf or NaN then
         and   #$0040
         beq   cn2
         lsr   a                          do not use '0' padding
         sta   ~paddChar

cn2      lda   ~style                   if the format is exponential then
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

cn5a     lda   ~digits                  if digits after decimal point = 0 then
         ldx   ~style
         bne   cn5b
         dec   a
cn5b     tax
         bne   cn10
         lda   ~altForm                   if ~altForm then
         beq   cn10
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
cn10     lda   case                     if letters should be lowercase than
         bne   dp0
         ldx   #2                         force first three chars to lowercase
cn11     lda   ~str,X                       (this covers INF or NAN)
         ora   #$2020
         sta   ~str,X
         dex
         bne   cn11
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
cn1      lda   ~precision               if precision is 0 then
         bne   cn2
         inc   a                          treat it as being 1
         sta   ~precision

cn2      sta   ~digits                  set the precision
         stz   ~style                   use exponential style for conversion
         ph4   #~decForm                convert do a decimal record
         ph4   <argp
         ph4   #~decRec
         fx2dec
;
;  Modify flags and format the number
;
         lda   ~altForm                 strip zeros?
         eor   #1
         sta   ~removeZeros
         lda   ~sig                     if sig is '0' then
         cmp   #1+'0'*256
         bne   mf0
         stz   ~exp                       set ~exp to zero
mf0      and   #$00FF                   if (exp < -4) or (exp >= ~precision) then
         clc
         adc   ~exp
         dec   A
         bpl   mf1
         cmp   #-4
         bge   mf3
mf1      cmp   ~precision
         blt   mf3
         dec   ~precision                 ~precision -= 1
mf1a     lda   case                       if case then
         bne   mf2
         brl   ~Format_e                    use e format specifier
mf2      brl   ~Format_E                  else use E format specifier
mf3      clc                            else
         eor   #$FFFF                     ~precision -= exp + 1
         adc   ~precision
         sta   ~precision
         lda   case                       if case then
         bne   mf4
         brl   ~Format_f                    use f format specifier
mf4      brl   ~Format_F                  else use F format specifier

case     ds    2
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
         stz   gotDigit                 no characters read yet
         stz   hex                      assume not in hex format
         stz   disp                     no characters in the buffer yet
lb1      jsl   ~getchar                 skip leading whitespace...
         cmp   #EOF                     if at EOF then
         bne   lb1b
         sta   ~eofFound                   eofFound = EOF
         lda   ~suppress                   if assignment is not suppressed then
         bne   lb1a
         dec   ~assignments                  no assignment made
lb1a     brl   lb12                        bail out
lb1b     tax                            ...back to skipping whitespace
         lda   __ctype+1,X
         and   #_space
         bne   lb1
         txa
         dec   ~scanWidth

         cmp   #'+'                     allow a leading sign
         beq   lb2
         cmp   #'-'
         bne   lb2a
lb2      jsr   NextChar
         jcs   lb10err
lb2a     cmp   #'0'                     check for leading 0x
         bne   lb3
         sta   gotDigit
         jsr   NextChar
         jcs   lb10good
         ldy   #'X'
         jsr   CmpLetter
         jcc   lb5b
lb2b     inc   hex                      if 0x found, flag as hex
         stz   gotDigit
         jsr   NextChar
         bcs   to_lb10err
         brl   lb5c

lb3      ldy   #'I'                     check for INF or INFINITY
         jsr   CmpLetter
         bcc   lb4
         jsr   NextChar
         bcs   to_lb10err
         ldy   #'N'
         jsr   CmpLetter
         bcc   to_lb9a
         jsr   NextChar
         bcs   to_lb10err
         ldy   #'F'
         jsr   CmpLetter
         bcc   to_lb9a
         jsr   NextChar
         bcs   to_lb10good
         ldy   #'I'
         jsr   CmpLetter
         bcc   infOrNan
         jsr   NextChar
         bcs   to_lb10err
         ldy   #'N'
         jsr   CmpLetter
         bcc   to_lb9a
         jsr   NextChar
         bcs   to_lb10err
         ldy   #'I'
         jsr   CmpLetter
         bcc   to_lb9a
         jsr   NextChar
         bcs   to_lb10err
         ldy   #'T'
         jsr   CmpLetter
         bcc   to_lb9a
         jsr   NextChar
         bcs   to_lb10err
         ldy   #'Y'
         jsr   CmpLetter
         bcc   to_lb9a
         jsr   PutChar
to_lb10good anop
         brl   lb10good

to_lb9a  brl   lb9a
to_lb10err brl lb10err

lb4      ldy   #'N'                     check for NAN or NAN(n-char-sequence)
         jsr   CmpLetter
         bcc   lb5c
         jsr   NextChar
         bcs   to_lb10err
         ldy   #'A'
         jsr   CmpLetter
         bcc   to_lb9a
         jsr   NextChar
         bcs   to_lb10err
         ldy   #'N'
         jsr   CmpLetter
         bcc   to_lb9a
         jsr   NextChar
         cmp   #'('
         beq  lb4a
infOrNan jsl   ~putback
         bra   to_lb10good

lb4a     jsr   NextChar
         cmp   #'0'
         blt   lb4b
         cmp   #'9'+1
         blt   lb4a
         cmp   #'A'
         blt   lb9a
         cmp   #'Z'+1
         blt   lb4a
         cmp   #'_'
         beq   lb4a
         cmp   #'a'
         blt   lb9a
         cmp   #'z'+1
         blt   lb4a
lb4b     cmp   #')'
         beq   lb10good
         bra   lb9a

lb5      jsr   GetChar
         bcs   lb10
lb5b     cmp   #'0'                     skip leading 0's (decimal only)
         beq   lb5

lb5c     jsr   GetDigits                get the digits before the '.'
         bcs   lb10
         cmp   #'.'                     get the optional '.'
         bne   lb6
         jsr   NextChar
         bcs   lb10
         jsr   GetDigits                get the fraction digits
         bcs   lb10
lb6      ldx   gotDigit                 error if no digits found
         beq   lb9a
         ldy   #'E'                     allow for an exponent
         ldx   hex
         beq   lb6c
         ldy   #'P'
lb6c     jsr   CmpLetter
         bcc   lb9a
lb7      jsr   NextChar                 allow for an exponent sign
         bcs   lb10err
         cmp   #'+'
         beq   lb8
         cmp   #'-'
         bne   lb9
lb8      jsr   NextChar
         bcs   lb10err
lb9      stz   gotDigit                 get exponent digits
         stz   hex
         jsr   GetDigits
         bcs   lb10
lb9a     jsl   ~putback                 return the last char to the input stream

lb10     lda   gotDigit                 if no digits read then
         bne   lb10good
lb10err  inc   ~scanError                 ~scanError = true
         lda   ~suppress                  if assignment is not suppressed then
         bne   lb12
         dec   ~assignments                 no assignment made
         bra   lb12                       skip the save
lb10good lda   ~suppress                quit if output is suppressed
         bne   lb13

         ph4   #0                       convert to an extended number
         ldy   disp                       (if too long, give a NaN)
         cpy   #l:~str
         blt   dg1
         ph4   #nanascbin
         bra   dg2
dg1      ph4   #~str
dg2      jsl   strtod
         phx                            convert and save
         pha
         ph4   <arg
         dec   ~size
         beq   lb11
         bmi   lb11a
         ph2   #FX2X_op
         bra   lb11b
lb11     ph2   #FX2D_op
         bra   lb11b
lb11a    ph2   #FX2S_op
lb11b    _SANEFP816
lb12     lda   ~suppress                quit if output is suppressed
         bne   lb13
         ldy   #2                       remove the parameter from the stack
         jsr   ~RemoveWord
lb13     rts
;
;  GetChar - get a character; return C set of no more can be read
;  NextChar - equivalent to PutChar followed by GetChar
;
NextChar jsr   PutChar
GetChar  lda   ~scanWidth
         beq   gc1
         jsl   ~getchar
         dec   ~scanWidth
         clc
         rts

gc1      sec
         rts
;
;  PutChar - out a character in the string
;
PutChar  ldy   disp
         cpy   #l:~str-1
         beq   pc1
         bge   pc2
         sta   ~str,Y
pc1      inc   disp
pc2      rts
;
;  GetDigits - read a stream of digits
;
GetDigits cmp  #'0'
         blt   gd1
         cmp   #'9'+1
         blt   gd0
         ldx   hex
         beq   gd1
         cmp   #'A'
         blt   gd1
         cmp   #'F'+1
         blt   gd0
         cmp   #'a'
         blt   gd1
         cmp   #'f'+1
         bge   gd1
gd0      sta   gotDigit
         jsr   NextChar
         bcs   gd2
         bra   GetDigits
gd1      clc
gd2      rts
;
;  CmpLetter - Compare character in A to a letter in Y, case-insensitively.
;  Returns with C set if it matches, or clear if it does not. 
;
CmpLetter pha
         tya
         cmp   1,s
         beq   cl1
         ora   #$20
         cmp   1,s
         beq   cl1
         clc
cl1      pla
         rts
;
;  Local data
;
disp     ds    2                        disp into ~str
gotDigit ds    2                        got a digit?
hex      ds    2                        is this hex float format?

nanascbin dc   c'nan(17)',i1'0'
         end
