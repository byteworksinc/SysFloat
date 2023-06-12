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

HT       equ   9                        horizontal tab
LF       equ   10                       line feed
VT       equ   11                       vertical tab
FF       equ   12                       form feed
CR       equ   13                       carriage return

         tsc                            set up direct page addressing
         phd
         tcd
         phb                            use our data bank
         phk
         plb
         
         lda   str                      set *ptr to str, if needed
         ldx   str+2
         jsr   setptr
         
sw1      lda   [str]                    skip leading whitespace
         and   #$00FF
         cmp   #' '
         beq   sw2
         cmp   #CR+1
         bge   cn0
         cmp   #HT
         blt   cn0
sw2      inc4  str
         bra   sw1

cn0      ph4   <str                     try to scan str as a hex float string
         jsl   ~HexStr2X
         lda   endPtr                   if it was a hex float string then
         ora   endPtr+2
         beq   cn1
         
         lda   endPtr                     set *ptr to endPtr, if needed
         ldx   endPtr+2
         jsr   setptr

         brl   cn5                        return t1

cn1      ph4   <str                     convert from ascii to decform
         ph4   #index
         ph4   #decrec
         ph4   #valid
         stz   index
         fcstr2dec
         
         lda   index                    if index != 0 then
         jeq   cn4
         
         ph4   #decrec                    convert decform to real
         ph4   #t1
         fdec2x

         ldy   index                      if string is 'infinity' then
         dey
         lda   [str],y
         ora   #$2020
         cmp   #'if'
         bne   cn2
         iny
         iny
         lda   [str],y
         ora   #$2020
         cmp   #'in'
         bne   cn3
         iny
         iny
         lda   [str],y
         ora   #$2020
         cmp   #'yt'
         bne   cn3
         iny
         iny
         sty   index                        extend index to point after it
         bra   cn3
         
cn2      cmp   #'(n'                      if string is 'nan(n-char-seq)' then
         bne   cn3
         iny
cn2a     iny
         beq   cn3                          if does not exceed 64K then
         lda   [str],y
         and   #$00FF
         cmp   #'0'
         blt   cn2b
         cmp   #'9'+1
         blt   cn2a
         cmp   #'_'
         beq   cn2a
         ora   #$0020
         cmp   #'a'
         blt   cn3
         cmp   #'z'+1
         blt   cn2a
cn2b     cmp   #')'
         bne   cn3
         iny
         sty   index                          extend index to point after it
         
cn3      lda   index
         clc                              set *ptr to str+index, if needed
         adc   str
         ldx   str+2
         bcc   cn3a
         inx
cn3a     jsr   setptr
         bra   cn5                      else

cn4      ldx   #8                         t1 := 0
cn4a     stz   t1,x
         dex
         dex
         bpl   cn4a
         lda   #EINVAL                    errno = EINVAL
         sta   >errno

cn5      lda   t1+8                     if result is inf or nan then
         asl   a
         cmp   #32767*2
         bne   cn7
         lda   [str]                      if str is not [+-]inf/nan then
         short M
         cmp   #'-'
         beq   cn6
         cmp   #'+'
         bne   cn6a
cn6      xba
cn6a     long  M
         and   #$00FD
         cmp   #'I'
         bge   cn7
         lda   #ERANGE                      errno = ERANGE
         sta   >errno

cn7      lda   rtl+1                    fix the stack & registers
         sta   ptr+2
         lda   rtl
         sta   ptr+1
         plb
         pld
         tsc
         clc
         adc   #8
         tcs

         ldx   #^t1                     return t1
         lda   #t1
         rtl

;local subroutine: if ptr is not null, save the value in A-X to *ptr
setptr   ldy   ptr                      if ptr != NULL then
         bne   sp1
         ldy   ptr+2
         beq   sp2
         
sp1      sta   [ptr]                      *ptr = A-X
         lda   endPtr+2
         txa
         ldy   #2
         sta   [ptr],y
sp2      rts
         end

****************************************************************
*
*  ~HexStr2X - Parse a hexadecimal floating-point string
*
*  Inputs:
*        str - pointer to the string (C string)
*
*  Outputs:
*        endPtr - points to next byte after the hex string,
*                 or NULL if no hex string was found
*        t1 - converted numeric value (if hex string found)
*
****************************************************************
*
~HexStr2X private
         using ~stdglobals

         subroutine (4:str),28
got_period equ 0                        flag: have we encountered a period?
full     equ   got_period+2             flag: is mantissa full?
mantissa equ   full+2                   mantissa
extrabits equ  mantissa+8               extra bits that do not fit in mantissa
exp_adjust equ extrabits+2              exponent adjustment
negate_exp equ exp_adjust+2             flag: -1 if exponent is negative?
exp      equ   negate_exp+2             exponent
nonzero  equ   exp+2                    flag: is mantissa non-zero?
got_digit equ  nonzero+2                flag: got any digit yet?
overflow equ   got_digit+2              flag: overflow of mant size or exp?
negative equ   overflow+2               flag: -1 if the sign is negative?

         stz   endPtr                   no hex string found yet
         stz   endPtr+2

         stz   got_period               no period yet
         stz   full                     not full yet
         stz   negate_exp               assume positive exponent
         stz   got_digit                no digit yet
         stz   overflow                 no overflow
         stz   negative                 assume positive sign
         stz   exp                      exponent value = 0
         stz   mantissa                 mantissa = 0.0
         stz   mantissa+2
         stz   mantissa+4
         stz   mantissa+6
         stz   extrabits                extrabits = 0
         lda   #63                      exponent adjustment = 63
         sta   exp_adjust

         jsr   firstch                  check for + or - sign
         cmp   #'+'
         beq   check_0
         cmp   #'-'
         bne   check_0b
         dec   negative
         
check_0  jsr   nextch                   check for 0x or 0X prefix
check_0b cmp   #'0'
         beq   check_x
         brl   ret
check_x  jsr   nextch
         and   #$df
         cmp   #'X'
         beq   digitlp
         brl   ret
         
digitlp  jsr   nextch                   get a character
         ldx   got_period               if there was no period yet
         bne   get_hex
         cmp   #'.'                       if character is '.'
         bne   get_hex
         dec   got_period                   flag that we got a period
         bra   digitlp                      loop for another digit
get_hex  jsr   hexdigit                 try to get a hex digit value
         bcs   mantdone                 if not a hex digit, mantissa is done
         ldx   #1                       flag that we got a digit
         stx   got_digit
         ldx   full                     if mantissa is full
         beq   donibble
         ora   extrabits                  record extra bits for rounding
         sta   extrabits
         lda   got_period                 if we are not past the period
         bne   digitlp
         lda   #4                           exp_adjust += 4
         clc
         adc   exp_adjust
         sta   exp_adjust
         bvc   digitlp                      if mantissa is too big to handle
         ldx   #1
         stx   overflow                       flag overflow
         bra   digitlp                    loop for another digit

donibble xba                            get nibble value in high bits
         asl   a
         asl   a
         asl   a
         asl   a
         ldx   #4                       for each bit in nibble:
bitloop  bit   mantissa+6                 if mantissa is now full
         bpl   notfull
         inc   full                         full = true
         sta   extrabits                    record next bit(s) for rounding
         lda   got_period                   if we are not past the period
         bne   digitlp
         txa                                  exp_adjust += number of extra bits
         clc
         adc   exp_adjust
         sta   exp_adjust
         bra   digitlp                      loop for another digit
notfull  asl   a                          shift bit into mantissa
         rol   mantissa
         rol   mantissa+2
         rol   mantissa+4
         rol   mantissa+6
         bit   got_period                 if we are past the period
         bpl   nextbit
         dec   exp_adjust                   exp_adjust--
         ldy   exp_adjust                   if mantissa is too small to handle
         cpy   #$7fff
         bne   nextbit
         sty   overflow                       flag overflow
nextbit  dex
         bne   bitloop
         bra   digitlp

mantdone tay                            save character
         lda   got_digit                if there was not a mantissa digit
         jeq   ret                        return indicating no hex string
         lda   str                      save current str position to endPtr
         sta   endPtr
         lda   str+2
         sta   endPtr+2
         stz   got_digit                no exponent digits so far

         lda   mantissa                 check if mantissa is nonzero
         ora   mantissa+2
         ora   mantissa+4
         ora   mantissa+6
         sta   nonzero                  set nonzero flag as appropriate
         beq   do_exp                   if mantissa is nonzero, normalize:
         lda   mantissa+6                 if high bit of mantissa is not 1:
         bmi   do_exp                       do    
normallp dec   exp_adjust                     exp_adjust--
         ldx   exp_adjust                     if mantissa is too small to handle
         cpx   #$7fff
         bne   no_ovf
         stx   overflow                         flag overflow
no_ovf   asl   mantissa                       shift mantissa left one bit
         rol   mantissa+2
         rol   mantissa+4
         rol   mantissa+6
         bpl   normallp                     while high bit of mantissa is not 1
         
do_exp   cpy   #'P'                     if last character was not 'p' or 'P'
         bne   adj_exp                    skip exponent part
         
         jsr   nextch                   get next character
         cmp   #'+'                     if it is '+'
         bne   chkminus
         jsr   nextch                     ignore it and get next char
         bra   exploop
chkminus cmp   #'-'                     else if it is '-'
         bne   exploop
         jsr   nextch                     get next character
         dec   negate_exp                 flag that exponent is negative
exploop  jsr   decdigit                 for each exponent digit
         bcs   neg_exp
         ldx   #1                         flag that we got a digit
         stx   got_digit
         asl   exp                        exp = exp*10 + digit
         pei   exp
         bcs   bigexp
         bmi   bigexp
         asl   exp
         asl   exp
         bcs   bigexp
         bmi   bigexp
         adc   1,s
         bvs   bigexp
         clc
         adc   exp
         bvs   bigexp
         sta   exp
         pla
         jsr   nextch
         bra   exploop
bigexp   pla
         ldx   #$7fff                   if exponent value overflows
         stx   exp                        exp = INT_MAX
         lda   nonzero                    if mantissa was not zero
         beq   bigexplp
         lda   exp_adjust                   if exp_adjust and exp signs differ
         eor   negate_exp
         bpl   bigexplp
         cmp   #-16383                        if |exp_adjust| is large
         bge   bigexplp
         stx   overflow                         flag overflow
bigexplp jsr   nextch
         jsr   decdigit
         bcc   bigexplp
neg_exp  lda   negate_exp               if exponent is negative
         beq   finalexp
         lda   exp                        negate exp
         eor   #$ffff
         inc   a
         sta   exp
finalexp lda   got_digit                if there was an exponent digit
         beq   adj_exp
         lda   str                        save current str position to endPtr
         sta   endPtr
         lda   str+2
         sta   endPtr+2
adj_exp  lda   exp                      add in exponent adjustment
         clc
         adc   exp_adjust
         bvc   expdone                  if addition overflows
         lda   #$7fff                     positive exponent -> INT_MAX
         ldx   negate_exp
         beq   expdone
         inc   a                          negative exponent -> INT_MIN
expdone  ldy   overflow                 if overflow occurred
         jne   error                      return a NaN
         ldx   nonzero                  if value is zero
         bne   bias
         txa                              exponent field = 0
         bra   storeexp

bias     clc                            else
         adc   #16383                     compute biased exp. [-16385..49150]
storeexp sta   exp
         cmp   #32767                   if it is [0..32766], it is valid
         blt   round
         cmp   #32767+16383+1           if it is larger, generate an infinity
         blt   inf                      otherwise, denormalize:
denormlp lsr   mantissa+6               while biased exponent is negative:
         ror   mantissa+4                 shift mantissa left one bit
         ror   mantissa+2
         ror   mantissa
         ror   extrabits                  adjust extrabits
         bcc   dn_next
         lda   extrabits
         ora   #1
         sta   extrabits
dn_next  inc   exp                        exp++
         bmi   denormlp
         
round    lda   extrabits                if there are extra bits
         beq   done
         FGETENV                          get rounding direction
         txa
         asl   a
         bcs   roundDn0
         bmi   roundUp                    if rounding to nearest then         
         lda   extrabits                    if less than halfway to next number
         bpl   done                           return value as-is
         asl   a                            if more than halfway to next number
         bne   do_round                       apply rounding
         lda   mantissa                     if exactly halfway to next number
         lsr   a                              if least significant bit is 0
         bcc   done                             return value as-is
         bra   do_round                       else apply rounding

roundUp  lda   negative                   if rounding upward
         bne   done                         if number is positive
         bra   do_round                       apply rounding

roundDn0 bmi   done                       if rounding downward
         lda   negative                     if number is negative
         beq   done                           apply rounding

do_round inc   mantissa                   perform the rounding if needed:
         bne   done                         increment mantissa
         inc   mantissa+2
         bne   done
         inc   mantissa+4
         bne   done
         inc   mantissa+6
         bne   done
         lda   #$8000                       if mantissa overflowed:
         sta   mantissa+6                     mantissa = 1.0
         inc   exp                            exp++ (could generate an infinity)

done     lda   mantissa                 done: store return value
         sta   t1
         lda   mantissa+2
         sta   t1+2
         lda   mantissa+4
         sta   t1+4
         lda   mantissa+6
         sta   t1+6
         lda   exp
         asl   a
         bra   setsign

error    lda   #$C011                   error: return NANASCBIN
         sta   t1+6
         bra   nanorinf

inf      lda   #$8000                   infinity: exponent field = 32767
         sta   t1+6                     mantissa = 1.0
nanorinf stz   t1+4
         stz   t1+2
         stz   t1
         lda   #32767*2
         
setsign  lsr   negative                 set sign
         ror   a
         sta   t1+8
ret      return                         return

;get next character of string
nextch   inc4  str
firstch  lda   [str]
         and   #$00FF
         rts

;get value of A, taken as a hex digit
;If A was a hex digit, returns with carry clear and digit value in A.
;If not, returns with carry set and A unmodified (if < '0') or A = A & 0xDF.
hexdigit cmp   #'0'
         blt   baddigit
         cmp   #'9'+1
         bge   letter
         and   #$000F
;        clc
         rts
letter   and   #$df
         cmp   #'A'
         blt   baddigit
         cmp   #'F'+1
         bge   baddigit
         and   #$000F
         adc   #9
;        clc
         rts

;get value of A, taken as a decimal digit
;If A was a decimal digit, returns with carry clear and digit value in A.
;If not, returns with carry set and A unmodified.
decdigit cmp   #'0'
         blt   baddigit
         cmp   #'9'+1
         bge   baddigit
         and   #$000F
;        clc
         rts
baddigit sec
         rts
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

endPtr   anop                           end of hex string (hex conversion only)
index    ds    2                        index for number conversions
valid    ds    2                        valid flag for SANE scanner
;
;  temp work space/return value
;
t1       ds    10                       real number
         end
