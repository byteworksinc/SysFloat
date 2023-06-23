         keep  obj/int64
         mcopy int64.macros
         case  off

****************************************************************
*
*  Int64 - 64-bit integer conversion routines.
*
*  This code implements routines called by ORCA/C generated
*  code for conversions between 64-bit integers and reals.
*
****************************************************************
*
Int64    private                        dummy segment
         end

****************************************************************
*
*  ~CnvULongLongReal - convert an unsigned long long integer 
*        into an extended SANE real
*
*  Inputs:
*        unsigned long long int on stack
*
*  Outputs:
*        extended real on stack
*
****************************************************************
*
~CnvULongLongReal start
mantissa equ   4                        mantissa (integer and fraction)
exponent equ   mantissa+8               biased exponent and sign bit

         lda   1,S                      move return value
         pha
         lda   4,S
         sta   2,S
         tsc                            set up DP
         phd
         tcd

         lda   mantissa+2               move 64-bit value to mantissa
         sta   mantissa
         lda   mantissa+4
         sta   mantissa+2
         lda   mantissa+6
         sta   mantissa+4
         lda   mantissa+8
         sta   mantissa+6

         ora   mantissa                 if value is 0 then
         ora   mantissa+2
         ora   mantissa+4
         beq   ret                        return

         lda   #63+16383                set initial exponent (2^63) and sign
         sta   exponent

         lda   mantissa+6               if number is normalized (i=1) then
         bmi   ret                        return

lp1      dec   exponent                 normalize number
         asl   mantissa
         rol   mantissa+2
         rol   mantissa+4
         rol   mantissa+6
         bpl   lp1

ret      pld
         rtl
         end

****************************************************************
*
*  ~CnvLongLongReal - convert a long long integer into
*        an extended SANE real
*
*  Inputs:
*        signed long long int on stack
*
*  Outputs:
*        extended real on stack
*
****************************************************************
*
~CnvLongLongReal start
mantissa equ   4                        mantissa (integer and fraction)
exponent equ   mantissa+8               biased exponent and sign bit

         lda   1,S                      move return value
         pha
         lda   4,S
         sta   2,S
         tsc                            set up DP
         phd
         tcd

         lda   mantissa+2               move 64-bit value to mantissa
         sta   mantissa
         lda   mantissa+4
         sta   mantissa+2
         lda   mantissa+6
         sta   mantissa+4
         lda   mantissa+8
         sta   mantissa+6

         ora   mantissa                 if value is 0 then
         ora   mantissa+2
         ora   mantissa+4
         beq   ret                        return

         ldy   #0                       default sign bit is 0 (positive)
         lda   mantissa+6               if mantissa is negative then
         bpl   lb0
         negate8 mantissa                 negate it
         ldy   #$8000                     sign bit is 1 (negative)

lb0      tya                            set sign
         ora   #63+16383                set initial exponent (2^63)
         sta   exponent

         lda   mantissa+6               if number is normalized (i=1) then
         bmi   ret                        return

lp1      dec   exponent                 normalize number
         asl   mantissa
         rol   mantissa+2
         rol   mantissa+4
         rol   mantissa+6
         bpl   lp1

ret      pld
         rtl
         end

****************************************************************
*
*  ~CnvRealLongLong - convert an extended SANE real into
*        a long long integer
*
*  Inputs:
*        extended real on stack
*
*  Outputs:
*        signed long long int on stack
*
*  Note: This avoids calling FX2C on negative numbers,
*  because it is buggy for certain values.
*
****************************************************************
*
~CnvRealLongLong start
         tsc
         clc
         adc   #4
         pea   0                        push src address for fcpxx
         pha
         pea   llmin|-16                push dst address for fcpxx
         pea   llmin
         pea   0                        push operand address for ftintx
         pha
         ftintx                         round
         fcpxx                          compare with LLONG_MIN
         bne   convert
         
         lda   #$8000                   if it is LONG_MIN, use that value
         sta   12,s
         asl   a
         sta   10,s
         sta   8,s
         sta   6,s
         bra   done                     otherwise
         
convert  lda   4+8,s
         pha                              save original sign
         asl   a                          force sign to positive
         lsr   a
         sta   6+8,s
         tsc
         clc
         adc   #6
         pea   0                          push src address for fx2c
         pha
         pea   0                          push dst address for fx2c
         inc   a
         inc   a
         pha
         fx2c                             convert
         pla                              if original value was negative
         bpl   done
         sec
         ldx   #0                           negate result
         txa
         sbc   6,s
         sta   6,s
         txa
         sbc   6+2,s
         sta   6+2,s
         txa
         sbc   6+4,s
         sta   6+4,s
         txa
         sbc   6+6,s
         sta   6+6,s

done     phb                            move return address
         pla
         plx
         ply
         phx
         pha
         plb
         rtl

llmin    dc    e'-9223372036854775808'
         end

****************************************************************
*
*  ~CnvRealULongLong - convert an extended SANE real into
*        an unsigned long long integer
*
*  Inputs:
*        extended real on stack
*
*  Outputs:
*        unsigned long long int on stack
*
****************************************************************
*
~CnvRealULongLong start
         pea   0                        initially assume val <= LLONG_MAX

         tsc
         clc
         adc   #6
         pea   0                        push src address for fcpxx
         pha
         pea   llbig|-16                push dst address for fcpxx
         pea   llbig
         pea   0                        push operand address for ftintx
         pha
         ftintx                         round
         fcpxx                          compare with LLONG_MAX+1
         bmi   convert
         
         lda   #1                       if val > LLONG_MAX:
         sta   1,S                        save flag to indicate this
         tsc
         clc
         adc   #6
         pea   llbig|-16                  push src address for fsubx
         pea   llbig
         pea   0                          push dst address for fsubx
         pha
         fsubx                            val -= LLONG_MAX+1
         
convert  tsc
         clc
         adc   #6
         pea   0                        push src address for fx2c
         pha
         pea   0                        push dst address for fx2c
         inc   a
         inc   a
         pha
         fx2c                           convert val as comp
         
         pla                            if orig val was > LLONG_MAX:
         beq   done
         lda   12,s
         eor   #$8000
         sta   12,s                       result += LLONG_MAX+1
         
done     phb                            move return address
         pla
         plx
         ply
         phx
         pha
         plb
         rtl

llbig    dc    e'9223372036854775808'
         end
