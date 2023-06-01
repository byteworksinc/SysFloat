         keep  obj/time
         mcopy time.macros
         case  on

****************************************************************
*
*  Time - Time and date libraries for C
*
*  This code implements the tables and subroutines needed to
*  support the standard C library TIME.
*
*  This porition of the library contains subroutines that are
*  dependent on the way floating-point calculations are
*  performed.  The main portion of the library is in ORCACLib.
*
*  January 1989
*  Mike Westerfield
*
*  Copyright 1989
*  Byte Works, Inc.
*
****************************************************************
*
Time     start                          dummy segment
         end

****************************************************************
*
*  difftime - return the difference between two times as a real
*
*  Inputs:
*        t1, t2 - two times
*
*  Outputs:
*        returns the difference as a real
*
****************************************************************
*
difftime start

         csubroutine (4:t1,4:t2),0
         phb
         phk
         plb

         sec                            get the difference
         lda   t1
         sbc   t2
         sta   itime
         lda   t1+2
         sbc   t2+2
         sta   itime+2
         lda   #0
         bcs   lb1
         dec   a
lb1      sta   itime+4
         sta   itime+6

         ph4   #itime                   convert to real
         ph4   #xtime
         fc2x

         plb
         creturn 10:xtime

itime    ds    8
xtime    ds    10
         end
