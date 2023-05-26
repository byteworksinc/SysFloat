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

         sub4  t1,t2,itime              get the difference
         ph4   #itime                   convert to real
         ph4   #xtime
         fl2x
         lla   t1,xtime

         plb
         creturn 4:t1

itime    ds    4
xtime    ds    10
         end
