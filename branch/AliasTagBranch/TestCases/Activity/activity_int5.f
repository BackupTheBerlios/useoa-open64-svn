! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   ICFGActivity
!                   ============
! Features:
!           1. Independent and Dependent variables are Array Index Exprs
!           2. Conditional Statements in flow sensitive analysis.
!
! Testing :
!          [X] Virtual LocTuple range.
!          [X] Partial Flags
!
! Status : No Issues
!
! Note: To Date April 8th 2008, all memory references are "May".
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


! Example:
! ========

       subroutine head()
         double precision x(2)
         double precision y(2)
         integer i
c$openad INDEPENDENT(x)
c                         [u: y,x] [v: x]         [iA: x]
         i=1
c                         [u: y,x] [v: x]         [iA: x]
         do while (i<3)
c                         [u: y,x] [v: x]         [iA: x]
           if (i<2) then
c                         [u: y,x] [v: x]         [iA: x]
             y(2)=x(1)
c                         [u: y]   [v: x,y]       [iA: y]
           else
c                         [u: y,x] [v: x]         [iA: x]
             y(1)=x(2)
c                         [u: y]   [v: x,y]       [iA: y]
           end if 
c                         [u: y]   [v: x,y]       [iA: y]
           i=i+1
c                         [u: y]   [v: x,y]       [iA: y]
         end do
c                         [u: y]   [v: x,y]       [iA: y]
         y(2)=y(1)*y(2)
c                         [u: y]   [v: x,y]       [iA: y]
c$openad DEPENDENT(y)
       end subroutine



! ICFGActivity:     ActiveMemRef(head) = (x,y)
! ============
