!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! most precise results (not here yet)
! p should be constant before the call, and not after the call
! v should be constant after the call, but not before the call
! m should be constant before and after the call
!
! still at call-return interference ...
!
! Note: Even if we use ICFGDFSolver, the analysis is still 
!       Intraprocedural.
!
! Issue: We are getting spurious Entry nodes for procedure bar
!        for which IN set is BOTTOM, which affects analysis
!        Talk to Michelle. May 19th 2008.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



       subroutine head(x, f)
       integer :: x
       integer :: f
       integer m, p, v


       ! all BOTTOM
       m = 2

       ! all BOTTOM, m=2
       p = 5
       
       ! all BOTTOM, m=2, p=5
       call bar(m,p,v,x)

       ! most precise: all BOTTOM, m=2, v=10
       ! Current     : all BOTTOM
       f = m + p + v + x

       ! most precise: all BOTTOM, m=2, v=10
       ! current     : all BOTTOM
       end subroutine


       subroutine bar(a,b,c,d)
       integer a,b,c,d

       ! all BOTTOM, *a=2, *b=5
       c = a * b

       ! all BOTTOM, *a=2, *b=5, *c=10
       b = c - d

       ! all BOTTOM, *a=2, *c=10
       return

       ! all BOTTOM, *a=2, *c=10
       end subroutine

