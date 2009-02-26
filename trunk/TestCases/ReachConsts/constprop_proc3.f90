!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! constprop_proc3.f90
!
! Due to context insensativity across calls, no constants get through 
! either call
!
! current == FIAliasAliasTag, which is CI and FI
! 
!
! Note: Current ReachConsts results (via ICFGDFSolver) are intraprocedural
!
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
       call bar(m,p,v,4)

       !                 most precise: all BOTTOM, m=2, p=6, v=10, temp=4
       ! context insensativity causes: all BOTTOM, temp=4
       !                 Current     : all BOTTOM
       p = 2

       !                 most precise: all BOTTOM, m=2, p=2, v=10, temp=4
       ! context insensativity causes: all BOTTOM, p=2, temp=4
       !                 Current     : all BOTTOM, p=2
       m = 5

       !                 most precise: all BOTTOM, m=5, p=2, v=10, temp=4
       ! context insensativity causes: all BOTTOM, m=5, p=2, temp=4
       !                 Current     : all BOTTOM, p=2, m=5
       call bar(p,m,v,4)


       !                    most precise: all BOTTOM, m=6, p=2, v=10, temp=4
       ! call-return interference causes: all BOTTOM, temp=4
       !                 Current        : all BOTTOM
       f = m + p + v + x

       !                    most precise: all BOTTOM, m=6, p=2, v=10, temp=4
       !    context insensativity causes: all BOTTOM, temp=4
       !                 Current        : all BOTTOM
       end subroutine


       subroutine bar(a,b,c,d)
       integer a,b,c,d
       
       ! in context: call#1: most precise: all BOTTOM, *a=2, *b=5, *d=4
       ! in context: call#2: most precise: all BOTTOM, *a=2, *b=5, *d=4, *c=10
       !    current:                       all BOTTOM,
       c = a * b

       ! in context: call#1: most precise: all BOTTOM, *a=2, *b=5, *d=4, *c=10
       ! in context: call#2: most precise: all BOTTOM, *a=2, *b=5, *d=4, *c=10
       !    current:                       all BOTTOM
       b = c - d

       ! in context: call#1: most precise: all BOTTOM, *a=2, *b=6, *d=4, *c=10
       ! in context: call#2: most precise: all BOTTOM, *a=2, *b=6, *d=4, *c=10
       !    current:                       all BOTTOM,
       return

       ! in context: call#1: most precise: all BOTTOM, *a=2, *b=6, *d=4, *c=10
       ! in context: call#2: most precise: all BOTTOM, *a=2, *b=6, *d=4, *c=10
       !    current:                       all BOTTOM,

       end subroutine

