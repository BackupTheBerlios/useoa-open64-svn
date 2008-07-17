!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! constprop_proc2.f90
!
! Due to context insensativity across calls, no constants get through 
! either call
!
! current == FIAliasAliasTag
!
! Due to context insensitive alias analysis, we see the following aliasTagSets:
! (M, deref-A, deref-B)  and (P, deref-A, deref-B) and (V, deref-C).  Also,
! since A, B, and C are reference parameters, they do not have their own 
! storage locations, so assignments to them within bar cannot define reaching
! constants.
!
! Therefore, currently, output is all BOTTOM, no constants get thru.
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
       call bar(m,p,v,x)

       !                    most precise: all BOTTOM, m=2, v=10
       !    context insensativity causes: all BOTTOM
       m = 5

       !                    most precise: all BOTTOM, m=5, v=10
       !    context insensativity causes: all BOTTOM, m=5
       p = 2

       !                    most precise: all BOTTOM, m=5, p=2, v=10
       !    context insensativity causes: all BOTTOM, m=5, p=2
       call bar(p,m,v,x)


       !                    most precise: all BOTTOM, p=2,  v=10
       !    context insensativity causes: all BOTTOM
       f = m + p + v + x

       !                    most precise: all BOTTOM, p=2,  v=10
       !    context insensativity causes: all BOTTOM
       end subroutine


       subroutine bar(a,b,c,d)
       integer a,b,c,d
       
       ! in context: call#1:               all BOTTOM, a=2, b=5
       ! in context: call#2: most precise: all BOTTOM, a=2, b=5, c=10
       ! no context:                       all BOTTOM
       c = a * b

       ! in context: call#1:               all BOTTOM, a=2, b=5,  c=10
       ! in context: call#2: most precise: all BOTTOM, a=2, b=5,  c=10
       ! no context:                       all BOTTOM
       b = c - d

       ! in context: call#1:               all BOTTOM, a=2, c=10
       ! in context: call#2: most precise: all BOTTOM, a=2, c=10
       ! no context:                       all BOTTOM
       return

       ! in context: call#1:               all BOTTOM, a=2, c=10
       ! in context: call#2: most precise: all BOTTOM, a=2, c=10
       ! no context:                       all BOTTOM
       end subroutine

