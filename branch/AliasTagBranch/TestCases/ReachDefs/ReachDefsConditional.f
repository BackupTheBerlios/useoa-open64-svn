! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   ReachDefs
!                   ==========
! Features:
!           1. ReachDefs in the presence of conditional
!              - Definition occurs along both the conditional path (for y)
!              - Definition occurs along single conditional path (for x)
!
! Testing :
!          [X] ReachDefs[Stmt]
!
! Status /Issues:
!
! Note:  1. Intraprocedural ReachDefs as of today April 9th 2008.
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!Example:
!========  
       program constprop_if1
          integer :: x, y, z

          x = 2                 
          if ( x .ge. 0 ) then   ! (x=2)
               y = 5             ! (x=2)
               x = y + y         ! (x=2),(y=5)
          else                  
               y = 3             ! (x=2)
          endif 
          z = x+y                ! (x=2), (y=5), (x=y+y), (y=3)


! Exit ReachDefs : (x=2), (y=5), (x=y+y), (y=3), (z=x+y)

       end program

