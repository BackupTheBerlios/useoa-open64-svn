! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   ReachDefs
!                   ==========
! Features:
!           1. ReachDefs inside Loop and Conditionals
!              Definition occurs along both the conditional path (for y)
!              Definition occurs along single conditional path (for x)
!
! Testing :
!          [X] ReachDefs[Stmt]
!
! Status /Issues: 
!
! Note:  1. Intraprocedural ReachDefs Analysis 
!        2. StmtHandle(0) indicate may-ReachDefs. Except strictlyLocal,
!           all other ReachDefs are May and therefore, we get 
!           StmtHandle(0) for them.
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


! Example:
! --------

       program constprop_loop
       integer :: x, y, z
       integer :: i

       y = 2                       
       do i = 1, 10               ! (y=2)
                                  ! (i=1), (x=2), (x=y+y),
                                  ! (y=5), (y=3), (z=x+y),
                                  ! (i=i+1)

          x = 2                   ! (i=1), (x=2), (x=y+y),
                                  ! (y=5), (y=3), (z=x+y),
                                  ! (i=i+1), (y=2)

          if ( x .ge. 0 ) then    ! (y=2), (i=1), (x=2),
                                  ! (y=5), (y=3), (z=x+y),
                                  ! (i=i+1)

               y = 5              ! (y=2), (i=1), (x=2),
                                  ! (y=5), (y=3), (z=x+y),
                                  ! (i=i+1)

               x = y + y          ! (i=1), (x=2), (y=5)
                                  ! (z=x+y), (i=i+1)
          else 
               y = 3              ! (y=2), (i=1), (x=2)
                                  ! (y=5), (y=3), (z=x+y),
                                  ! (i=i+1)
          endif 
          z = x+y                 ! (i=1), (x=2), (x=y+y),
                                  ! (y=5), (y=3), (i=i+1)
                                  ! (z=x+y)
                                  
       end do

! Exit ReachDefs : (i=1), (x=2), (x=y+y), (y=5), (y=3) (i=i+1)
!                  (z=x+y)

       end program






