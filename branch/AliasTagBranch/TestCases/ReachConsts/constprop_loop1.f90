!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program to test reaching constants when there is a loop and 
! if stmt
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
program constprop_loop
  integer :: x, y, z
  integer :: i


  ! All BOTTOM
  y = 5


  ! All BOTTOM, y=5
  do i = 1, 10

    ! All BOTTOM, y=5
    x = 2    

    ! All BOTTOM, y=5, x=2
    if ( x .ge. 0 ) then

      ! All BOTTOM, y=5, x=2
      y = 5

      ! All BOTTOM, y=5, x=2
      x = y + y
    else

      ! All BOTTOM, y=5, x=2
      y = 5
    endif

    ! All BOTTOM, y=5
    z = y
  end do

    ! All BOTTOM, y=5

end program

