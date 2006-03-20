!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program to test reaching constants when there is a loop and 
! if stmt
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
program constprop_loop
  integer :: x, y, z
  integer :: i

  y = 5

  do i = 1, 10
    x = 2
    if ( x .ge. 0. ) then
      y = 5
      x = y + y
    else
      y = 5
    endif
    z = y
  end do

end program

