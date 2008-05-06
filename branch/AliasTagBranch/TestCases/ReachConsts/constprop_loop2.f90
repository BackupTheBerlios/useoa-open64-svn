!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program to test reaching constants when there is a loop and 
! if stmt
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
program constprop_loop2
  integer :: x, y, z
  integer :: i

  !Reaching Consts x=TOP, y=TOP, z=TOP, i=TOP
  y = 5
  
  do i = 1, 10

    !Reaching Consts x=BOTTOM, y=BOTTOM, z=BOTTOM, i=BOTTOM
    x = 2

    !Reaching Consts x=2, y=BOTTOM, z=BOTTOM, i=BOTTOM
    if ( x .ge. 0 ) then

      !Reaching Consts x=2, y=BOTTOM, z=BOTTOM, i=BOTTOM
      y = 5

      !Reaching Consts x=2, y=5, z=BOTTOM, i=BOTTOM
      x = y + y
    else

      !Reaching Consts x=2, y=BOTTOM, z=BOTTOM, i=BOTTOM  
      y = 3

      x=5
    endif

    !Reaching Consts x=BOTTOM, y=BOTTOM, z=BOTTOM, i=BOTTOM
    z = y
  end do

  !Reaching Consts x=BOTTOM, y=BOTTOM, y=BOTTOM, i=BOTTOM 
end program

