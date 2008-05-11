!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program to test reaching constants when there is a loop and 
! if stmt
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
program constprop_loop
  integer :: x, y, z
  integer :: i

  !Reaching Consts x=TOP, y=TOP, z=TOP, i=TOP
  y = 5

  do i = 1, 10
    ! i=1     Reaching Consts x=TOP,    y=5, z=TOP, i=TOP
    ! i<=10   Reaching Consts x=TOP,    y=5, z=5,   i=BOTTOM
    ! i=i+1   Reaching Consts x=BOTTOM, y=5, z=5,   i=BOTTOM

    !Reaching Consts y=5, z=5, i=BOTTOM
    x = 2    

    !Reaching Consts x=2, y=5 z=5
    if ( x .ge. 0 ) then

      !Reaching Consts x=2, y=5, z=5  
      y = 5

      !Reaching Consts x=2, y=5, z=5
      x = y + y
    else

      !Reaching Consts x=2, y=5, z=5  
      y = 5
    endif

    !Reaching Consts x=BOTTOM, y=5, z=5
    z = y
  end do

    !Reaching Consts x=BOTTOM, y=5, z=5
end program

