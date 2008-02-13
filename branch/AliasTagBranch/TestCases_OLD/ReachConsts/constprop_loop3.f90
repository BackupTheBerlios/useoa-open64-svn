!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program to test reaching constants when there is a loop and 
! if stmt
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
program constprop_loop3
  integer :: x, y, z
  integer :: i

  !Set all proc def locations initially to bottom as OUT set of Entry node
  !Set all proc def locations initially to top at OUT set of all other nodes
  !This facilitates constants defined above a loop reaching into a loop
  !  when they are not re-defined within the loop to a different value, as
  !  well as correctly handling branches outside of the loop.

  !Reaching Consts x=BOTTOM, y=BOTTOM, z=BOTTOM, i=BOTTOM
  x = 2

  !Reaching Consts x=2, y=BOTTOM, z=BOTTOM, i=BOTTOM
  !i = 1    ! compiler supplied

  do i = 1, 10

    !Reaching Consts x=2, y=BOTTOM, z=BOTTOM, i=BOTTOM
    y = 3

    !Reaching Consts x=2, y=3, z=BOTTOM
    if ( x .ge. 0. ) then

      !Reaching Consts x=2, y=3, z=BOTTOM  
      y = 5

      !Reaching Consts x=2, y=5, z=BOTTOM
      z = y + y
    else

      !Reaching Consts x=2, y=3, z=BOTTOM  
      y = 5
    endif

    !Reaching Consts x=2, y=5, z=BOTTOM
    z = y

    !Reaching Consts x=2, y=5, z=5
    !i = i + 1    ! compiler supplied
  end do

    !Reaching Consts x=2, y=BOTTOM, z=BOTTOM
end program

