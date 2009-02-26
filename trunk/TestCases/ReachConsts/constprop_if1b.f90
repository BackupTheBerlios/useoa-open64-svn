!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program to const reaching definitions when there is an if stmt
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
program constprop_if1b
!  double precision :: x, y, z
! for ConstProp, only ConstValBasicInterface that is defined
! and useful is Open64IntegerConstVal
! so am making x, y, and z into integers
!
  integer :: x, y, z

  ! All BOTTOM
  x = 2

  ! All BOTTOM, x=2
  if ( x .ge. 0 ) then

    ! All BOTTOM, x=2
    y = 5

    ! All BOTTOM, x=2,y=5
    x = y + y
  else

    ! All BOTTOM, x=2
    y = 3

    ! All BOTTOM, x=2, y=3
    x = 10
  endif

  ! All BOTTOM, x=10
  z = y

  ! All BOTTOM, x=10

end program

