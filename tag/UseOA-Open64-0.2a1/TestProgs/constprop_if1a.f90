!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program to const reaching definitions when there is an if stmt
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
program constprop_if1a
!  double precision :: x, y, z
! for ConstProp, only ConstValBasicInterface that is defined
! and useful is Open64IntegerConstVal
! so am making x, y, and z into integers
!
  integer :: x, y, z

  x = 2
  if ( x .ge. 0 ) then
    y = 5
    x = y + y
  else
    y = 5
  endif
  z = y

end program

