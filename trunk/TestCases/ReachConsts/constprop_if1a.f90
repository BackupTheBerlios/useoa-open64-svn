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

  ! All BOTTOM
  x = 2


  ! All BOTTOM, x=2
  if ( x .ge. 0 ) then
      

    ! All BOTTOM, x=2
    y = 5
    
    ! All BOTTOM, x=2, y=5
    x = y + y
  else
      
    ! All BOTTOM, x=2
    y = 5
  endif

  ! All BOTTOM, y=5
  z = y


  ! All BOTTOM, y=5, z=5

end program

