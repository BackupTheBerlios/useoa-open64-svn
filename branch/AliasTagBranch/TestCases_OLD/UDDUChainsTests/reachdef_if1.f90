!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program to test reaching definitions when there is an if stmt
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
program reachdefs_if
  double precision :: x, y, z

  x = 2
  if ( x .ge. 0. ) then
    y = 5
    x = y + y
  else
    y = 3
  endif
  z = y

end program

