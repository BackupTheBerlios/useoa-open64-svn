!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program to test reaching definitions when there is a loop and 
! if stmt
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
program reachdefs_if
  double precision :: x, y, z
  integer :: i

  do i = 1, 10
    x = 2
    if ( x .ge. 0. ) then
      y = 5
      x = y + y
      y = x * 5
    else
      y = 3
    endif
    z = y
  end do

end program

