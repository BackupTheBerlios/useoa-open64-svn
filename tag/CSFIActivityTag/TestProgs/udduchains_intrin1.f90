!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program to test udduchains when there are intrinsic func calls
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine head(x1,x2,y1,y2)
  double precision t1,t2,x1,x2,y1,y2

    t1=x1*x2
    t2=x1*sin(t1)
    y1=cos(t2)
    y2=t2*x2

end subroutine

  
program udduchains_intrin1
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

