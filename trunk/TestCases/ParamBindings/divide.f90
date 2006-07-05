program division
implicit none
real :: a,b,c
  a = 1000.0
  b =   20.0
  call divide(a,b,c)
  print *,c
end program division

subroutine divide(a,b,c)
implicit none
integer , intent(in) :: a
integer , intent(in) :: b
integer , intent(out):: c
  c=a/b
end subroutine divide
