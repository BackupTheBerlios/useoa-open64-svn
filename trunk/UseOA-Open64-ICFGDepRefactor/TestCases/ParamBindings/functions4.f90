!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program with a recursive function
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

recursive function testf(in,k) result(out)
  integer , dimension(6) :: in, out
  integer k
  if (k .gt. 0) then
     out=testf(in,k-1)
     out(size(in)-k+1)=in(k)
     out(k)=in(size(in)-k+1)
  else
     out=in
  end if
end function 

program recfunc
interface 
recursive function testf(in,k) result(out)
  integer , dimension(6) :: in, out
  integer k
end function
end interface
  integer, dimension(6) :: in=(/1,2,3,4,5,6/)
  
  print *, in

  in = testf(in,3)
  
  print *, in	
end program recfunc
