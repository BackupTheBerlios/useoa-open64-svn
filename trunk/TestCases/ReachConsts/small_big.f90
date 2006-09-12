! PLM 07/26/06  Reference http://www.kcl.ac.uk/kis/support/cit/fortran/f2003_book_examples/ch1204.f90


program ch1204
implicit none
integer :: i
real    :: small = 1.0
real    :: big   = 1.0
  do i=1,50

    !Reachin constant i=BOTTOM, smal=1.0, big=1.0
    print 100,i,small,big
    
    100 format(' ',i3,' ',f7.3,' ',f7.3)
    !Reaching Constant small = 1.0
    small=small/10.0

    !Reaching constant big = 1.0
    big=big*10.0
  end do
end program ch1204
