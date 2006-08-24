! PLM 07/26/06  Reference http://www.kcl.ac.uk/kis/support/cit/fortran/f2003_book_examples/ch1104.f90



program ch1104
implicit none
integer , parameter :: nrow=5
integer , parameter :: ncol=6
REAL , DIMENSION(1:nrow,1:ncol) &
                   :: Exam_Results    = 0.0
real , dimension(1:nrow) &
                   :: People_average  = 0.0
real , dimension(1:ncol) &
                   :: Subject_Average = 0.0
integer :: r,c

  !Reaching constant: nrow = 5, r = TOP
  do r=1,nrow

    !Reaching Constants: r = BOTTOM , ncol = 6, exam_results(*,*) = 0.0
    read *, exam_results(r,1:ncol)
  end do

  !Reaching Constants: nrow = 5, Exam_Results(*,*) = TOP 
  Exam_Results(1:nrow,3) = 2.5 * Exam_Results(1:nrow,3)

  !Reaching Constant: nrow = 5, r = TOP
  do r=1,nrow
   
    !reaching Constant: ncol = 6, c = TOP
    do c=1,ncol

      !Reaching Constant: people_average(*) = BOTTOM, r = BOTTOM, c = BOTTOM, exam_results(*,*) = TOP
      people_average(r) = people_average(r) + &
                       exam_results(r,c)
    end do
  end do

  !Reaching Constants: ncol = 6
  people_average = people_average / ncol
 
  !Reaching Constant: ncol = 6, c = BOTTOM
  do c=1,ncol

    !Reaching Constants: nrow = 5, r = BOTTOM 
    do r=1,nrow

      !Reaching Constant: subject_average = 0.0, c = BOTTOM, c = BOTTOM
      subject_average(c) = subject_average(c) + &
                        exam_results(r,c)
    end do
  end do

  !Reaching Constant nrow = 5
  subject_average = subject_average / nrow
  print *,' People averages'
  print *, people_average
  print *, ' Subject averages'
  print *, subject_average
end program ch1104
