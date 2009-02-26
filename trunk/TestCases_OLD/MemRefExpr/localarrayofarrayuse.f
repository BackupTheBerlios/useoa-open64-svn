
      subroutine head() 
 
      INTEGER, DIMENSION(3) :: VECTOR= (/ 1,3,2 /), A, B 
        B = A(VECTOR)            ! A(1) = B(1), A(3) = B(2), A(2) = B(3) 
        A = B( (/ 3,2,1 /) )     ! A(1) = B(3), A(2) = B(2), A(3) = B(1) 
 
      end subroutine 
