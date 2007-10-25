

      subroutine head(A,B) 
 
      INTEGER, DIMENSION(3) :: VECTOR= (/ 1,3,2 /), A, B 
      integer i 
 
        B = A(VECTOR(i))         ! A(1) = B(1), A(3) = B(2), A(2) = B(3) 
        B( (/ 3,2,1 /) ) = A     ! A(1) = B(3), A(2) = B(2), A(3) = B(1) 
 
      end subroutine 

