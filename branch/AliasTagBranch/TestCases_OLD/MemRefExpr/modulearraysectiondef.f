

      module globals 
 
        double precision, dimension(10) :: x 
 
      end module 
 
      subroutine head() 
       use globals 
 
       double precision t2 
 
       x(2:4) = t2 
      end subroutine 
