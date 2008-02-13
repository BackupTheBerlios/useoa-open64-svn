

      subroutine head() 
       double precision, dimension(10) :: x 
 
       call bar(x(2:4)) 
      end subroutine 
 
      subroutine bar(a) 
       double precision, dimension(2) :: a 
       double precision b 
 
        a(1) = b 
       return 
       end 
