

      subroutine head() 
       double precision, dimension(2) :: x 
 
       call bar(x(i)) 
      end subroutine 
 
      subroutine bar(a) 
       double precision, dimension(2) :: a 
       double precision b 
 
        a(1) = b 
       return 
       end 
