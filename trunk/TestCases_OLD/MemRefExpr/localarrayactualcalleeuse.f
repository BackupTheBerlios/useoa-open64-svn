

      subroutine head() 
       double precision, dimension(2) :: x 
 
       call bar(x) 
      end subroutine 
 
      subroutine bar(a) 
       double precision, dimension(2) :: a 
       double precision b 
 
       b = a(1) 
       return 
       end 
